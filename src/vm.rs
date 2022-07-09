#[cfg(feature = "rlox_debug")]
use crate::debug::disassemble_instruction;
use std::mem::MaybeUninit;

use crate::object::{Obj, ObjFunction, ObjString};
use crate::value::{as_bool, as_number, print_value, Value};
use crate::{Compiler, OpCode};

use crate::chunk::{IndexConverter, InstructionIndex};
use crate::compiler::FunctionHandle;
use crate::scanner::Scanner;
use crate::stack::UnsafeStack;
use crate::table::Table;

/// The result of compiling and possibly executing a piece of code.
#[cfg_attr(feature = "rlox_debug", derive(Debug))]
#[derive(PartialEq)]
pub enum InterpretResult {
    /// The code was compiled and executed successfully.
    Ok,

    /// An error occurred while compiling the code, and therefore it was not executed.
    CompileError,

    /// After successful compilation, an error occurred while executing the compiled code.
    RuntimeError,
}

/// A frame or window for executing a function. It stores the absolute start index of the
/// functions' locals within the VM stack. At compile time, only a relative index is known.
#[cfg_attr(feature = "rlox_debug", derive(Debug))]
pub struct CallFrame<const SIZE: usize> {
    /// The function being executed. This is mainly used for
    /// retrieving the functions' constants.
    function: *mut ObjFunction,

    /// The instruction pointer of this function. This is used to jump back
    /// to and resume execution when returning from a callee.
    ip: *mut u8,

    /// A reference to the VM value stack, for storing the functions' locals.
    slots: *mut [Value],
}

impl<const SIZE: usize> CallFrame<SIZE> {
    pub fn new(function: &mut ObjFunction, slots: &mut [Value]) -> Self {
        Self {
            function,
            ip: function.chunk.code.as_mut_ptr(),
            slots,
        }
    }

    pub unsafe fn increment_ip(&mut self, count: usize) {
        self.ip = self.ip.add(count);
    }

    pub unsafe fn decrement_ip(&mut self, count: usize) {
        self.ip = self.ip.sub(count);
    }

    pub unsafe fn get_function(&self) -> &ObjFunction {
        self.function.as_ref().unwrap_unchecked()
    }

    pub unsafe fn get_function_mut(&mut self) -> &mut ObjFunction {
        self.function.as_mut().unwrap_unchecked()
    }

    pub unsafe fn get_slot_at_unchecked(&mut self, index: InstructionIndex) -> &Value {
        &self.slots.as_ref().unwrap_unchecked()[index]
    }

    pub unsafe fn set_slot_at(&mut self, index: InstructionIndex, value: Value) {
        self.slots.as_mut().unwrap_unchecked()[index] = value;
    }
}

/// The maximum call stack size.
const FRAMES_MAX: usize = 64;

/// The maximum value stack size.
const STACK_MAX: usize = FRAMES_MAX * u8::MAX as usize;

#[cfg_attr(feature = "rlox_debug", derive(Debug))]
pub struct VM {
    frames: UnsafeStack<CallFrame<STACK_MAX>, FRAMES_MAX>,
    current_frame: MaybeUninit<*mut CallFrame<STACK_MAX>>,
    stack: UnsafeStack<Value, STACK_MAX>,
    globals: Table<ObjString, Value>,
    strings: Table<ObjString, Value>,
}

impl VM {
    pub fn new(strings: Table<ObjString, Value>) -> Self {
        Self {
            frames: UnsafeStack::new(),
            current_frame: MaybeUninit::uninit(),
            stack: UnsafeStack::new(),
            globals: Table::new(),
            strings,
        }
    }

    pub fn interpret(source: String) -> InterpretResult {
        let mut compiler = Compiler::new(Scanner::new(&source), FunctionHandle::default());

        match compiler.compile() {
            None => InterpretResult::CompileError,
            Some((function, string_cache)) => {
                let mut vm = VM::new(string_cache);
                vm.stack.push(Value::from_obj(Obj::Function(function)));

                vm.call(function, 0);
                unsafe { vm.run() }
            }
        }
    }

    unsafe fn run(&mut self) -> InterpretResult {
        loop {
            self.current_frame =
                MaybeUninit::new(self.frames.get_at_unchecked_mut(self.frames.len() - 1));
            if cfg!(feature = "rlox_debug") {
                // Disassemble current instruction
                let offset = VM::get_offset(self.get_current_frame_mut());
                disassemble_instruction(&self.get_current_frame_mut().get_function().chunk, offset);
            }

            let opcode = OpCode::try_from(self.read_byte());
            if opcode.is_err() {
                println!("{}", opcode.unwrap_err());
                return InterpretResult::CompileError;
            }

            match opcode.unwrap() {
                OpCode::Add => {
                    if self.type_check_two_operands(Value::is_string) {
                        self.concatenate();
                    } else if self
                        .validate_two_operands(Value::is_number, "Operands must be numbers.")
                    {
                        self.op_add();
                    } else {
                        self.runtime_error("Operands must be two numbers or two strings.");
                        return InterpretResult::RuntimeError;
                    }
                }
                OpCode::Call => {
                    let argc = self.read_byte();
                    if !self.call_value(argc as usize, argc) {
                        return InterpretResult::RuntimeError;
                    }
                    self.current_frame =
                        MaybeUninit::new(self.frames.get_at_unchecked_mut(self.frames.len() - 1));
                }
                OpCode::Constant => {
                    let index = self.read_byte();
                    let value = self
                        .get_current_frame_mut()
                        .get_function()
                        .chunk
                        .read_constant(index as usize);
                    self.stack.push(value);
                }
                OpCode::ConstantLong => {
                    let index = self.read_long_index();
                    let value = self
                        .get_current_frame_mut()
                        .get_function()
                        .chunk
                        .read_constant(index);
                    self.stack.push(value);
                }
                OpCode::DefineGlobal => {
                    // Note: the clox implementation pop()'s the value off of the stack
                    // _after_ inserting it on the stack (using peek(0)).
                    // This is to support a triggered garbage collection while interning the string.
                    // In rlox we hope to prevent the need for a GC due to rusts'
                    // memory model characteristics.
                    let value = self.stack.pop().unwrap();
                    let var_name: ObjString = self.read_constant_string(false).clone();

                    self.globals.insert(var_name, value);
                }
                OpCode::DefineGlobalLong => {
                    // Note: the clox implementation pop()'s the value off of the stack
                    // _after_ inserting it on the stack (using peek(0)).
                    // This is to support a triggered garbage collection while interning the string.
                    // In rlox we hope to prevent the need for a GC due to rusts'
                    // memory model characteristics.
                    let value = self.stack.pop().unwrap();
                    let var_name: ObjString = self.read_constant_string(true).clone();

                    self.globals.insert(var_name, value);
                }
                OpCode::Divide => {
                    if self.validate_two_operands(Value::is_number, "Operands must be numbers.") {
                        self.op_divide();
                    } else {
                        return InterpretResult::RuntimeError;
                    }
                }
                OpCode::Equal => {
                    let y = self.stack.pop();
                    let x = self.stack.pop();
                    let result = Value::from_bool(x == y);

                    self.stack.push(result);
                }
                OpCode::False => self.stack.push(Value::from_bool(false)),
                OpCode::GetGlobal => {
                    if !self.op_get_global(false) {
                        return InterpretResult::RuntimeError;
                    }
                }
                OpCode::GetGlobalLong => {
                    if !self.op_get_global(true) {
                        return InterpretResult::RuntimeError;
                    }
                }
                OpCode::GetLocal => {
                    self.op_get_local(false);
                }
                OpCode::GetLocalLong => self.op_get_local(true),
                OpCode::Greater => {
                    if self.validate_two_operands(Value::is_number, "Operands must be numbers.") {
                        self.op_greater();
                    } else {
                        return InterpretResult::RuntimeError;
                    }
                }
                OpCode::Jump => {
                    let offset = self.read_short();
                    self.get_current_frame_mut().increment_ip(offset as usize);
                }
                OpCode::JumpIfFalse => {
                    let offset = self.read_short();
                    if VM::is_falsey(self.stack.peek(0).unwrap()) {
                        self.get_current_frame_mut().increment_ip(offset as usize);
                    }
                }
                OpCode::Less => {
                    if self.validate_two_operands(Value::is_number, "Operands must be numbers.") {
                        self.op_less();
                    } else {
                        return InterpretResult::RuntimeError;
                    }
                }
                OpCode::Loop => {
                    let offset = self.read_short();
                    self.get_current_frame_mut().decrement_ip(offset as usize);
                }
                OpCode::Multiply => {
                    if self.validate_two_operands(Value::is_number, "Operands must be numbers.") {
                        self.op_multiply();
                    } else {
                        return InterpretResult::RuntimeError;
                    }
                }
                OpCode::Negate => match self.stack.peek(0) {
                    Some(elem) => {
                        if !Value::is_number(elem) {
                            self.runtime_error("Operand must be a number.");
                            return InterpretResult::RuntimeError;
                        }
                        self.stack_top_negate_number();
                    }
                    None => (),
                },
                OpCode::Nil => self.stack.push(Value::from_nil()),
                OpCode::Not => match self.stack.pop() {
                    Some(elem) => self.stack.push(Value::from_bool(Self::is_falsey(&elem))),
                    None => (),
                },
                OpCode::Pop => {
                    self.stack.pop().unwrap();
                }
                OpCode::PopN => {
                    let n = self.read_byte();
                    self.stack.popn(n as usize);
                }
                OpCode::Print => match self.stack.pop() {
                    Some(elem) => {
                        print_value(&elem);
                        println!();
                    }
                    None => {
                        self.runtime_error("No operand to print");
                        return InterpretResult::RuntimeError;
                    }
                },
                OpCode::Return => {
                    // Exit rlox interpreter
                    return InterpretResult::Ok;
                }
                OpCode::SetGlobal => {
                    if !self.op_set_global(false) {
                        return InterpretResult::RuntimeError;
                    }
                }
                OpCode::SetGlobalLong => {
                    if !self.op_set_global(true) {
                        return InterpretResult::RuntimeError;
                    }
                }
                OpCode::SetLocal => self.op_set_local(false),
                OpCode::SetLocalLong => self.op_set_local(true),
                OpCode::Subtract => {
                    self.op_subtract();
                }
                OpCode::True => self.stack.push(Value::from_bool(true)),
            };

            if cfg!(feature = "rlox_debug") {
                // Stack tracking
                println!("Stack: {:?}", self.stack)
            }
        }
    }

    unsafe fn get_current_frame_mut(&mut self) -> &mut CallFrame<STACK_MAX> {
        self.current_frame.assume_init().as_mut().unwrap_unchecked()
    }

    fn call(&mut self, function: &mut ObjFunction, arg_count: u8) -> bool {
        if arg_count != function.arity {
            self.runtime_error(&format!(
                "Expect {} arguments but got {}.",
                function.arity, arg_count
            ));
            return false;
        }

        if self.frames.len() == FRAMES_MAX {
            self.runtime_error("Stack overflow.");
            return false;
        }

        let start_idx = self.stack.len() - arg_count as usize - 1;
        let call_frame = CallFrame::new(function, &mut self.stack[start_idx..]);

        self.frames.push(call_frame);
        true
    }

    fn call_value(&mut self, distance: usize, arg_count: u8) -> bool {
        let callee = unsafe { self.stack.peek(distance).unwrap_unchecked() };

        if Value::is_obj(callee) {
            if let Value::Obj(Obj::Function(function)) = callee {
                return self.call(unsafe { function.as_mut().unwrap_unchecked() }, arg_count);
            }
        }

        self.runtime_error("Can only call functions and classes.");
        false
    }

    unsafe fn stack_pop_two_unchecked(&mut self) -> (Value, Value) {
        (
            self.stack.pop().unwrap_unchecked(),
            self.stack.pop().unwrap_unchecked(),
        )
    }

    fn stack_top_negate_number(&mut self) {
        match self.stack.peek(0) {
            Some(elem) => {
                let replacement = Value::from_number(-as_number(elem));
                unsafe { self.stack.replace_top(replacement) };
            }
            None => (),
        }
    }

    fn is_falsey(value: &Value) -> bool {
        Value::is_nil(value) || (Value::is_bool(value) && !as_bool(value))
    }

    fn concatenate(&mut self) {
        let rhs = self.stack.pop();
        let lhs = self.stack.pop();
        if let (Some(y), Some(x)) = (rhs, lhs) {
            let concatenated = ObjString::add_interned(
                unsafe { x.as_rlox_string_ref().unwrap_unchecked() },
                unsafe { y.as_rlox_string_ref().unwrap_unchecked() },
                &mut self.strings,
            );
            self.stack.push(Value::from_obj(Obj::String(concatenated)))
        }
    }

    fn type_check_two_operands(&mut self, validator: fn(&Value) -> bool) -> bool {
        match self.stack.peek(0) {
            Some(rhs) => match self.stack.peek(1) {
                Some(lhs) => {
                    // comment
                    validator(lhs) && validator(rhs)
                }
                None => false,
            },
            None => false,
        }
    }

    fn validate_two_operands(&mut self, validator: fn(&Value) -> bool, message: &str) -> bool {
        if !self.type_check_two_operands(validator) {
            self.runtime_error(message);
            return false;
        }

        true
    }

    fn runtime_error(&mut self, message: &str) {
        eprintln!("{}", message);

        for idx in (0..self.frames.len()).rev() {
            unsafe {
                let call_frame = self.frames.get_at_unchecked_mut(idx);
                let function = call_frame.function.as_ref().unwrap_unchecked();
                let chunk = &call_frame.get_function().chunk;
                let offset = VM::get_offset(call_frame);
                let line = chunk.get_line(offset);
                eprint!("[line {}] in ", line.no);
                if let Some(function_name) = function.name {
                    eprintln!("{}", (*function_name).data);
                } else {
                    eprintln!("script");
                }
            }
        }

        self.stack.reset();
    }

    unsafe fn read_constant_string(&mut self, long_constant: bool) -> &ObjString {
        let index = if long_constant {
            self.read_long_index()
        } else {
            self.read_byte() as InstructionIndex
        };

        self.frames
            .get_at_unchecked_mut(self.frames.len() - 1)
            .get_function_mut()
            .chunk
            .read_constant(index)
            .as_rlox_string_ref()
            .unwrap_unchecked()
    }

    #[inline(always)]
    fn op_add(&mut self) {
        let (y, x) = unsafe { self.stack_pop_two_unchecked() };
        let result = Value::from_number(as_number(&x) + as_number(&y));

        self.stack.push(result);
    }

    #[inline(always)]
    fn op_divide(&mut self) {
        let (y, x) = unsafe { self.stack_pop_two_unchecked() };
        let result = Value::from_number(as_number(&x) / as_number(&y));

        self.stack.push(result);
    }

    #[inline(always)]
    fn op_greater(&mut self) {
        let (y, x) = unsafe { self.stack_pop_two_unchecked() };
        let result = Value::from_bool(as_number(&x) > as_number(&y));

        self.stack.push(result);
    }

    #[inline(always)]
    fn op_less(&mut self) {
        let (y, x) = unsafe { self.stack_pop_two_unchecked() };
        let result = Value::from_bool(as_number(&x) < as_number(&y));

        self.stack.push(result);
    }

    #[inline(always)]
    fn op_multiply(&mut self) {
        let (y, x) = unsafe { self.stack_pop_two_unchecked() };
        let result = Value::from_number(as_number(&x) * as_number(&y));

        self.stack.push(result);
    }

    #[inline(always)]
    fn op_subtract(&mut self) {
        let (y, x) = unsafe { self.stack_pop_two_unchecked() };
        let result = Value::from_number(as_number(&x) - as_number(&y));

        self.stack.push(result);
    }

    fn op_get_local(&mut self, long_local: bool) {
        let idx = unsafe {
            if long_local {
                self.read_long_index()
            } else {
                self.read_byte() as InstructionIndex
            }
        };

        let call_frame = unsafe { self.frames.get_at_unchecked_mut(self.frames.len() - 1) };
        let value = unsafe { call_frame.get_slot_at_unchecked(idx).clone() };
        self.stack.push(value);
    }

    fn op_set_local(&mut self, long_local: bool) {
        let slot = unsafe {
            if long_local {
                self.read_long_index()
            } else {
                self.read_byte() as InstructionIndex
            }
        };

        let value = self.stack.peek(0).unwrap().clone();

        let call_frame = unsafe { self.frames.get_at_unchecked_mut(self.frames.len() - 1) };
        unsafe { call_frame.set_slot_at(slot, value) };
    }

    fn op_get_global(&mut self, long_global: bool) -> bool {
        let var_name = unsafe { self.read_constant_string(long_global).clone() };
        match self.globals.get(&var_name) {
            Some(value) => {
                // TODO can value be cloned here?
                self.stack.push(value.clone());
                true
            }
            None => {
                self.runtime_error(&format!("Undefined variable '{}'.", var_name.data));
                false
            }
        }
    }

    fn op_set_global(&mut self, long_global: bool) -> bool {
        let var_name = unsafe { self.read_constant_string(long_global).clone() };

        match self.globals.get(&var_name) {
            Some(_) => {
                let value = self.stack.peek(0).unwrap().clone();
                self.globals.insert(var_name, value);
                true
            }
            None => {
                self.runtime_error(&format!("Undefined variable '{}'.", &var_name.data));
                false
            }
        }
    }

    #[inline(always)]
    unsafe fn read_byte(&mut self) -> u8 {
        let call_frame = self.frames.get_at_unchecked_mut(self.frames.len() - 1);
        let byte = *call_frame.ip;
        call_frame.increment_ip(1);

        byte
    }

    #[inline(always)]
    unsafe fn read_short(&mut self) -> u16 {
        let call_frame = self.frames.get_at_unchecked_mut(self.frames.len() - 1);
        call_frame.increment_ip(2);

        ((*call_frame.ip.offset(-2) as u16) << 8u16) | *call_frame.ip.offset(-1) as u16
    }

    unsafe fn read_long_index(&mut self) -> InstructionIndex {
        InstructionIndex::from_n_most_significant_le_bytes::<3>([
            self.read_byte(),
            self.read_byte(),
            self.read_byte(),
        ])
    }

    #[inline(always)]
    unsafe fn get_offset(call_frame: &CallFrame<STACK_MAX>) -> InstructionIndex {
        let start = call_frame.get_function().chunk.code.as_ptr() as usize;
        call_frame.ip as usize - start
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn interpret_numeric_expr() {
        let source = String::from("!(5 - 4 > 3 * 2 == !nil);");
        assert_eq!(VM::interpret(source), InterpretResult::Ok);
    }

    #[test]
    fn interpret_string_concatenation() {
        let source = String::from("\"st\" + \"ri\" + \"ng\";");
        assert_eq!(VM::interpret(source), InterpretResult::Ok);
    }

    #[test]
    fn interpret_string() {
        let source = String::from("var x = \"abc\" + \"def\"; print x;");
        assert_eq!(VM::interpret(source), InterpretResult::Ok);
    }

    #[test]
    fn interpret_print_statement() {
        let source = String::from("print 1 + 2;");
        assert_eq!(VM::interpret(source), InterpretResult::Ok);
    }

    #[test]
    fn interpret_global_variable_access() {
        let source = String::from("var beverage = \"cafe au lait\";\nvar breakfast = \"beignets with \" + beverage;\nprint breakfast;");
        assert_eq!(VM::interpret(source), InterpretResult::Ok);
    }

    #[test]
    fn interpret_global_variable_assignment() {
        let source = String::from(
            "var breakfast = \"beignets\";\nvar beverage = \"cafe au lait\";\nbreakfast = \"beignets with \" + beverage;\nprint breakfast;");
        assert_eq!(VM::interpret(source), InterpretResult::Ok);
    }

    #[test]
    fn print_local_variable() {
        let source = String::from("{ var cup; print cup;}");
        assert_eq!(VM::interpret(source), InterpretResult::Ok);
    }

    #[test]
    fn execute_while_loop() {
        let source =
            String::from("var idx = 0; print idx; while (idx < 10) { idx = idx + 1; } print idx;");
        assert_eq!(VM::interpret(source), InterpretResult::Ok);
    }

    #[test]
    fn execute_for_loop_with_var() {
        let source = String::from("for (var x = 0; x < 10; x = x + 1) { print x; }");
        assert_eq!(VM::interpret(source), InterpretResult::Ok);
    }

    #[test]
    fn exec_and_operator() {
        let source =
            String::from("if (true and false) { print \"true\"; } else { print \"false\"; }");
        assert_eq!(VM::interpret(source), InterpretResult::Ok);
    }

    #[test]
    fn exec_or_operator() {
        let source =
            String::from("if (true or false) { print \"true\"; } else { print \"false\"; }");
        assert_eq!(VM::interpret(source), InterpretResult::Ok);
    }

    #[test]
    fn print_stack_trace() {
        let source = String::from(
            "fun a() { b(); }\
        fun b() { c(); }\
        fun c() { \
            c(\"too\", \"many\"); \
            }\
        \
        a();",
        );

        assert_eq!(VM::interpret(source), InterpretResult::RuntimeError);
    }
}
