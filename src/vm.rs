#[cfg(feature = "rlox_debug")]
use crate::debug::disassemble_instruction;

use crate::object::{value_as_rlox_string_ref, Obj, ObjString};
use crate::value::{as_bool, as_number, print_value, Value};
use crate::{Chunk, Compiler, OpCode};

use crate::chunk::{IndexConverter, InstructionIndex};
use crate::scanner::Scanner;
use crate::stack::UnsafeStack;
use crate::table::Table;
use std::ptr;

#[cfg_attr(feature = "rlox_debug", derive(Debug))]
#[derive(PartialEq)]
pub enum InterpretResult {
    Ok,
    CompileError,
    RuntimeError,
}

#[cfg_attr(feature = "rlox_debug", derive(Debug))]
pub struct VM {
    ip: *mut u8,
    stack: UnsafeStack<Value, 256>,
    globals: Table<ObjString, Value>,
    strings: Table<ObjString, Value>,
}

impl VM {
    pub fn new(strings: Table<ObjString, Value>) -> Self {
        Self {
            ip: ptr::null_mut(),
            stack: UnsafeStack::new(),
            globals: Table::new(),
            strings,
        }
    }

    pub fn interpret(source: String) -> InterpretResult {
        let mut chunk = Chunk::new();
        let mut compiler = Compiler::new(Scanner::new(source), &mut chunk);

        if !compiler.compile() {
            InterpretResult::CompileError
        } else {
            let mut vm = VM::new(Table::from(compiler.string_cache()));
            unsafe { vm.run(&mut chunk) }
        }
    }

    pub unsafe fn run(&mut self, chunk: &mut Chunk) -> InterpretResult {
        self.ip = chunk.code.as_mut_ptr();

        loop {
            if cfg!(feature = "rlox_debug") {
                // Disassemble current instruction
                let offset = VM::get_offset(chunk, self.ip);
                disassemble_instruction(chunk, offset);
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
                    } else if self.validate_two_operands(
                        chunk,
                        Value::is_number,
                        "Operands must be numbers.",
                    ) {
                        self.op_add();
                    } else {
                        self.runtime_error(chunk, "Operands must be two numbers or two strings.");
                        return InterpretResult::RuntimeError;
                    }
                }
                OpCode::Constant => {
                    let value = chunk.read_constant(self.read_byte() as usize);
                    self.stack.push(value);
                }
                OpCode::ConstantLong => {
                    let value = chunk.read_constant(self.read_long_index());
                    self.stack.push(value);
                }
                OpCode::DefineGlobal => {
                    // Note: the clox implementation pop()'s the value off of the stack
                    // _after_ inserting it on the stack (using peek(0)).
                    // This is to support a triggered garbage collection while interning the string.
                    // In rlox we hope to prevent the need for a GC due to rusts'
                    // memory model characteristics.
                    let value = self.stack.pop().unwrap();
                    let var_name: ObjString = self.read_constant_string(chunk, false).clone();

                    self.globals.insert(var_name, value);
                }
                OpCode::DefineGlobalLong => {
                    // Note: the clox implementation pop()'s the value off of the stack
                    // _after_ inserting it on the stack (using peek(0)).
                    // This is to support a triggered garbage collection while interning the string.
                    // In rlox we hope to prevent the need for a GC due to rusts'
                    // memory model characteristics.
                    let value = self.stack.pop().unwrap();
                    let var_name: ObjString = self.read_constant_string(chunk, true).clone();

                    self.globals.insert(var_name, value);
                }
                OpCode::Divide => {
                    if self.validate_two_operands(
                        chunk,
                        Value::is_number,
                        "Operands must be numbers.",
                    ) {
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
                    if !self.op_get_global(chunk, false) {
                        return InterpretResult::RuntimeError;
                    }
                }
                OpCode::GetGlobalLong => {
                    if !self.op_get_global(chunk, true) {
                        return InterpretResult::RuntimeError;
                    }
                }
                OpCode::GetLocal => {
                    self.op_get_local(false);
                }
                OpCode::GetLocalLong => self.op_get_local(true),
                OpCode::Greater => {
                    if self.validate_two_operands(
                        chunk,
                        Value::is_number,
                        "Operands must be numbers.",
                    ) {
                        self.op_greater();
                    } else {
                        return InterpretResult::RuntimeError;
                    }
                }
                OpCode::Jump => {
                    let offset = self.read_short();
                    self.ip = self.ip.offset(offset as isize);
                }
                OpCode::JumpIfFalse => {
                    let offset = self.read_short();
                    if VM::is_falsey(self.stack.peek(0).unwrap()) {
                        self.ip = self.ip.offset(offset as isize);
                    }
                }
                OpCode::Less => {
                    if self.validate_two_operands(
                        chunk,
                        Value::is_number,
                        "Operands must be numbers.",
                    ) {
                        self.op_less();
                    } else {
                        return InterpretResult::RuntimeError;
                    }
                }
                OpCode::Loop => {
                    let offset = self.read_short();
                    self.ip = self.ip.sub(offset as usize);
                }
                OpCode::Multiply => {
                    if self.validate_two_operands(
                        chunk,
                        Value::is_number,
                        "Operands must be numbers.",
                    ) {
                        self.op_multiply();
                    } else {
                        return InterpretResult::RuntimeError;
                    }
                }
                OpCode::Negate => match self.stack.peek(0) {
                    Some(elem) => {
                        if !Value::is_number(elem) {
                            self.runtime_error(chunk, "Operand must be a number.");
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
                        self.runtime_error(chunk, "No operand to print");
                        return InterpretResult::RuntimeError;
                    }
                },
                OpCode::Return => {
                    // Exit rlox interpreter
                    return InterpretResult::Ok;
                }
                OpCode::SetGlobal => {
                    if !self.op_set_global(chunk, false) {
                        return InterpretResult::RuntimeError;
                    }
                }
                OpCode::SetGlobalLong => {
                    if !self.op_set_global(chunk, true) {
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
                &value_as_rlox_string_ref(x),
                &value_as_rlox_string_ref(y),
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

    fn validate_two_operands(
        &mut self,
        chunk: &Chunk,
        validator: fn(&Value) -> bool,
        message: &str,
    ) -> bool {
        if !self.type_check_two_operands(validator) {
            self.runtime_error(chunk, message);
            return false;
        }

        true
    }

    fn runtime_error(&mut self, chunk: &Chunk, message: &str) {
        eprintln!("{}", message);
        let offset = VM::get_offset(chunk, self.ip);
        let line = chunk.get_line(offset);
        eprintln!("[line {}] in script", line.no);

        self.stack.reset();
    }

    fn read_constant_string(&mut self, chunk: &mut Chunk, long_constant: bool) -> &ObjString {
        let value = unsafe {
            if long_constant {
                chunk.read_constant(self.read_long_index())
            } else {
                chunk.read_constant(self.read_byte() as InstructionIndex)
            }
        };

        value_as_rlox_string_ref(value)
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

        let value = unsafe { self.stack.get_at_unchecked(idx).clone() };
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
        unsafe { self.stack.set_at_unchecked(slot, value) };
    }

    fn op_get_global(&mut self, chunk: &mut Chunk, long_global: bool) -> bool {
        let var_name = self.read_constant_string(chunk, long_global).clone();
        match self.globals.get(&var_name) {
            Some(value) => {
                // TODO can value be cloned here?
                self.stack.push(value.clone());
                true
            }
            None => {
                self.runtime_error(chunk, &format!("Undefined variable '{}'.", var_name.data));
                false
            }
        }
    }

    fn op_set_global(&mut self, chunk: &mut Chunk, long_global: bool) -> bool {
        let var_name = self.read_constant_string(chunk, long_global).clone();

        match self.globals.get(&var_name) {
            Some(_) => {
                let value = self.stack.peek(0).unwrap().clone();
                self.globals.insert(var_name, value);
                true
            }
            None => {
                self.runtime_error(chunk, &format!("Undefined variable '{}'.", &var_name.data));
                false
            }
        }
    }

    #[inline(always)]
    unsafe fn read_byte(&mut self) -> u8 {
        let byte = *self.ip;
        self.ip = self.ip.offset(1);

        byte
    }

    #[inline(always)]
    unsafe fn read_short(&mut self) -> u16 {
        self.ip = self.ip.offset(2);

        ((*self.ip.offset(-2) as u16) << 8u16) | *self.ip.offset(-1) as u16
    }

    unsafe fn read_long_index(&mut self) -> InstructionIndex {
        InstructionIndex::from_n_most_significant_le_bytes::<3>([
            self.read_byte(),
            self.read_byte(),
            self.read_byte(),
        ])
    }

    #[inline(always)]
    fn get_offset(chunk: &Chunk, code_ptr: *const u8) -> InstructionIndex {
        let start = chunk.code.as_ptr() as usize;
        code_ptr as usize - start
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
}
