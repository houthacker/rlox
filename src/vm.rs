#[cfg(feature = "rlox_debug")]
use crate::debug::disassemble_instruction;

use crate::object::{value_as_rlox_string, Obj, ObjString};
use crate::value::{as_bool, as_number, print_value, Value};
use crate::{Chunk, Compiler, OpCode};

use crate::chunk::{InstructionIndex, InstructionIndexConverter};
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
}

impl VM {
    pub fn new() -> Self {
        Self {
            ip: ptr::null_mut(),
            stack: UnsafeStack::new(),
            globals: Table::new(),
        }
    }

    pub fn interpret(source: String) -> InterpretResult {
        let mut chunk = Chunk::new();
        let mut compiler = Compiler::new(Scanner::new(source), &mut chunk);

        if !compiler.compile() {
            InterpretResult::CompileError
        } else {
            let mut vm = VM::new();
            unsafe { vm.run(&mut chunk) }
        }
    }

    pub unsafe fn run(&mut self, chunk: &mut Chunk) -> InterpretResult {
        self.ip = chunk.code.as_mut_ptr();

        loop {
            if cfg!(feature = "rlox_debug") {
                // Stack tracking
                print!("          ");
                for sp in 0..self.stack.len() {
                    print!("[ ");
                    match self.stack.peek(sp) {
                        Some(elem) => {
                            print_value(elem);
                        }
                        None => (),
                    }
                    print!(" ]");
                }
                println!();

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
                    let value = chunk.read_constant(self.read_long_constant_index());
                    self.stack.push(value);
                }
                OpCode::DefineGlobal => {
                    let var_name: ObjString = self.read_constant_string(chunk, false);

                    // Note: the clox implementation pop()'s the value off of the stack
                    // _after_ inserting it on the stack (using peek(0)).
                    // This is to support a triggered garbage collection while interning the string.
                    // In rlox we hope to prevent the need for a GC due to rusts'
                    // memory model characteristics.
                    let value = self.stack.pop().unwrap();
                    self.globals.insert(var_name, value);

                    if cfg!(feature = "rlox_debug") {
                        println!("{:?}", &self.globals);
                    }
                }
                OpCode::DefineLongGlobal => {
                    let var_name: ObjString = self.read_constant_string(chunk, true);

                    // Note: the clox implementation pop()'s the value off of the stack
                    // _after_ inserting it on the stack (using peek(0)).
                    // This is to support a triggered garbage collection while interning the string.
                    // In rlox we hope to prevent the need for a GC due to rusts'
                    // memory model characteristics.
                    self.globals.insert(var_name, self.stack.pop().unwrap());
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
                OpCode::GetLongGlobal => {
                    if !self.op_get_global(chunk, true) {
                        return InterpretResult::RuntimeError;
                    }
                }
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
                OpCode::Pop => match self.stack.pop() {
                    Some(_) => (),
                    None => (),
                },
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
                OpCode::Subtract => {
                    self.op_subtract();
                }
                OpCode::True => self.stack.push(Value::from_bool(true)),
            };
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
                self.stack.replace_top(replacement);
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
            let concatenated = ObjString::add(&value_as_rlox_string(x), &value_as_rlox_string(y));
            self.stack.push(Value::from_obj(Obj::String(concatenated)))
        }
    }

    fn type_check_two_operands(&mut self, validator: fn(&Value) -> bool) -> bool {
        if cfg!(feature = "rlox_debug") {
            println!("\nStack: {:?}", self.stack);
        }
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

    fn read_constant_string(&mut self, chunk: &mut Chunk, long_constant: bool) -> ObjString {
        let value = unsafe {
            if long_constant {
                chunk.read_constant(self.read_long_constant_index())
            } else {
                chunk.read_constant(self.read_byte() as InstructionIndex)
            }
        };

        value_as_rlox_string(value)
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

    #[inline(always)]
    fn op_get_global(&mut self, chunk: &mut Chunk, long_global: bool) -> bool {
        let name = self.read_constant_string(chunk, long_global);
        match self.globals.get(&name) {
            Some(value) => {
                // TODO can value be cloned here?
                self.stack.push(value.clone());
                true
            }
            None => {
                self.runtime_error(chunk, &format!("Undefined variable '{}'.", &name.data));
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

    unsafe fn read_long_constant_index(&mut self) -> InstructionIndex {
        InstructionIndex::from_most_significant_le_bytes([
            self.read_byte(),
            self.read_byte(),
            self.read_byte(),
        ])
    }

    #[inline(always)]
    fn get_offset(chunk: &Chunk, code_ptr: *const u8) -> usize {
        let start = chunk.code.as_ptr() as usize;
        code_ptr as usize - start
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    #[ignore] // return statements do not yet work
    fn interpret_test_chunk() {
        let source = String::from("return -((1.2 + 3.4) / 5.6)");
        assert_eq!(VM::interpret(source), InterpretResult::Ok);
    }

    #[test]
    fn interpret_numeric_expr() {
        let source = String::from("!(5 - 4 > 3 * 2 == !nil)");
        assert_eq!(VM::interpret(source), InterpretResult::Ok);
    }

    #[test]
    fn interpret_string_concatenation() {
        let source = String::from("\"st\" + \"ri\" + \"ng\"");
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
}
