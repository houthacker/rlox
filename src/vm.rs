use crate::debug::{disassemble_chunk, disassemble_instruction};
use crate::object::value_as_rlox_string_ref;
use crate::value::{as_bool, as_number, print_value, Value};
use crate::{Chunk, Compiler, OpCode};

use crate::stack::Stack;
use std::ptr;

#[cfg_attr(feature = "rlox_debug", derive(Debug))]
pub enum InterpretResult {
    Ok,
    CompileError,
    RuntimeError,
}

#[cfg_attr(feature = "rlox_debug", derive(Debug))]
pub struct VM {
    ip: *mut u8,
    stack: Stack<Value, 256>,
    compiler: Compiler,
}

impl VM {
    pub fn new() -> Self {
        Self {
            ip: ptr::null_mut(),
            stack: Stack::new(),
            compiler: Compiler::new(),
        }
    }

    pub fn interpret(&mut self, source: String) -> InterpretResult {
        let mut chunk = Chunk::new();

        if !self.compiler.compile(source, &mut chunk) {
            return InterpretResult::CompileError;
        }

        disassemble_chunk(&chunk, "Chunk");

        self.ip = chunk.code.as_mut_ptr();

        unsafe { self.run(&chunk) }
    }

    unsafe fn run(&mut self, chunk: &Chunk) -> InterpretResult {
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
                OpCode::Return => match self.stack.pop() {
                    Some(elem) => {
                        print_value(&elem);
                        println!();
                        return InterpretResult::Ok;
                    }
                    None => panic!("StackUnderFlow"),
                },
                OpCode::Subtract => {
                    self.op_subtract();
                }
                OpCode::True => self.stack.push(Value::from_bool(true)),
            };
        }
    }

    fn stack_pop_two(&mut self) -> Option<(Value, Value)> {
        if self.stack.len() >= 2 {
            Some((self.stack.pop().unwrap(), self.stack.pop().unwrap()))
        } else {
            None
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
        match self.stack_pop_two() {
            Some((y, x)) => {
                let concatenated = value_as_rlox_string_ref(&x) + value_as_rlox_string_ref(&y);
                self.stack.push(Value::from_obj(Box::new(concatenated)))
            }
            None => (),
        }
    }

    fn type_check_two_operands(&mut self, validator: fn(&Value) -> bool) -> bool {
        match self.stack.peek(0) {
            Some(rhs) => match self.stack.peek(1) {
                Some(lhs) => validator(lhs) && validator(rhs),
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
    unsafe fn read_byte(&mut self) -> u8 {
        let byte = *self.ip;
        self.ip = self.ip.offset(1);

        byte
    }

    unsafe fn read_long_constant_index(&mut self) -> usize {
        let le_bytes = [
            self.read_byte(),
            self.read_byte(),
            self.read_byte(),
            0,
            0,
            0,
            0,
            0,
        ];

        usize::from_le_bytes(le_bytes)
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
    fn interpret_test_chunk() {
        let mut vm = VM::new();
        let source = String::from("return -((1.2 + 3.4) / 5.6)");

        vm.interpret(source);
    }

    #[test]
    fn interpret_numeric_expr() {
        let mut vm = VM::new();
        let source = String::from("!(5 - 4 > 3 * 2 == !nil)");

        vm.interpret(source);
    }

    #[test]
    fn interpret_string_concatenation() {
        let mut vm = VM::new();
        let source = String::from("\"st\" + \"ri\" + \"ng\"");

        vm.interpret(source);
    }

    #[test]
    fn interpret_string() {
        let mut vm = VM::new();
        let source = String::from("\"abc\"");

        vm.interpret(source);
    }
}
