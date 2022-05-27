use crate::debug::{disassemble_chunk, disassemble_instruction};
use crate::object::{as_string_ref, Obj};
use crate::value::{cleanup_value, print_value, Value, ValueType, U};
use crate::{as_bool, as_number, bool_val, nil_val, number_val, obj_val, Chunk, Compiler, OpCode};

use std::ptr;

const VM_STACK_FILLER: Option<Value> = None;
const VM_STACK_MAX: usize = 256;

#[cfg_attr(feature = "rlox_debug", derive(Debug))]
pub enum InterpretResult {
    Ok,
    CompileError,
    RuntimeError,
}

#[cfg_attr(feature = "rlox_debug", derive(Debug))]
pub struct VM {
    ip: *mut u8,
    stack: [Option<Value>; VM_STACK_MAX],
    stack_top: usize,
    stack_max: usize,
    compiler: Compiler,
}

#[cfg(feature = "rlox_debug")]
impl Drop for VM {
    fn drop(&mut self) {
        if self.stack_max > 0 {
            for n in 0..self.stack_max {
                match self.stack[n].to_owned() {
                    Some(v) => {
                        cleanup_value(v);
                    }
                    None => (),
                }
            }
        }
    }
}

impl VM {
    pub fn new() -> Self {
        Self {
            ip: ptr::null_mut(),
            stack: [VM_STACK_FILLER; VM_STACK_MAX],
            stack_top: 0,
            stack_max: 0,
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
                for sp in 0..self.stack_top {
                    print!("[ ");
                    print_value(self.stack[sp].as_ref().unwrap_unchecked());
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
                    let value = self.read_constant(chunk);
                    self.stack_push(value);
                }
                OpCode::ConstantLong => {
                    let value = self.read_long_constant(chunk);
                    self.stack_push(value);
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
                    let y = self.stack_pop().to_owned();
                    let x = self.stack_pop().to_owned();
                    let result = bool_val!(x == y);

                    self.stack_push(result);
                }
                OpCode::False => self.stack_push(bool_val!(false)),
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
                OpCode::Negate => {
                    if !Value::is_number(self.peek(0)) {
                        self.runtime_error(chunk, "Operand must be a number.");
                        return InterpretResult::RuntimeError;
                    }
                    self.stack_top_negate_number();
                }
                OpCode::Nil => self.stack_push(nil_val!()),
                OpCode::Not => {
                    let value = self.stack_pop();
                    self.stack_push(bool_val!(Self::is_falsey(&value)))
                }
                OpCode::Return => {
                    print_value(&self.stack_pop());
                    println!();
                    return InterpretResult::Ok;
                }
                OpCode::Subtract => {
                    self.op_subtract();
                }
                OpCode::True => self.stack_push(bool_val!(true)),
            };
        }
    }

    fn reset_stack(&mut self) {
        self.stack = [VM_STACK_FILLER; VM_STACK_MAX];
        self.stack_top = 0;
    }

    fn stack_push(&mut self, value: Value) {
        self.stack_drop_top_in_place();

        self.stack[self.stack_top] = Some(value);
        self.stack_top += 1;
        self.stack_max = std::cmp::max(self.stack_top, self.stack_max);
    }

    fn stack_drop_top_in_place(&mut self) {
        match self.stack[self.stack_top].to_owned() {
            Some(v) => {
                cleanup_value(v);
            }
            None => (),
        }
    }

    fn stack_pop(&mut self) -> Value {
        self.stack_top -= 1;
        unsafe { self.stack[self.stack_top].unwrap_unchecked() }
    }

    fn stack_pop_two(&mut self) -> (Value, Value) {
        self.stack_top -= 2;

        unsafe {
            (
                self.stack[self.stack_top + 1].unwrap_unchecked(),
                self.stack[self.stack_top].unwrap_unchecked(),
            )
        }
    }

    fn stack_top_negate_number(&mut self) {
        // in-place replacement of number values does not require
        // dropping these values first, because they are completely
        // stored on the (Rust) stack, and do not refer to any heap-allocated storage.
        self.stack[self.stack_top] = Some(number_val!(-as_number!(unsafe {
            self.stack[self.stack_top].unwrap_unchecked()
        })))
    }

    fn peek(&mut self, distance: usize) -> &Value {
        unsafe {
            self.stack[self.stack_top - 1 - distance]
                .as_ref()
                .unwrap_unchecked()
        }
    }

    fn is_falsey(value: &Value) -> bool {
        Value::is_nil(value) || (Value::is_bool(value) && !as_bool!(value))
    }

    fn concatenate(&mut self) {
        let y = self.stack_pop().to_owned();
        let x = self.stack_pop().to_owned();
        let concatenated = as_string_ref(&x) + as_string_ref(&y);

        self.stack_push(obj_val!(concatenated))
    }

    fn type_check_two_operands(&mut self, validator: fn(&Value) -> bool) -> bool {
        validator(self.peek(0)) && validator(self.peek(1))
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

        self.reset_stack();
    }

    #[inline(always)]
    fn op_add(&mut self) {
        let (y, x) = self.stack_pop_two();
        let result = number_val!(as_number!(x) + as_number!(y));

        self.stack_push(result);
    }

    #[inline(always)]
    fn op_divide(&mut self) {
        let (y, x) = self.stack_pop_two();
        let result = number_val!(as_number!(x) / as_number!(y));

        self.stack_push(result);
    }

    #[inline(always)]
    fn op_greater(&mut self) {
        let (y, x) = self.stack_pop_two();
        let result = bool_val!(as_number!(x) > as_number!(y));

        self.stack_push(result);
    }

    #[inline(always)]
    fn op_less(&mut self) {
        let (y, x) = self.stack_pop_two();
        let result = bool_val!(as_number!(x) < as_number!(y));

        self.stack_push(result);
    }

    #[inline(always)]
    fn op_multiply(&mut self) {
        let (y, x) = self.stack_pop_two();
        let result = number_val!(as_number!(x) * as_number!(y));

        self.stack_push(result);
    }

    #[inline(always)]
    fn op_subtract(&mut self) {
        let (y, x) = self.stack_pop_two();
        let result = number_val!(as_number!(x) - as_number!(y));

        self.stack_push(result);
    }

    #[inline(always)]
    unsafe fn read_byte(&mut self) -> u8 {
        let byte = *self.ip;
        self.ip = self.ip.offset(1);

        byte
    }

    #[inline(always)]
    unsafe fn read_constant(&mut self, chunk: &Chunk) -> Value {
        chunk.constants[self.read_byte() as usize]
    }

    unsafe fn read_long_constant(&mut self, chunk: &Chunk) -> Value {
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
        let idx = usize::from_le_bytes(le_bytes);

        chunk.constants[idx]
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
