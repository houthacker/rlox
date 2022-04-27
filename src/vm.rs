use crate::debug::disassemble_instruction;
use crate::value::{print_value, Value};
use crate::{Chunk, OpCode};
use std::ptr;

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
}

#[cfg(feature = "rlox_debug")]
impl Drop for VM {
    fn drop(&mut self) {
        println!("Dropping VM");
    }
}

impl VM {
    pub fn new() -> Self {
        VM {
            ip: ptr::null_mut(),
            stack: [None; VM_STACK_MAX],
            stack_top: 0,
        }
    }

    pub fn interpret(&mut self, chunk: &mut Chunk) -> InterpretResult {
        self.ip = chunk.code.as_mut_ptr();

        unsafe { self.run(chunk) }
    }

    unsafe fn run(&mut self, chunk: &Chunk) -> InterpretResult {
        loop {
            if cfg!(feature = "rlox_debug") {
                // Stack tracking
                print!("          ");
                for sp in 0..self.stack_top {
                    print!("[ ");
                    print_value(&self.stack[sp].unwrap());
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
                OpCode::OpAdd => {
                    self.op_add();
                }
                OpCode::OpConstant => {
                    let value = self.read_constant(chunk);
                    self.stack_push(value);
                }
                OpCode::OpConstantLong => {
                    let value = self.read_long_constant(chunk);
                    self.stack_push(value);
                }
                OpCode::OpDivide => {
                    self.op_divide();
                }
                OpCode::OpMultiply => {
                    self.op_multiply();
                }
                OpCode::OpNegate => {
                    self.stack_top_negate();
                }
                OpCode::OpReturn => {
                    print_value(&self.stack_pop());
                    println!();
                    return InterpretResult::Ok;
                }
                OpCode::OpSubtract => {
                    self.op_subtract();
                }
                _ => return InterpretResult::CompileError,
            };
        }
    }

    fn reset_stack(&mut self) {
        self.stack = [None; VM_STACK_MAX];
        self.stack_top = 0;
    }

    fn stack_push(&mut self, value: Value) {
        self.stack[self.stack_top] = Some(value);
        self.stack_top += 1;
    }

    fn stack_pop(&mut self) -> Value {
        self.stack_top -= 1;
        self.stack[self.stack_top].unwrap()
    }

    fn stack_top_negate(&mut self) {
        self.stack[self.stack_top] = Some(-self.stack[self.stack_top].unwrap())
    }

    #[inline(always)]
    fn op_add(&mut self) {
        let (y, x) = self.pop_two();
        self.stack_push(x + y);
    }

    #[inline(always)]
    fn op_divide(&mut self) {
        let (y, x) = self.pop_two();
        self.stack_push(x / y);
    }

    #[inline(always)]
    fn op_multiply(&mut self) {
        let (y, x) = self.pop_two();
        self.stack_push(x * y);
    }

    #[inline(always)]
    fn op_subtract(&mut self) {
        let (y, x) = self.pop_two();
        self.stack_push(x - y);
    }

    #[inline(always)]
    fn pop_two(&mut self) -> (Value, Value) {
        (self.stack_pop(), self.stack_pop())
    }

    #[inline(always)]
    unsafe fn read_byte(&mut self) -> u8 {
        let byte = *self.ip;
        self.ip = self.ip.offset(1);

        byte
    }

    #[inline(always)]
    unsafe fn read_constant(&mut self, chunk: &Chunk) -> Value {
        *chunk.constants.get_unchecked(self.read_byte() as usize)
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

        *chunk.constants.get_unchecked(idx)
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
        let mut chunk = Chunk::new();
        let mut vm = VM::new();

        chunk.write_constant(1.2, 123);
        chunk.write_constant(3.4, 123);
        chunk.write(OpCode::OpAdd as u8, 123);
        chunk.write_constant(5.6, 123);
        chunk.write(OpCode::OpDivide as u8, 123);
        chunk.write(OpCode::OpNegate as u8, 123);
        chunk.write(OpCode::OpReturn as u8, 123);

        vm.interpret(&mut chunk);
    }
}
