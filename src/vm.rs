use crate::debug::{disassemble_chunk, disassemble_instruction};
use crate::value::{print_value, Value, ValueType, U};
use crate::{as_bool, as_number, bool_val, nil_val, number_val, Chunk, Compiler, OpCode};
use std::mem::MaybeUninit;
use std::ptr;

const VM_STACK_FILLER: MaybeUninit<Value> = MaybeUninit::uninit();
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
    stack: [MaybeUninit<Value>; VM_STACK_MAX],
    stack_top: usize,
    compiler: Compiler,
}

#[cfg(feature = "rlox_debug")]
impl Drop for VM {
    fn drop(&mut self) {
        println!("Dropping VM");
    }
}

impl VM {
    pub fn new() -> Self {
        Self {
            ip: ptr::null_mut(),
            stack: [VM_STACK_FILLER; VM_STACK_MAX],
            stack_top: 0,
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
                    print_value(self.stack[sp].assume_init_ref());
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
                    if self.validate_two_operands(
                        chunk,
                        Value::is_number,
                        "Operands must be numbers.",
                    ) {
                        self.op_add();
                    } else {
                        return InterpretResult::RuntimeError;
                    }
                }
                OpCode::Constant => {
                    let value = self.read_constant(chunk);
                    self.stack_push(*value);
                }
                OpCode::ConstantLong => {
                    let value = self.read_long_constant(chunk);
                    self.stack_push(*value);
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
                    let y = self.stack_pop();
                    let x = self.stack_pop();
                    let result = bool_val!(Self::values_equal(&x, &y));
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
                    if !Value::is_number(&self.peek(0)) {
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
        self.stack[self.stack_top] = MaybeUninit::new(value);
        self.stack_top += 1;
    }

    fn stack_pop(&mut self) -> Value {
        self.stack_top -= 1;
        unsafe { self.stack[self.stack_top].assume_init() }
    }

    fn stack_top_negate_number(&mut self) {
        self.stack[self.stack_top] = MaybeUninit::new(number_val!(-as_number!(unsafe {
            self.stack[self.stack_top].assume_init()
        })))
    }

    fn peek(&mut self, distance: usize) -> Value {
        unsafe { self.stack[self.stack_top - 1 - distance].assume_init() }
    }

    fn is_falsey(value: &Value) -> bool {
        Value::is_nil(value) || (Value::is_bool(value) && !as_bool!(*value))
    }

    fn values_equal(x: &Value, y: &Value) -> bool {
        if x.kind != y.kind {
            false
        } else {
            match x.kind {
                ValueType::Nil => true,
                ValueType::Bool => as_bool!(*x) == as_bool!(*y),
                ValueType::Number => as_number!(*x) == as_number!(*y),
            }
        }
    }

    fn validate_two_operands(
        &mut self,
        chunk: &Chunk,
        validator: fn(&Value) -> bool,
        message: &str,
    ) -> bool {
        if !validator(&self.peek(0)) || !validator(&self.peek(1)) {
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
        let (y, x) = self.pop_two();
        self.stack_push(number_val!(as_number!(x) + as_number!(y)));
    }

    #[inline(always)]
    fn op_divide(&mut self) {
        let (y, x) = self.pop_two();
        self.stack_push(number_val!(as_number!(x) / as_number!(y)));
    }

    #[inline(always)]
    fn op_greater(&mut self) {
        let (y, x) = self.pop_two();
        self.stack_push(bool_val!(as_number!(x) > as_number!(y)));
    }

    #[inline(always)]
    fn op_less(&mut self) {
        let (y, x) = self.pop_two();
        self.stack_push(bool_val!(as_number!(x) < as_number!(y)));
    }

    #[inline(always)]
    fn op_multiply(&mut self) {
        let (y, x) = self.pop_two();
        self.stack_push(number_val!(as_number!(x) * as_number!(y)));
    }

    #[inline(always)]
    fn op_subtract(&mut self) {
        let (y, x) = self.pop_two();
        self.stack_push(number_val!(as_number!(x) - as_number!(y)));
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
    unsafe fn read_constant<'b>(&mut self, chunk: &'b Chunk) -> &'b Value {
        chunk.constants.get_unchecked(self.read_byte() as usize)
    }

    unsafe fn read_long_constant<'b>(&mut self, chunk: &'b Chunk) -> &'b Value {
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

        chunk.constants.get_unchecked(idx)
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
}