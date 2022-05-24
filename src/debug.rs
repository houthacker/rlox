use crate::chunk::InstructionIndex;
use crate::value::print_value;
use crate::{Chunk, OpCode};

#[cfg(feature = "rlox_debug")]
pub fn disassemble_chunk(chunk: &Chunk, name: &str) {
    println!("== {} ==", name);

    let code = &chunk.code;
    let mut offset: InstructionIndex = 0;
    while offset < code.len() {
        offset = disassemble_instruction(chunk, offset);
    }
}

#[cfg(feature = "rlox_debug")]
pub fn disassemble_instruction(chunk: &Chunk, offset: InstructionIndex) -> InstructionIndex {
    print!("{:04} ", offset);
    let ln = chunk.get_line(offset);

    if offset > 0 && ln == chunk.get_line(offset - 1) {
        print!("   | ")
    } else {
        print!("{:4} ", ln.no)
    }

    let instruction: u8 = chunk.code[offset];
    match OpCode::try_from(instruction) {
        Ok(OpCode::Constant) => constant_instruction(&OpCode::Constant.to_string(), chunk, offset),
        Ok(OpCode::ConstantLong) => {
            constant_long_instruction(&OpCode::ConstantLong.to_string(), chunk, offset)
        }
        Ok(opcode) => simple_instruction(&opcode.to_string(), offset),
        Err(msg) => {
            println!("{} {}", msg, instruction);
            offset + 1
        }
    }
}

fn simple_instruction(name: &str, offset: usize) -> usize {
    println!("{}", name);
    offset + 1
}

fn constant_instruction(name: &str, chunk: &Chunk, offset: InstructionIndex) -> InstructionIndex {
    // Retrieve the index of the constant value (offset + 0 is OpCode::OpConstant)
    print_constant(name, chunk.code[offset + 1] as usize, chunk);
    offset + 2
}

fn constant_long_instruction(
    name: &str,
    chunk: &Chunk,
    offset: InstructionIndex,
) -> InstructionIndex {
    let le_bytes = [
        chunk.code[offset + 1],
        chunk.code[offset + 2],
        chunk.code[offset + 3],
        0,
        0,
        0,
        0,
        0,
    ];
    let idx = usize::from_le_bytes(le_bytes);
    print_constant(name, idx, chunk);

    offset + 4
}

fn print_constant(name: &str, idx: InstructionIndex, chunk: &Chunk) {
    print!("{:-16} {:4} '", name, idx);
    print_value(&chunk.constants[idx]);
    println!("'");
}
