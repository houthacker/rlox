use crate::chunk::{IndexConverter, InstructionIndex};
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
        Ok(OpCode::Call) => byte_instruction(&OpCode::Call.to_string(), chunk, offset),
        Ok(OpCode::Constant) => constant_instruction(&OpCode::Constant.to_string(), chunk, offset),
        Ok(OpCode::ConstantLong) => {
            constant_long_instruction(&OpCode::ConstantLong.to_string(), chunk, offset)
        }
        Ok(OpCode::DefineGlobal) => {
            constant_instruction(&OpCode::DefineGlobal.to_string(), chunk, offset)
        }
        Ok(OpCode::DefineGlobalLong) => {
            constant_long_instruction(&OpCode::DefineGlobalLong.to_string(), chunk, offset)
        }
        Ok(OpCode::GetGlobal) => {
            constant_instruction(&OpCode::GetGlobal.to_string(), chunk, offset)
        }
        Ok(OpCode::GetGlobalLong) => {
            constant_long_instruction(&OpCode::GetGlobalLong.to_string(), chunk, offset)
        }
        Ok(OpCode::Jump) => jump_instruction(&OpCode::Jump.to_string(), 1, chunk, offset),
        Ok(OpCode::JumpIfFalse) => {
            jump_instruction(&OpCode::JumpIfFalse.to_string(), 1, chunk, offset)
        }
        Ok(OpCode::Loop) => jump_instruction(&OpCode::Loop.to_string(), -1, chunk, offset),
        Ok(OpCode::SetGlobal) => {
            constant_instruction(&OpCode::SetGlobal.to_string(), chunk, offset)
        }
        Ok(OpCode::SetGlobalLong) => {
            constant_long_instruction(&OpCode::SetGlobalLong.to_string(), chunk, offset)
        }
        Ok(OpCode::GetLocal) => byte_instruction(&OpCode::GetLocal.to_string(), chunk, offset),
        Ok(OpCode::GetLocalLong) => {
            byte_long_instruction(&OpCode::GetLocal.to_string(), chunk, offset)
        }
        Ok(OpCode::SetLocal) => byte_instruction(&OpCode::GetLocal.to_string(), chunk, offset),
        Ok(OpCode::SetLocalLong) => {
            byte_long_instruction(&OpCode::GetLocal.to_string(), chunk, offset)
        }
        Ok(OpCode::PopN) => byte_instruction(&OpCode::PopN.to_string(), chunk, offset),
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

fn byte_instruction(name: &str, chunk: &Chunk, offset: InstructionIndex) -> InstructionIndex {
    let slot = chunk.code[offset + 1];
    println!("{:-16} {:4}", name, slot);
    offset + 2
}

fn jump_instruction(
    name: &str,
    sign: isize,
    chunk: &Chunk,
    offset: InstructionIndex,
) -> InstructionIndex {
    let mut jump = (chunk.code[offset + 1] as u16) << 8u16;
    jump |= chunk.code[offset + 2] as u16;

    let addend = 3 + sign * (jump as isize);
    let loc = if addend >= 0 {
        offset + (addend as usize)
    } else {
        offset - (-addend as usize)
    };

    println!("{:-16} {:4} -> {}", name, offset, loc);

    offset + 3
}

fn byte_long_instruction(name: &str, chunk: &Chunk, offset: InstructionIndex) -> InstructionIndex {
    let slot = InstructionIndex::from_n_most_significant_le_bytes::<3>([
        chunk.code[offset + 1],
        chunk.code[offset + 2],
        chunk.code[offset + 3],
    ]);
    println!("{:-16} {:4}", name, slot);
    offset + 4
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
    let idx = InstructionIndex::from_n_most_significant_le_bytes::<3>([
        chunk.code[offset + 1],
        chunk.code[offset + 2],
        chunk.code[offset + 3],
    ]);
    print_constant(name, idx, chunk);

    offset + 4
}

fn print_constant(name: &str, idx: InstructionIndex, chunk: &Chunk) {
    print!("{:-16} {:4} '", name, idx);
    print_value(&chunk.constants[idx]);
    println!("'");
}
