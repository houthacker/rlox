use crate::{Chunk, OpCode};

#[cfg(feature = "rlox_debug")]
pub fn disassemble_chunk(chunk: &Chunk, name: &str) {
    println!("== {} ==", name);

    let code = &chunk.code;
    let mut offset: usize = 0;
    while offset < code.len() {
        offset = disassemble_instruction(chunk, offset);
    }
}

#[cfg(feature = "rlox_debug")]
pub fn disassemble_instruction(chunk: &Chunk, offset: usize) -> usize {
    print!("{:04} ", offset);

    let instruction: u8 = chunk.code[offset];
    match OpCode::try_from(instruction) {
        Ok(opcode) => return simple_instruction(&opcode.to_string(), offset),
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
