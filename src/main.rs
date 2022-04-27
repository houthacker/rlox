mod chunk;
mod value;
mod vm;

#[cfg(feature = "rlox_debug")]
mod debug;

use chunk::{Chunk, OpCode};
use vm::VM;

fn main() {
    let mut chunk = Chunk::new();
    let mut vm = VM::new();

    chunk.write_constant(1.2, 123);
    chunk.write(OpCode::OpNegate as u8, 123);
    chunk.write(OpCode::OpReturn as u8, 120);

    // #[cfg(feature = "rlox_debug")]
    debug::disassemble_chunk(&chunk, "test chunk");

    vm.interpret(&mut chunk);
    println!("Done")
}
