mod chunk;

#[cfg(feature = "rlox_debug")]
mod debug;

use chunk::{ Chunk, OpCode };

fn main() {
    let mut chunk = Chunk::new();
    chunk.write(OpCode::OpReturn as u8);

    #[cfg(feature = "rlox_debug")]
    debug::disassemble_chunk(&chunk, "test chunk");

    // required check
    Chunk::destroy(chunk);

    println!("Done")
}
