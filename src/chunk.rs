use std::fmt;
use std::fmt::Formatter;

/// bytecode instructions for the rlox VM
#[cfg_attr(feature = "rlox_debug", derive(Debug))]
pub enum OpCode {
    OpReturn = 0,
}

impl TryFrom<u8> for OpCode {
    type Error = &'static str;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(OpCode::OpReturn),
            _ => Err("Unknown OpCode"),
        }
    }
}

#[cfg(feature = "rlox_debug")]
impl fmt::Display for OpCode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[cfg_attr(feature = "rlox_debug", derive(Debug))]
pub struct Chunk {
    pub code: Vec<u8>,
}

#[cfg(feature = "rlox_debug")]
impl Drop for Chunk {
    fn drop(&mut self) {
        println!("Dropping Chunk");
    }
}

impl Chunk {
    pub fn new() -> Chunk {
        Chunk { code: Vec::new() }
    }

    pub fn destroy(_chunk: Chunk) {
        // calls drop
    }

    pub fn write(&mut self, byte: u8) {
        self.code.push(byte);
    }
}
