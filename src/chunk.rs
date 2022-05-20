use crate::value::{Value, ValueType, U};
use std::fmt::{Display, Formatter};

pub type InstructionIndex = usize;
pub type LineNumber = u32;

/// bytecode instructions for the rlox VM
#[cfg_attr(feature = "rlox_debug", derive(Debug))]
pub enum OpCode {
    Add,
    Constant,
    ConstantLong,
    Divide,
    Equal,
    False,
    Greater,
    Less,
    Multiply,
    Negate,
    Nil,
    Not,
    Return,
    Subtract,
    True,
}

impl Display for OpCode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl TryFrom<u8> for OpCode {
    type Error = &'static str;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(OpCode::Add),
            1 => Ok(OpCode::Constant),
            2 => Ok(OpCode::ConstantLong),
            3 => Ok(OpCode::Divide),
            4 => Ok(OpCode::Equal),
            5 => Ok(OpCode::False),
            6 => Ok(OpCode::Greater),
            7 => Ok(OpCode::Less),
            8 => Ok(OpCode::Multiply),
            9 => Ok(OpCode::Negate),
            10 => Ok(OpCode::Nil),
            11 => Ok(OpCode::Not),
            12 => Ok(OpCode::Return),
            13 => Ok(OpCode::Subtract),
            14 => Ok(OpCode::True),
            _ => Err("Unknown OpCode"),
        }
    }
}

#[cfg_attr(feature = "rlox_debug", derive(Debug))]
#[derive(Clone, PartialEq)]
pub struct Line {
    pub no: LineNumber,
    pub references: usize,
}

#[cfg_attr(feature = "rlox_debug", derive(Debug))]
#[derive(Clone)]
pub struct Chunk {
    pub code: Vec<u8>,
    pub lines: Vec<Line>,
    pub constants: Vec<Value>,
}

impl Chunk {
    pub fn new() -> Self {
        Self {
            code: Vec::new(),
            lines: Vec::new(),
            constants: Vec::new(),
        }
    }

    /// Appends the given byte to this chunk.
    ///
    /// # Arguments
    /// * `byte` - The coded byte to append
    /// * `line` - The line number at which the related source code resides
    pub fn write(&mut self, byte: u8, line: LineNumber) {
        self.code.push(byte);
        self.encode_line(line)
    }

    /// Stores the given value in this chunk's constant pool
    /// and encodes it in the resulting byte code.
    ///
    /// # Panics
    /// * If the amount of constants exceeds ```2^24-1```
    /// # Arguments
    /// * `value` - The constant value to write
    /// * `line` - The related source code line number  
    pub fn write_constant(&mut self, value: Value, line: LineNumber) {
        match self.add_constant(value) {
            x if x <= u8::MAX as InstructionIndex => {
                self.write(OpCode::Constant as u8, line);
                self.write(x as u8, line);
            }
            x => {
                self.write(OpCode::ConstantLong as u8, line);
                let bytes: [u8; 4] = (x as u32).to_le_bytes();

                // rlox supports constant indexes up to 24 bits, so assert the last byte is zero
                if bytes[3] == 0 {
                    for i in 0..=2 {
                        self.write(bytes[i], line);
                    }

                    return;
                }

                panic!("Too many constants")
            }
        }
    }

    /// Returns the source code line at which the given instruction
    /// resides.
    ///
    /// # Panics
    /// * If the given InstructionIndex doesn't exist in this chunk.
    /// # Arguments
    /// * `idx` - The index of the instruction within this chunk
    pub fn get_line(&self, idx: InstructionIndex) -> &Line {
        self.try_get_line(idx).unwrap()
    }

    // Adds the given value to the constant pool
    // of this chunk, and returns the index at which
    // it was added.
    fn add_constant(&mut self, value: Value) -> InstructionIndex {
        self.constants.push(value);
        self.constants.len() - 1
    }

    fn try_get_line(&self, idx: InstructionIndex) -> Option<&Line> {
        let mut count = idx + 1; // InstructionIndex is 0-based

        for line in &self.lines {
            if count <= line.references {
                return Some(line);
            }

            count -= line.references
        }

        None
    }

    fn encode_line(&mut self, line: LineNumber) {
        if self.lines.is_empty() {
            self.lines.push(Line {
                no: line,
                references: 1,
            })
        } else if self.lines[self.lines.len() - 1].no == line {
            unsafe {
                let len = self.lines.len();
                self.lines.get_unchecked_mut(len - 1).references += 1
            }
        } else {
            let mut found = false;
            for ln in self.lines.iter_mut() {
                if ln.no == line {
                    ln.references += 1;
                    found = true;
                    break;
                }
            }

            if !found {
                self.lines.push(Line {
                    no: line,
                    references: 1,
                })
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::number_val;

    #[test]
    fn chunk_create() {
        let chunk = Chunk::new();

        assert!(chunk.code.is_empty());
        assert!(chunk.lines.is_empty());
        assert!(chunk.constants.is_empty());
    }

    #[test]
    fn chunk_write_single_byte() {
        let mut chunk = Chunk::new();

        chunk.write(42, 1);
        assert_eq!(chunk.lines.len(), 1);
        assert_eq!(
            chunk.lines.get(0),
            Some(&Line {
                no: 1,
                references: 1
            })
        );

        assert_eq!(chunk.code.len(), 1);
        assert_eq!(chunk.code.get(0), Some(&42));

        assert!(chunk.constants.is_empty());
    }

    #[test]
    fn chunk_write_constant() {
        let mut chunk = Chunk::new();

        chunk.write_constant(number_val!(1.337), 2);
        assert_eq!(chunk.lines.len(), 1);
        assert_eq!(
            chunk.lines.get(0),
            Some(&Line {
                no: 2,
                references: 2
            })
        );

        assert_eq!(chunk.code.len(), 2);
        assert_eq!(chunk.code.get(0), Some(&(OpCode::Constant as u8)));
        assert_eq!(chunk.code.get(1), Some(&0));

        assert_eq!(chunk.constants.len(), 1);
        assert_eq!(chunk.constants.get(0), Some(&number_val!(1.337)));
    }

    #[test]
    fn chunk_write_long_constant() {
        let mut chunk = Chunk::new();
        let max = 257;

        for i in 1..=max {
            chunk.write_constant(number_val!(1.337), i);
        }

        assert_eq!(chunk.code.len(), 516);
        assert_eq!(chunk.lines.len(), max as usize);
        assert_eq!(chunk.constants.len(), max as usize);
    }
}
