use crate::value::Value;
use std::fmt::{Display, Formatter};

pub type InstructionIndex = usize;
pub type LineNumber = u32;

/// rlox supports constant indexes up to 24 bits, so this conversion is used
/// a lot in the rlox Compiler / VM
pub trait InstructionIndexConverter {
    fn to_most_significant_le_bytes(&self) -> Result<[u8; 3], &'static str>;

    fn from_most_significant_le_bytes(data: [u8; 3]) -> InstructionIndex;
}

impl InstructionIndexConverter for InstructionIndex {
    fn to_most_significant_le_bytes(&self) -> Result<[u8; 3], &'static str> {
        let data = self.to_le_bytes();

        // Ensure all bytes after data[2] are 0, since we don't want
        // to lose any significance here. If so, we return Err
        if data[0] + data[1] + data[2] != data.iter().sum() {
            Err("Cannot convert: only 24 most significant bits may be nonzero.")
        } else {
            Ok([data[0], data[1], data[2]])
        }
    }

    fn from_most_significant_le_bytes(data: [u8; 3]) -> InstructionIndex {
        InstructionIndex::from_le_bytes([data[0], data[1], data[2], 0, 0, 0, 0, 0])
    }
}

/// bytecode instructions for the rlox VM
#[cfg_attr(feature = "rlox_debug", derive(Debug))]
#[derive(PartialEq)]
pub enum OpCode {
    Add = 0,
    Constant = 1,
    ConstantLong = 2,
    DefineGlobal = 3,
    DefineGlobalLong = 4,
    Divide = 5,
    Equal = 6,
    False = 7,
    GetGlobal = 8,
    GetGlobalLong = 9,
    GetLocal = 10,
    GetLocalLong = 11,
    Greater = 12,
    Less = 13,
    Multiply = 14,
    Negate = 15,
    Nil = 16,
    Not = 17,
    Pop = 18,
    PopN = 19,
    Print = 20,
    Return = 21,
    SetGlobal = 22,
    SetGlobalLong = 23,
    SetLocal = 24,
    SetLocalLong = 25,
    Subtract = 26,
    True = 27,
}

impl OpCode {
    pub fn is_long_discriminant(&self) -> bool {
        use OpCode::*;

        matches!(
            self,
            ConstantLong
                | DefineGlobalLong
                | GetGlobalLong
                | GetLocalLong
                | SetGlobalLong
                | SetLocalLong
        )
    }
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
            3 => Ok(OpCode::DefineGlobal),
            4 => Ok(OpCode::DefineGlobalLong),
            5 => Ok(OpCode::Divide),
            6 => Ok(OpCode::Equal),
            7 => Ok(OpCode::False),
            8 => Ok(OpCode::GetGlobal),
            9 => Ok(OpCode::GetGlobalLong),
            10 => Ok(OpCode::GetLocal),
            11 => Ok(OpCode::GetLocalLong),
            12 => Ok(OpCode::Greater),
            13 => Ok(OpCode::Less),
            14 => Ok(OpCode::Multiply),
            15 => Ok(OpCode::Negate),
            16 => Ok(OpCode::Nil),
            17 => Ok(OpCode::Not),
            18 => Ok(OpCode::Pop),
            19 => Ok(OpCode::PopN),
            20 => Ok(OpCode::Print),
            21 => Ok(OpCode::Return),
            22 => Ok(OpCode::SetGlobal),
            23 => Ok(OpCode::SetGlobalLong),
            24 => Ok(OpCode::SetLocal),
            25 => Ok(OpCode::SetLocalLong),
            26 => Ok(OpCode::Subtract),
            27 => Ok(OpCode::True),
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
    pub fn write_constant(&mut self, value: Value, line: LineNumber) -> InstructionIndex {
        match self.add_constant(value) {
            x if x <= u8::MAX as InstructionIndex => {
                self.write(OpCode::Constant as u8, line);
                self.write(x as u8, line);
                x
            }
            x => {
                self.write(OpCode::ConstantLong as u8, line);
                match x.to_most_significant_le_bytes() {
                    Ok(bytes) => {
                        bytes.iter().for_each(|b| self.write(*b, line));
                        x
                    }
                    Err(_) => panic!("Too many constants"),
                }
            }
        }
    }

    pub fn read_constant(&self, index: InstructionIndex) -> Value {
        self.constants[index].clone()
    }

    /// Returns the source code line at which the given instruction
    /// resides.
    ///
    /// # Panics
    /// * If the given InstructionIndex doesn't exist in this chunk.
    /// # Arguments
    /// * `idx` - The index of the instruction within this chunk
    pub fn get_line(&self, idx: InstructionIndex) -> Line {
        self.try_get_line(idx).unwrap()
    }

    // Adds the given value to the constant pool
    // of this chunk, and returns the index at which
    // it was added.
    pub fn add_constant(&mut self, value: Value) -> InstructionIndex {
        self.constants.push(value);
        self.constants.len() - 1
    }

    fn try_get_line(&self, idx: InstructionIndex) -> Option<Line> {
        let mut count = idx + 1; // InstructionIndex is 0-based

        for line in self.lines.iter() {
            if count <= line.references {
                return Some(line.clone());
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

        chunk.write_constant(Value::from_number(1.337), 2);
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
        assert_eq!(chunk.constants.get(0), Some(&Value::from_number(1.337)));
    }

    #[test]
    fn chunk_write_long_constant() {
        let mut chunk = Chunk::new();
        let max = 257;

        for i in 1..=max {
            chunk.write_constant(Value::from_number(1.337), i);
        }

        assert_eq!(chunk.code.len(), 516);
        assert_eq!(chunk.lines.len(), max as usize);
        assert_eq!(chunk.constants.len(), max as usize);
    }
}
