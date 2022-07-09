use crate::value::Value;
use std::fmt::{Display, Formatter};
use std::mem;

pub type InstructionIndex = usize;
pub type ConstantIndex = usize;
pub type LineNumber = u32;

/// rlox supports constant indexes up to 24 bits, so this conversion is used
/// a lot in the rlox Compiler / VM
pub trait IndexConverter {
    fn to_n_most_significant_le_bytes<const N: usize>(&self) -> Result<[u8; N], &'static str>;

    fn from_n_most_significant_le_bytes<const N: usize>(data: [u8; N]) -> Self;
}

impl IndexConverter for usize {
    /*
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
    */

    fn to_n_most_significant_le_bytes<const N: usize>(&self) -> Result<[u8; N], &'static str> {
        if N > 8 {
            return Err("N cannot be > 8");
        }

        let data: [u8; 8] = self.to_le_bytes();
        let mut sum = 0usize;
        data[N..8].iter().for_each(|b| sum += *b as usize);

        // Ensure all bytes after data[N-1] are 0, since we don't want
        // to lose any significance here. If so, we return Err
        if sum != 0 {
            Err("Cannot convert: only N most significant bits may be nonzero.")
        } else {
            let mut result = [0u8; N];
            result.copy_from_slice(&data[..N]);

            Ok(result)
        }
    }

    /*
    fn from_most_significant_le_bytes(data: [u8; 3]) -> usize {
        usize::from_le_bytes([data[0], data[1], data[2], 0, 0, 0, 0, 0])
    }
    */

    fn from_n_most_significant_le_bytes<const N: usize>(data: [u8; N]) -> Self {
        let mut le_bytes = [0u8; mem::size_of::<Self>()];
        le_bytes.copy_from_slice(&data[..]);

        Self::from_le_bytes(le_bytes)
    }
}

pub enum PatchJumpResult {
    OffsetOutOfCodeBounds,
    OffsetTooLarge,
    Ok,
}

/// bytecode instructions for the rlox VM
#[cfg_attr(feature = "rlox_debug", derive(Debug))]
#[derive(PartialEq)]
pub enum OpCode {
    Add = 0,
    Call,
    Constant,
    ConstantLong,
    DefineGlobal,
    DefineGlobalLong,
    Divide,
    Equal,
    False,
    GetGlobal,
    GetGlobalLong,
    GetLocal,
    GetLocalLong,
    Greater,
    Jump,
    JumpIfFalse,
    Less,
    Loop,
    Multiply,
    Negate,
    Nil,
    Not,
    Pop,
    PopN,
    Print,
    Return,
    SetGlobal,
    SetGlobalLong,
    SetLocal,
    SetLocalLong,
    Subtract,
    True,
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
            1 => Ok(OpCode::Call),
            2 => Ok(OpCode::Constant),
            3 => Ok(OpCode::ConstantLong),
            4 => Ok(OpCode::DefineGlobal),
            5 => Ok(OpCode::DefineGlobalLong),
            6 => Ok(OpCode::Divide),
            7 => Ok(OpCode::Equal),
            8 => Ok(OpCode::False),
            9 => Ok(OpCode::GetGlobal),
            10 => Ok(OpCode::GetGlobalLong),
            11 => Ok(OpCode::GetLocal),
            12 => Ok(OpCode::GetLocalLong),
            13 => Ok(OpCode::Greater),
            14 => Ok(OpCode::Jump),
            15 => Ok(OpCode::JumpIfFalse),
            16 => Ok(OpCode::Less),
            17 => Ok(OpCode::Loop),
            18 => Ok(OpCode::Multiply),
            19 => Ok(OpCode::Negate),
            20 => Ok(OpCode::Nil),
            21 => Ok(OpCode::Not),
            22 => Ok(OpCode::Pop),
            23 => Ok(OpCode::PopN),
            24 => Ok(OpCode::Print),
            25 => Ok(OpCode::Return),
            26 => Ok(OpCode::SetGlobal),
            27 => Ok(OpCode::SetGlobalLong),
            28 => Ok(OpCode::SetLocal),
            29 => Ok(OpCode::SetLocalLong),
            30 => Ok(OpCode::Subtract),
            31 => Ok(OpCode::True),
            _ => Err("Unknown OpCode"),
        }
    }
}

#[cfg_attr(feature = "rlox_debug", derive(Debug))]
#[derive(Copy, Clone, PartialEq)]
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
    pub fn write_constant(&mut self, value: Value, line: LineNumber) -> ConstantIndex {
        match self.add_constant(value) {
            x if x <= u8::MAX as ConstantIndex => {
                self.write(OpCode::Constant as u8, line);
                self.write(x as u8, line);
                x
            }
            x => {
                self.write(OpCode::ConstantLong as u8, line);
                match x.to_n_most_significant_le_bytes::<3>() {
                    Ok(bytes) => {
                        bytes.iter().for_each(|b| self.write(*b, line));
                        x
                    }
                    Err(_) => panic!("Too many constants"),
                }
            }
        }
    }

    pub fn read_constant(&self, index: ConstantIndex) -> Value {
        self.constants[index].clone()
    }

    pub fn patch_jump(&mut self, offset: InstructionIndex) -> PatchJumpResult {
        let len = self.code.len();

        if offset > len + 2 {
            PatchJumpResult::OffsetOutOfCodeBounds
        } else {
            // -2 to adjust for the bytecode for the jump offset itself.
            let distance = self.code.len() - offset - 2;

            if distance > u16::MAX as usize {
                PatchJumpResult::OffsetTooLarge
            } else {
                self.code[offset] = ((distance >> 8) & 0xff) as u8;
                self.code[offset + 1] = (distance & 0xff) as u8;
                PatchJumpResult::Ok
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
    pub fn get_line(&self, idx: InstructionIndex) -> Line {
        self.try_get_line(idx).unwrap()
    }

    // Adds the given value to the constant pool
    // of this chunk, and returns the index at which
    // it was added.
    pub fn add_constant(&mut self, value: Value) -> ConstantIndex {
        self.constants.push(value);
        self.constants.len() - 1
    }

    fn try_get_line(&self, idx: InstructionIndex) -> Option<Line> {
        let mut count = idx + 1; // InstructionIndex is 0-based

        for line in self.lines.iter() {
            if count <= line.references {
                return Some(*line);
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
