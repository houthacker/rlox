use crate::chunk::{InstructionIndex, InstructionIndexConverter};

#[cfg(feature = "rlox_debug")]
use crate::debug::disassemble_chunk;

use crate::object::{Obj, ObjString};
use crate::scanner::{Scanner, Token, TokenType};
use crate::table::Table;
use crate::value::Value;
use crate::{Chunk, OpCode};
use std::mem::MaybeUninit;

#[cfg_attr(feature = "rlox_debug", derive(Debug))]
#[derive(Copy, Clone, PartialEq, PartialOrd)]
enum Precedence {
    None,
    Assignment, // =
    Or,         // or
    And,        // and
    Equality,   // == !=
    Comparison, // < > <= >=
    Term,       // + -
    Factor,     // * /
    Unary,      // ! -
    Call,       // . ()
    Primary,
}

impl Precedence {
    fn try_next(&self) -> Option<Precedence> {
        let me = *self as u8;
        match Precedence::try_from(me + 1) {
            Ok(x) => Some(x),
            Err(_) => None,
        }
    }

    fn next(&mut self) -> Precedence {
        self.try_next().unwrap()
    }
}

impl TryFrom<u8> for Precedence {
    type Error = &'static str;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(Precedence::None),
            1 => Ok(Precedence::Assignment),
            2 => Ok(Precedence::Or),
            3 => Ok(Precedence::And),
            4 => Ok(Precedence::Equality),
            5 => Ok(Precedence::Comparison),
            6 => Ok(Precedence::Term),
            7 => Ok(Precedence::Factor),
            8 => Ok(Precedence::Unary),
            9 => Ok(Precedence::Call),
            10 => Ok(Precedence::Primary),
            _ => Err("Unknown Precedence"),
        }
    }
}

enum TokenPosition {
    Current,
    Previous,
}

type ParseFn = fn(compiler: &mut Compiler);
type PrefixParseFn = fn(compiler: &mut Compiler, can_assign: bool);
struct ParseRule(Option<PrefixParseFn>, Option<ParseFn>, Precedence);

#[cfg_attr(feature = "rlox_debug", derive(Debug))]
pub struct Compiler<'a> {
    scanner: Scanner,
    chunk: &'a mut Chunk,
    string_cache: Table<ObjString, Value>,
    current_token: MaybeUninit<Token>,
    previous_token: MaybeUninit<Token>,
    had_error: bool,
    panic_mode: bool,
}

impl<'a> Compiler<'a> {
    pub fn new(scanner: Scanner, chunk: &'a mut Chunk) -> Self {
        Self {
            scanner,
            chunk,
            string_cache: Table::new(),
            current_token: MaybeUninit::uninit(),
            previous_token: MaybeUninit::uninit(),
            had_error: false,
            panic_mode: false,
        }
    }

    #[rustfmt::skip]
    fn get_rule(
        token_type: TokenType,
    ) -> ParseRule {
        match token_type {
            TokenType::Bang         => ParseRule(Some(unary),   None,           Precedence::None),
            TokenType::BangEqual    => ParseRule(None,          Some(binary),   Precedence::Equality),
            TokenType::EqualEqual   => ParseRule(None,          Some(binary),   Precedence::Equality),
            TokenType::False        => ParseRule(Some(literal), None,           Precedence::None),
            TokenType::Greater      => ParseRule(None,          Some(binary),   Precedence::Comparison),
            TokenType::GreaterEqual => ParseRule(None,          Some(binary),   Precedence::Comparison),
            TokenType::Identifier   => ParseRule(Some(variable),None,           Precedence::None),
            TokenType::LeftParen    => ParseRule(Some(grouping),None,           Precedence::None),
            TokenType::Less         => ParseRule(None,          Some(binary),   Precedence::Comparison),
            TokenType::LessEqual    => ParseRule(None,          Some(binary),   Precedence::Comparison),
            TokenType::Minus        => ParseRule(Some(unary),   Some(binary),   Precedence::Term),
            TokenType::Nil          => ParseRule(Some(literal), None,           Precedence::None),
            TokenType::Number       => ParseRule(Some(number),  None,           Precedence::None),
            TokenType::Plus         => ParseRule(None,          Some(binary),   Precedence::Term),
            TokenType::Slash        => ParseRule(None,          Some(binary),   Precedence::Factor),
            TokenType::Star         => ParseRule(None,          Some(binary),   Precedence::Factor),
            TokenType::String       => ParseRule(Some(string),  None,           Precedence::None),
            TokenType::True         => ParseRule(Some(literal), None,           Precedence::None),
            _                       => ParseRule(None,          None,           Precedence::None),
        }
    }

    pub fn compile(&mut self) -> bool {
        self.advance();

        while !self.match_token_type(TokenType::EOF) {
            self.declaration();
        }

        self.end_compiler();
        !self.had_error
    }

    pub fn string_cache(&self) -> &Table<ObjString, Value> {
        &self.string_cache
    }

    // Notifies the user of an error at the current token.
    // This is a convenience method for ```self.error_at(self.parser.current.unwrap(), message)```.
    fn error_at_current(&mut self, message: &str) {
        self.error_at(unsafe { self.current_token.assume_init_read() }, message);
    }

    // Notifies the user of an error at the previous token. This is the most frequent use case,
    // so we keep the name short.
    fn error(&mut self, message: &str) {
        self.error_at(unsafe { self.previous_token.assume_init_read() }, message);
    }

    // Notifies the user of an error at the given token.
    fn error_at(&mut self, token: Token, message: &str) {
        if self.panic_mode {
            // suppress any new errors when in panic mode
            return;
        }

        self.panic_mode = true;
        eprint!("[line {}] Error", token.line);

        if token.kind == TokenType::EOF {
            eprint!(" at end")
        } else if token.kind != TokenType::Error {
            eprint!(" at '{}'", token.lexeme)
        }

        eprintln!(": {}", message);
        self.had_error = true
    }

    fn advance(&mut self) {
        self.previous_token = MaybeUninit::new(unsafe { self.current_token.assume_init_read() });

        loop {
            self.current_token = MaybeUninit::new(self.scanner.scan_token());
            let current_ref = unsafe { self.current_token.assume_init_ref() };

            if current_ref.kind != TokenType::Error {
                break;
            }

            self.error_at_current(
                unsafe { self.current_token.assume_init_read() }
                    .lexeme
                    .as_str(),
            );
        }
    }

    fn consume(&mut self, token_type: TokenType, message: &str) {
        if unsafe { self.current_token.assume_init_ref() }.kind == token_type {
            self.advance();
            return;
        }

        self.error_at_current(message);
    }

    fn check_token_type(&self, token_type: TokenType) -> bool {
        unsafe { self.current_token.assume_init_ref() }.kind == token_type
    }

    fn match_token_type(&mut self, token_type: TokenType) -> bool {
        if !self.check_token_type(token_type) {
            false
        } else {
            self.advance();
            true
        }
    }

    fn emit_opcode(&mut self, opcode: OpCode) {
        self.emit_byte(opcode as u8);
    }

    fn emit_opcodes(&mut self, opcode1: OpCode, opcode2: OpCode) {
        self.emit_opcode(opcode1);
        self.emit_opcode(opcode2);
    }

    fn emit_byte(&mut self, byte: u8) {
        let ln = unsafe { self.previous_token.assume_init_ref() }.line;
        self.chunk.write(byte, ln);
    }

    fn emit_bytes(&mut self, byte1: u8, byte2: u8) {
        self.emit_byte(byte1);
        self.emit_byte(byte2);
    }

    fn emit_return(&mut self) {
        self.emit_opcode(OpCode::Return);
    }

    fn end_compiler(&mut self) {
        self.emit_return();

        if cfg!(feature = "rlox_debug") && !self.had_error {
            disassemble_chunk(self.chunk, "code");
        }
    }

    fn parse_precedence(&mut self, precedence: Precedence) {
        self.advance();

        // To prevent assignments like a * b = c + d, determine first
        // if an assignment is allowed in this context.
        let can_assign = precedence <= Precedence::Assignment;

        // prefix rule
        match Compiler::get_rule(unsafe { self.previous_token.assume_init_ref() }.kind).0 {
            Some(parse_fn) => parse_fn(self, can_assign),
            None => self.error("Expect expression."),
        }

        while precedence
            < Compiler::get_rule(unsafe { self.current_token.assume_init_ref() }.kind).2
        {
            self.advance();

            // infix rule
            match Compiler::get_rule(unsafe { self.previous_token.assume_init_ref() }.kind).1 {
                Some(parse_fn) => parse_fn(self),
                None => { /* TODO error out? */ }
            }
        }

        if can_assign && self.match_token_type(TokenType::Equal) {
            self.error("Invalid assignment target.");
        }
    }

    fn identifier_constant(&mut self, token_position: TokenPosition) -> InstructionIndex {
        let token = match token_position {
            TokenPosition::Current => unsafe { self.current_token.assume_init_ref() },
            TokenPosition::Previous => unsafe { self.previous_token.assume_init_ref() },
        };

        let value = Value::from_obj(Obj::String(ObjString::copy_string_interned(
            &token.lexeme,
            &mut self.string_cache,
        )));

        // Just add the constant and do not emit OpCode::Constant, since
        // identifier_constant must cause the VM to add the constant value to the stack,
        // not its' related variable name.
        self.chunk.add_constant(value)
    }

    fn parse_variable(&mut self, error_message: &str) -> InstructionIndex {
        self.consume(TokenType::Identifier, error_message);

        self.identifier_constant(TokenPosition::Previous)
    }

    fn define_global_variable(&mut self, global_index: InstructionIndex) {
        if global_index <= u8::MAX as InstructionIndex {
            self.emit_bytes(OpCode::DefineGlobal as u8, global_index as u8);
        } else {
            match global_index.to_most_significant_le_bytes() {
                Ok(bytes) => {
                    self.emit_opcode(OpCode::DefineGlobalLong);
                    for byte in bytes {
                        self.emit_byte(byte);
                    }
                }
                Err(_) => {
                    // TODO log error message?
                    self.error("Too many global variables.")
                }
            }
        }
    }

    fn expression(&mut self) {
        self.parse_precedence(Precedence::Assignment);
    }

    fn var_declaration(&mut self) {
        let global_index = self.parse_variable("Expect variable name.");

        if self.match_token_type(TokenType::Equal) {
            self.expression();
        } else {
            self.emit_opcode(OpCode::Nil);
        }

        self.consume(
            TokenType::Semicolon,
            "Expect ';' after variable declaration.",
        );

        self.define_global_variable(global_index);
    }

    fn expression_statement(&mut self) {
        self.expression();
        self.consume(TokenType::Semicolon, "Expect ';' after expression");
        self.emit_opcode(OpCode::Pop);
    }

    fn print_statement(&mut self) {
        self.expression();
        self.consume(TokenType::Semicolon, "Expect ';' after value.");
        self.emit_opcode(OpCode::Print);
    }

    // Continue to retrieve tokens until a statement boundary has been reached.
    // Then try to continue compilation.
    fn synchronize(&mut self) {
        while unsafe { self.current_token.assume_init_ref() }.kind != TokenType::EOF {
            if unsafe { self.previous_token.assume_init_ref().kind == TokenType::Semicolon } {
                return;
            }

            match unsafe { self.current_token.assume_init_ref() }.kind {
                TokenType::Class
                | TokenType::Fun
                | TokenType::Var
                | TokenType::For
                | TokenType::If
                | TokenType::While
                | TokenType::Print
                | TokenType::Return => {
                    return;
                }
                _ => (),
            }

            self.advance();
        }
    }

    fn declaration(&mut self) {
        if self.match_token_type(TokenType::Var) {
            self.var_declaration();
        } else {
            self.statement();
        }

        if self.panic_mode {
            self.synchronize();
        }
    }

    fn statement(&mut self) {
        if self.match_token_type(TokenType::Print) {
            self.print_statement();
        } else {
            self.expression_statement();
        }
    }
}

fn unary(compiler: &mut Compiler, _can_assign: bool) {
    let token_type = unsafe { compiler.previous_token.assume_init_ref() }.kind;
    compiler.parse_precedence(Precedence::Unary);

    match token_type {
        TokenType::Bang => compiler.emit_opcode(OpCode::Not),
        TokenType::Minus => compiler.emit_opcode(OpCode::Negate),
        _ => (),
    }
}

fn binary(compiler: &mut Compiler) {
    let operator_type = unsafe { compiler.previous_token.assume_init_ref() }.kind;
    let mut rule = Compiler::get_rule(operator_type);
    compiler.parse_precedence(rule.2.next());

    match operator_type {
        TokenType::BangEqual => compiler.emit_opcodes(OpCode::Equal, OpCode::Not), // todo optimize?
        TokenType::EqualEqual => compiler.emit_opcode(OpCode::Equal),
        TokenType::Greater => compiler.emit_opcode(OpCode::Greater),
        TokenType::GreaterEqual => compiler.emit_opcodes(OpCode::Less, OpCode::Not),
        TokenType::Less => compiler.emit_opcode(OpCode::Less),
        TokenType::LessEqual => compiler.emit_opcodes(OpCode::Greater, OpCode::Not),
        TokenType::Minus => compiler.emit_opcode(OpCode::Subtract),
        TokenType::Plus => compiler.emit_opcode(OpCode::Add),
        TokenType::Slash => compiler.emit_opcode(OpCode::Divide),
        TokenType::Star => compiler.emit_opcode(OpCode::Multiply),
        _ => (),
    }
}

fn literal(compiler: &mut Compiler, _can_assign: bool) {
    match unsafe { compiler.previous_token.assume_init_ref() }.kind {
        TokenType::False => compiler.emit_opcode(OpCode::False),
        TokenType::Nil => compiler.emit_opcode(OpCode::Nil),
        TokenType::True => compiler.emit_opcode(OpCode::True),
        _ => (),
    }
}

fn grouping(compiler: &mut Compiler, _can_assign: bool) {
    compiler.expression();
    compiler.consume(TokenType::RightParen, "Expect ')' after expression.")
}

fn number(compiler: &mut Compiler, _can_assign: bool) {
    let prev = unsafe { compiler.previous_token.assume_init_ref() };
    let ln = prev.line;
    let number = prev.lexeme.parse::<f64>().unwrap();

    compiler
        .chunk
        .write_constant(Value::from_number(number), ln);
}

fn string(compiler: &mut Compiler, _can_assign: bool) {
    let prev = unsafe { compiler.previous_token.assume_init_ref() };
    let ln = prev.line;

    let slice = &prev.lexeme[1..prev.lexeme.len() - 1];
    let rlox_string = ObjString::copy_string_interned(slice, &mut compiler.string_cache);
    let rlox_value = Value::from_obj(Obj::String(rlox_string));
    compiler.chunk.write_constant(rlox_value, ln);
}

fn variable(compiler: &mut Compiler, can_assign: bool) {
    named_variable(compiler, can_assign);
}

fn named_variable(compiler: &mut Compiler, can_assign: bool) {
    let idx = compiler.identifier_constant(TokenPosition::Previous);
    let is_assignment = can_assign && compiler.match_token_type(TokenType::Equal);

    if idx <= u8::MAX as InstructionIndex {
        if is_assignment {
            compiler.expression();
            compiler.emit_bytes(OpCode::SetGlobal as u8, idx as u8);
        } else {
            compiler.emit_bytes(OpCode::GetGlobal as u8, idx as u8);
        }
    } else {
        if is_assignment {
            compiler.expression();
            compiler.emit_opcode(OpCode::SetGlobalLong);
        } else {
            compiler.emit_opcode(OpCode::GetGlobalLong);
        }

        match idx.to_most_significant_le_bytes() {
            Ok(bytes) => bytes.iter().for_each(|b| compiler.emit_byte(*b)),
            Err(msg) => compiler.error(msg),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn compile_with_global_variables() {
        let source = String::from("var beverage = \"cafe au lait\";\nvar breakfast = \"beignets with \" + beverage;\nprint breakfast;");
        let mut chunk = Chunk::new();
        let mut compiler = Compiler::new(Scanner::new(source), &mut chunk);

        assert!(compiler.compile());

        let expected_constants = vec![
            Value::from_obj(Obj::String(ObjString::new_interned(
                String::from("beverage"),
                &mut compiler.string_cache,
            ))),
            Value::from_obj(Obj::String(ObjString::new_interned(
                String::from("cafe au lait"),
                &mut compiler.string_cache,
            ))),
            Value::from_obj(Obj::String(ObjString::new_interned(
                String::from("breakfast"),
                &mut compiler.string_cache,
            ))),
            Value::from_obj(Obj::String(ObjString::new_interned(
                String::from("beignets with "),
                &mut compiler.string_cache,
            ))),
            Value::from_obj(Obj::String(ObjString::new_interned(
                String::from("beverage"),
                &mut compiler.string_cache,
            ))),
            Value::from_obj(Obj::String(ObjString::new_interned(
                String::from("breakfast"),
                &mut compiler.string_cache,
            ))),
        ];

        assert_eq!(expected_constants.len(), chunk.constants.len());
        expected_constants
            .iter()
            .all(|expected_element| chunk.constants.contains(expected_element));

        assert_eq!(
            chunk.code,
            vec![
                OpCode::Constant as u8,
                1u8, // stack.push('cafe au lait')
                OpCode::DefineGlobal as u8,
                0u8, // constants[0] = 'beverage', globals['beverage'] = 'cafe au lait'
                OpCode::Constant as u8,
                3u8, // constants[3] = 'beignets with '
                OpCode::GetGlobal as u8,
                4u8,               // stack.push(globals['beverage'])
                OpCode::Add as u8, // stack.push(stack.pop() + stack.pop())
                OpCode::DefineGlobal as u8,
                2u8, // constants[2] = 'breakfast', globals['breakfast'] = 'beignets with cafe au lait'
                OpCode::GetGlobal as u8,
                5u8,                 // stack.push(globals['breakfast'])
                OpCode::Print as u8, // print stack.pop() -> 'beignets with cafe au lait'
                OpCode::Return as u8,
            ]
        );
    }

    #[test]
    fn compile_numeric_binary_unary() {
        let source = String::from("!(5 - 4 > 3 * 2 == !nil)");
        let mut chunk = Chunk::new();
        let mut compiler = Compiler::new(Scanner::new(source), &mut chunk);

        assert!(compiler.compile());

        assert_eq!(
            chunk.constants,
            vec![
                Value::from_number(5f64),
                Value::from_number(4f64),
                Value::from_number(3f64),
                Value::from_number(2f64)
            ]
        );

        assert_eq!(
            chunk.code,
            vec![
                OpCode::Constant as u8,
                0u8, // constant index
                OpCode::Constant as u8,
                1u8,
                OpCode::Subtract as u8,
                OpCode::Constant as u8,
                2u8,
                OpCode::Constant as u8,
                3u8,
                OpCode::Multiply as u8,
                OpCode::Greater as u8,
                OpCode::Nil as u8,
                OpCode::Not as u8,
                OpCode::Equal as u8,
                OpCode::Not as u8,
                OpCode::Return as u8,
            ]
        );
    }

    #[test]
    fn invalid_assignment_target() {
        let source = String::from("a * b = c + d;");
        let mut chunk = Chunk::new();
        let mut compiler = Compiler::new(Scanner::new(source), &mut chunk);

        debug_assert!(!compiler.compile())
    }
}
