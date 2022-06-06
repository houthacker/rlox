use crate::chunk::LineNumber;

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

type ParseFn = fn(compiler: &mut Compiler);
struct ParseRule(Option<ParseFn>, Option<ParseFn>, Precedence);

//#[cfg_attr(feature = "rlox_debug", derive(Debug))]
pub struct Compiler<'a> {
    scanner: Scanner,
    chunk: &'a mut Chunk,
    current_token: MaybeUninit<Token>,
    previous_token: MaybeUninit<Token>,
    had_error: bool,
    panic_mode: bool,
    string_cache: Table<ObjString, Value>,
}

impl<'a> Compiler<'a> {
    pub fn new(scanner: Scanner, chunk: &'a mut Chunk) -> Self {
        Self {
            scanner,
            chunk,
            current_token: MaybeUninit::uninit(),
            previous_token: MaybeUninit::uninit(),
            had_error: false,
            panic_mode: false,
            string_cache: Table::new(),
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
        self.expression();
        self.consume(TokenType::EOF, "Expect end of expression.");

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

    fn emit_byte(&mut self, byte: u8) {
        let ln = unsafe { self.previous_token.assume_init_ref() }.line;
        self.chunk.write(byte, ln);
    }

    fn emit_bytes(&mut self, byte1: u8, byte2: u8) {
        self.emit_byte(byte1);
        self.emit_byte(byte2);
    }

    fn emit_return(&mut self) {
        self.emit_byte(OpCode::Return as u8);
    }

    fn emit_constant(&mut self, value: Value, line: LineNumber) {
        self.chunk.write_constant(value, line);
    }

    fn end_compiler(&mut self) {
        self.emit_return();

        if cfg!(feature = "rlox_debug") && !self.had_error {
            disassemble_chunk(self.chunk, "code");
        }
    }

    fn parse_precedence(&mut self, precedence: Precedence) {
        self.advance();

        // prefix rule
        match Compiler::get_rule(unsafe { self.previous_token.assume_init_ref() }.kind).0 {
            Some(parse_fn) => parse_fn(self),
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
    }

    fn expression(&mut self) {
        self.parse_precedence(Precedence::Assignment);
    }
}

fn unary(compiler: &mut Compiler) {
    let token_type = unsafe { compiler.previous_token.assume_init_ref() }.kind;
    compiler.parse_precedence(Precedence::Unary);

    match token_type {
        TokenType::Bang => compiler.emit_byte(OpCode::Not as u8),
        TokenType::Minus => compiler.emit_byte(OpCode::Negate as u8),
        _ => (),
    }
}

fn binary(compiler: &mut Compiler) {
    let operator_type = unsafe { compiler.previous_token.assume_init_ref() }.kind;
    let mut rule = Compiler::get_rule(operator_type);
    compiler.parse_precedence(rule.2.next());

    match operator_type {
        TokenType::BangEqual => compiler.emit_bytes(OpCode::Equal as u8, OpCode::Not as u8), // todo optimize
        TokenType::EqualEqual => compiler.emit_byte(OpCode::Equal as u8),
        TokenType::Greater => compiler.emit_byte(OpCode::Greater as u8),
        TokenType::GreaterEqual => compiler.emit_bytes(OpCode::Less as u8, OpCode::Not as u8),
        TokenType::Less => compiler.emit_byte(OpCode::Less as u8),
        TokenType::LessEqual => compiler.emit_bytes(OpCode::Greater as u8, OpCode::Not as u8),
        TokenType::Minus => compiler.emit_byte(OpCode::Subtract as u8),
        TokenType::Plus => compiler.emit_byte(OpCode::Add as u8),
        TokenType::Slash => compiler.emit_byte(OpCode::Divide as u8),
        TokenType::Star => compiler.emit_byte(OpCode::Multiply as u8),
        _ => (),
    }
}

fn literal(compiler: &mut Compiler) {
    match unsafe { compiler.previous_token.assume_init_ref() }.kind {
        TokenType::False => compiler.emit_byte(OpCode::False as u8),
        TokenType::Nil => compiler.emit_byte(OpCode::Nil as u8),
        TokenType::True => compiler.emit_byte(OpCode::True as u8),
        _ => (),
    }
}

fn grouping(compiler: &mut Compiler) {
    compiler.expression();
    compiler.consume(TokenType::RightParen, "Expect ')' after expression.")
}

fn number(compiler: &mut Compiler) {
    let prev = unsafe { compiler.previous_token.assume_init_ref() };
    let ln = prev.line;
    let number = prev.lexeme.parse::<f64>().unwrap();

    compiler.emit_constant(Value::from_number(number), ln);
}

fn string(compiler: &mut Compiler) {
    let prev = unsafe { compiler.previous_token.assume_init_ref() };
    let ln = prev.line;

    let slice = &prev.lexeme[1..prev.lexeme.len() - 1];
    let rlox_boxed_string = ObjString::copy_string(slice, &mut compiler.string_cache);
    let rlox_value = Value::from_obj(Obj::String(rlox_boxed_string));
    compiler.emit_constant(rlox_value, ln);
}

#[cfg(test)]
mod tests {
    use super::*;

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
}
