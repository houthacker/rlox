use crate::chunk::LineNumber;
use crate::debug::disassemble_chunk;
use crate::object::ObjString;
use crate::scanner::{Scanner, Token, TokenType};
use crate::value::Value;
use crate::{Chunk, OpCode};
use std::ptr::NonNull;

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

type ParseFn = for<'r> fn(&'r mut Compiler);
struct ParseRule(Option<ParseFn>, Option<ParseFn>, Precedence);

#[cfg_attr(feature = "rlox_debug", derive(Debug))]
struct Parser {
    current: Option<Token>,
    previous: Option<Token>,
    had_error: bool,
    panic_mode: bool,
}

impl Parser {
    fn new() -> Self {
        Self {
            current: None,
            previous: None,
            had_error: false,
            panic_mode: false,
        }
    }
}

#[cfg_attr(feature = "rlox_debug", derive(Debug))]
pub struct Compiler {
    parser: Parser,
    scanner: Option<Scanner>,
    compiling_chunk: NonNull<Chunk>,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            parser: Parser::new(),
            scanner: None,
            compiling_chunk: NonNull::dangling(),
        }
    }

    #[rustfmt::skip]
    fn get_rule(
        token_type: TokenType,
    ) -> ParseRule {
        match token_type {
            TokenType::Bang         => ParseRule(Some(Self::unary),         None,                   Precedence::None),
            TokenType::BangEqual    => ParseRule(None,                      Some(Self::binary),     Precedence::Equality),
            TokenType::EqualEqual   => ParseRule(None,                      Some(Self::binary),     Precedence::Equality),
            TokenType::False        => ParseRule(Some(Self::literal),       None,                   Precedence::None),
            TokenType::Greater      => ParseRule(None,                      Some(Self::binary),     Precedence::Comparison),
            TokenType::GreaterEqual => ParseRule(None,                      Some(Self::binary),     Precedence::Comparison),
            TokenType::LeftParen    => ParseRule(Some(Self::grouping),      None,                   Precedence::None),
            TokenType::Less         => ParseRule(None,                      Some(Self::binary),     Precedence::Comparison),
            TokenType::LessEqual    => ParseRule(None,                      Some(Self::binary),     Precedence::Comparison),
            TokenType::Minus        => ParseRule(Some(Self::unary),         Some(Self::binary),     Precedence::Term),
            TokenType::Nil          => ParseRule(Some(Self::literal),       None,                   Precedence::None),
            TokenType::Number       => ParseRule(Some(Self::number),        None,                   Precedence::None),
            TokenType::Plus         => ParseRule(None,                      Some(Self::binary),     Precedence::Term),
            TokenType::Slash        => ParseRule(None,                      Some(Self::binary),     Precedence::Factor),
            TokenType::Star         => ParseRule(None,                      Some(Self::binary),     Precedence::Factor),
            TokenType::String       => ParseRule(Some(Self::string), None, Precedence::None),
            TokenType::True         => ParseRule(Some(Self::literal),       None,                   Precedence::None),
            _                       => ParseRule(None,                      None,                   Precedence::None),
        }
    }

    pub fn compile(&mut self, source: String, chunk: &mut Chunk) -> bool {
        self.scanner = Some(Scanner::new(source));
        self.compiling_chunk = NonNull::from(chunk);

        self.advance();
        self.expression();
        self.consume(TokenType::EOF, "Expect end of expression.");

        self.end_compiler();
        !self.parser.had_error
    }

    fn current_chunk(&mut self) -> &mut Chunk {
        unsafe { self.compiling_chunk.as_mut() }
    }

    // Notifies the user of an error at the current token.
    // This is a convenience method for ```self.error_at(self.parser.current.unwrap(), message)```.
    fn error_at_current(&mut self, message: &str) {
        let token = self.borrow_current().clone();

        self.error_at(token, message);
    }

    // Notifies the user of an error at the previous token. This is the most frequent use case,
    // so we keep the name short.
    fn error(&mut self, message: &str) {
        self.error_at(self.borrow_previous().clone(), message);
    }

    // Notifies the user of an error at the given token.
    fn error_at(&mut self, token: Token, message: &str) {
        if self.parser.panic_mode {
            // suppress any new errors when in panic mode
            return;
        }

        self.parser.panic_mode = true;
        eprint!("[line {}] Error", token.line);

        if token.kind == TokenType::EOF {
            eprint!(" at end")
        } else if token.kind != TokenType::Error {
            eprint!(" at '{}'", token.lexeme)
        }

        eprintln!(": {}", message);
        self.parser.had_error = true
    }

    fn advance(&mut self) {
        self.parser.previous = self.parser.current.clone();

        loop {
            let token = self.scanner.as_mut().unwrap().scan_token();
            let kind = token.kind;
            self.parser.current = Some(token);
            if kind != TokenType::Error {
                break;
            }

            let msg = self.borrow_current().clone().lexeme;
            self.error_at_current(&msg);
        }
    }

    fn consume(&mut self, token_type: TokenType, message: &str) {
        if self.borrow_current().kind == token_type {
            self.advance();
            return;
        }

        self.error_at_current(message);
    }

    fn emit_byte(&mut self, byte: u8) {
        let ln = self.borrow_previous().line;
        self.current_chunk().write(byte, ln);
    }

    fn emit_bytes(&mut self, byte1: u8, byte2: u8) {
        self.emit_byte(byte1);
        self.emit_byte(byte2);
    }

    fn emit_return(&mut self) {
        self.emit_byte(OpCode::Return as u8);
    }

    fn emit_constant(&mut self, value: Value, line: LineNumber) {
        self.current_chunk().write_constant(value, line);
    }

    fn end_compiler(&mut self) {
        self.emit_return();

        if cfg!(feature = "rlox_debug") && !self.parser.had_error {
            disassemble_chunk(self.current_chunk(), "code");
        }
    }

    fn binary(&mut self) {
        let operator_type = self.borrow_previous().kind;
        let mut rule = Compiler::get_rule(operator_type);
        self.parse_precedence(rule.2.next());

        match operator_type {
            TokenType::BangEqual => self.emit_bytes(OpCode::Equal as u8, OpCode::Not as u8), // todo optimize
            TokenType::EqualEqual => self.emit_byte(OpCode::Equal as u8),
            TokenType::Greater => self.emit_byte(OpCode::Greater as u8),
            TokenType::GreaterEqual => self.emit_bytes(OpCode::Less as u8, OpCode::Not as u8),
            TokenType::Less => self.emit_byte(OpCode::Less as u8),
            TokenType::LessEqual => self.emit_bytes(OpCode::Greater as u8, OpCode::Not as u8),
            TokenType::Minus => self.emit_byte(OpCode::Subtract as u8),
            TokenType::Plus => self.emit_byte(OpCode::Add as u8),
            TokenType::Slash => self.emit_byte(OpCode::Divide as u8),
            TokenType::Star => self.emit_byte(OpCode::Multiply as u8),
            _ => (),
        }
    }

    fn literal(&mut self) {
        match self.borrow_previous().kind {
            TokenType::False => self.emit_byte(OpCode::False as u8),
            TokenType::Nil => self.emit_byte(OpCode::Nil as u8),
            TokenType::True => self.emit_byte(OpCode::True as u8),
            _ => (),
        }
    }

    fn grouping(&mut self) {
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after expression.")
    }

    fn number(&mut self) {
        let token = self.borrow_previous();
        let ln = token.line;
        let number = token.lexeme.parse::<f64>().unwrap();

        self.emit_constant(Value::from_number(number), ln);
    }

    fn string(&mut self) {
        let prev = self.borrow_previous();
        let ln = prev.line;

        let rlox_boxed_string = ObjString::copy_string(&prev.lexeme[1..prev.lexeme.len() - 1]);
        let rlox_value = Value::from_obj(Box::new(rlox_boxed_string));
        self.emit_constant(rlox_value, ln);
    }

    fn unary(&mut self) {
        let token_type = self.borrow_previous().kind;

        self.parse_precedence(Precedence::Unary);

        match token_type {
            TokenType::Bang => self.emit_byte(OpCode::Not as u8),
            TokenType::Minus => self.emit_byte(OpCode::Negate as u8),
            _ => (),
        }
    }

    fn parse_precedence(&mut self, precedence: Precedence) {
        self.advance();

        // prefix rule
        match Compiler::get_rule(self.borrow_previous().kind).0 {
            Some(parse_fn) => parse_fn(self),
            None => self.error("Expect expression."),
        }

        while precedence < Compiler::get_rule(self.borrow_current().kind).2 {
            self.advance();

            // infix rule
            match Compiler::get_rule(self.borrow_previous().kind).1 {
                Some(parse_fn) => parse_fn(self),
                None => { /* TODO error out? */ }
            }
        }
    }

    fn expression(&mut self) {
        self.parse_precedence(Precedence::Assignment);
    }

    fn borrow_current(&self) -> &Token {
        self.parser.current.as_ref().unwrap()
    }

    fn borrow_previous(&self) -> &Token {
        self.parser.previous.as_ref().unwrap()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn compile_numeric_binary_unary() {
        let mut compiler = Compiler::new();
        let source = String::from("!(5 - 4 > 3 * 2 == !nil)");
        let mut chunk = Chunk::new();

        assert!(compiler.compile(source, &mut chunk));

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
