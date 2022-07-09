use crate::chunk::LineNumber;

#[cfg_attr(feature = "rlox_debug", derive(Debug))]
#[derive(Copy, Clone, PartialEq)]
pub enum TokenType {
    // Single-character tokens
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One- or two character tokens
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals
    Identifier,
    String,
    Number,

    // Keywords
    And,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    // Other
    Error,
    EOF,
}

#[cfg_attr(feature = "rlox_debug", derive(Debug))]
pub struct Token {
    pub kind: TokenType,
    pub lexeme: String,
    pub line: LineNumber,
}

impl Clone for Token {
    fn clone(&self) -> Self {
        Self {
            kind: self.kind,
            lexeme: self.lexeme.clone(),
            line: self.line,
        }
    }
}

#[cfg_attr(feature = "rlox_debug", derive(Debug))]
pub struct Scanner<'a> {
    source: &'a str,
    start: usize,
    current: usize,
    line: LineNumber,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            start: 0,
            current: 0,
            line: 1,
        }
    }

    pub fn from_current_position(instance: &Self) -> Self {
        Self::new(&instance.source[instance.current..])
    }

    pub fn scan_token(&mut self) -> Token {
        self.skip_whitespace();

        self.start = self.current;

        if self.is_at_end() {
            return self.make_token(TokenType::EOF);
        }

        let c = self.advance();
        if Self::is_alpha(c) {
            return self.identifier();
        }
        if Self::is_digit(c) {
            return self.number();
        }

        match c {
            Some("(") => self.make_token(TokenType::LeftParen),
            Some(")") => self.make_token(TokenType::RightParen),
            Some("{") => self.make_token(TokenType::LeftBrace),
            Some("}") => self.make_token(TokenType::RightBrace),
            Some(";") => self.make_token(TokenType::Semicolon),
            Some(",") => self.make_token(TokenType::Comma),
            Some(".") => self.make_token(TokenType::Dot),
            Some("-") => self.make_token(TokenType::Minus),
            Some("+") => self.make_token(TokenType::Plus),
            Some("/") => self.make_token(TokenType::Slash),
            Some("*") => self.make_token(TokenType::Star),
            Some("!") => {
                if self.match_next("=") {
                    self.make_token(TokenType::BangEqual)
                } else {
                    self.make_token(TokenType::Bang)
                }
            }
            Some("=") => {
                if self.match_next("=") {
                    self.make_token(TokenType::EqualEqual)
                } else {
                    self.make_token(TokenType::Equal)
                }
            }
            Some("<") => {
                if self.match_next("=") {
                    self.make_token(TokenType::LessEqual)
                } else {
                    self.make_token(TokenType::Less)
                }
            }
            Some(">") => {
                if self.match_next("=") {
                    self.make_token(TokenType::GreaterEqual)
                } else {
                    self.make_token(TokenType::Greater)
                }
            }
            Some("\"") => self.string(),
            _ => self.error_token("Unexpected character."),
        }
    }

    fn is_alpha(c: Option<&str>) -> bool {
        return match c {
            None => false,
            Some(s) => match s.len() {
                0 => false,
                _ => {
                    let first = s.as_bytes()[0];
                    (b'a'..=b'z').contains(&first)
                        || (b'A'..=b'Z').contains(&first)
                        || first == b'_'
                }
            },
        };
    }

    fn is_digit(c: Option<&str>) -> bool {
        return match c {
            None => false,
            Some(s) => match s.len() {
                0 => false,
                _ => {
                    let first = s.as_bytes()[0];
                    (b'0'..=b'9').contains(&first)
                }
            },
        };
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn advance(&mut self) -> Option<&str> {
        self.current += 1;

        if self.current > self.source.len() {
            return None;
        }

        Some(&self.source[self.current - 1..self.current])
    }

    fn peek(&self) -> Option<&str> {
        if self.current >= self.source.len() {
            return None;
        }

        Some(&self.source[self.current..=self.current])
    }

    fn peek_next(&self) -> Option<&str> {
        if self.is_at_end() {
            return None;
        }

        Some(&self.source[self.current + 1..=self.current + 1])
    }

    fn match_next(&mut self, expected: &str) -> bool {
        return if self.is_at_end() {
            false
        } else {
            return match self.peek() {
                Some(x) => {
                    return if x != expected {
                        false
                    } else {
                        self.current += 1;
                        true
                    }
                }
                None => false,
            };
        };
    }

    fn make_token(&self, token_type: TokenType) -> Token {
        match token_type {
            TokenType::EOF => Token {
                kind: token_type,
                lexeme: String::new(),
                line: self.line,
            },
            _ => Token {
                kind: token_type,
                lexeme: (&self.source[self.start..self.current]).to_owned(),
                line: self.line,
            },
        }
    }

    fn error_token(&self, message: &'static str) -> Token {
        Token {
            kind: TokenType::Error,
            lexeme: String::from(message),
            line: self.line,
        }
    }

    fn skip_whitespace(&mut self) {
        loop {
            match self.peek() {
                Some(" ") | Some("\r") | Some("\t") => {
                    self.advance();
                }
                Some("\n") => {
                    self.line += 1;
                    self.advance();
                }
                // Technically, line comments are not whitespace, but for rlox
                // these are also discarded, so we handle them here.
                Some("/") => {
                    if self.peek_next() == Some("/") {
                        while self.peek() != Some("\n") && !self.is_at_end() {
                            self.advance();
                        }
                    } else {
                        return;
                    }
                }
                _ => return,
            }
        }
    }

    fn check_keyword(
        &self,
        source: &str,
        start: usize,
        rest: &str,
        token_type: TokenType,
    ) -> TokenType {
        if &source[start..] == rest {
            return token_type;
        }

        TokenType::Identifier
    }

    fn identifier_type(&self) -> TokenType {
        // We already read the whole identifier, now we check its type.
        let lexeme = &self.source[self.start..self.current];
        let bytes = lexeme.as_bytes();

        match bytes[0] {
            x if x == b'a' => self.check_keyword(lexeme, 1, "nd", TokenType::And),
            x if x == b'c' => self.check_keyword(lexeme, 1, "lass", TokenType::Class),
            x if x == b'e' => self.check_keyword(lexeme, 1, "lse", TokenType::Else),
            x if x == b'f' => {
                if self.current - self.start > 1 {
                    match bytes[1] {
                        x if x == b'a' => self.check_keyword(lexeme, 2, "lse", TokenType::False),
                        x if x == b'o' => self.check_keyword(lexeme, 2, "r", TokenType::For),
                        x if x == b'u' => self.check_keyword(lexeme, 2, "n", TokenType::Fun),
                        _ => TokenType::Identifier,
                    }
                } else {
                    TokenType::Identifier
                }
            }
            x if x == b'i' => self.check_keyword(lexeme, 1, "f", TokenType::If),
            x if x == b'n' => self.check_keyword(lexeme, 1, "il", TokenType::Nil),
            x if x == b'o' => self.check_keyword(lexeme, 1, "r", TokenType::Or),
            x if x == b'p' => self.check_keyword(lexeme, 1, "rint", TokenType::Print),
            x if x == b'r' => self.check_keyword(lexeme, 1, "eturn", TokenType::Return),
            x if x == b's' => self.check_keyword(lexeme, 1, "uper", TokenType::Super),
            x if x == b't' => {
                if self.current - self.start > 1 {
                    match bytes[1] {
                        x if x == b'h' => self.check_keyword(lexeme, 2, "is", TokenType::This),
                        x if x == b'r' => self.check_keyword(lexeme, 2, "ue", TokenType::True),
                        _ => TokenType::Identifier,
                    }
                } else {
                    TokenType::Identifier
                }
            }
            x if x == b'v' => self.check_keyword(lexeme, 1, "ar", TokenType::Var),
            x if x == b'w' => self.check_keyword(lexeme, 1, "hile", TokenType::While),
            _ => TokenType::Identifier,
        }
    }

    fn identifier(&mut self) -> Token {
        while Self::is_alpha(self.peek()) || Self::is_digit(self.peek()) {
            self.advance();
        }

        self.make_token(self.identifier_type())
    }

    fn number(&mut self) -> Token {
        while Self::is_digit(self.peek()) {
            self.advance();
        }

        if self.peek() == Some(".") && Self::is_digit(self.peek_next()) {
            // Consume the .
            self.advance();

            while Self::is_digit(self.peek()) {
                self.advance();
            }
        }

        self.make_token(TokenType::Number)
    }

    fn string(&mut self) -> Token {
        while self.peek() != Some("\"") && !self.is_at_end() {
            if self.peek() == Some("\n") {
                self.line += 1;
            }
            self.advance();
        }

        if self.is_at_end() {
            return self.error_token("Unterminated string.");
        }

        // Consume the closing quote
        self.advance();

        self.make_token(TokenType::String)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn scan_numeric_binary_unary() {
        let source = String::from("!(5 - 4 > 3 * 2 == !nil)");
        let mut scanner = Scanner::new(&source);

        let mut token = scanner.scan_token();
        assert_eq!(token.kind, TokenType::Bang);

        token = scanner.scan_token();
        assert_eq!(token.kind, TokenType::LeftParen);

        token = scanner.scan_token();
        assert_eq!(token.kind, TokenType::Number);

        token = scanner.scan_token();
        assert_eq!(token.kind, TokenType::Minus);

        token = scanner.scan_token();
        assert_eq!(token.kind, TokenType::Number);

        token = scanner.scan_token();
        assert_eq!(token.kind, TokenType::Greater);

        token = scanner.scan_token();
        assert_eq!(token.kind, TokenType::Number);

        token = scanner.scan_token();
        assert_eq!(token.kind, TokenType::Star);

        token = scanner.scan_token();
        assert_eq!(token.kind, TokenType::Number);

        token = scanner.scan_token();
        assert_eq!(token.kind, TokenType::EqualEqual);

        token = scanner.scan_token();
        assert_eq!(token.kind, TokenType::Bang);

        token = scanner.scan_token();
        assert_eq!(token.kind, TokenType::Nil);

        token = scanner.scan_token();
        assert_eq!(token.kind, TokenType::RightParen);

        token = scanner.scan_token();
        assert_eq!(token.kind, TokenType::EOF);
    }
}
