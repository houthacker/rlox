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
pub struct Scanner {
    source: String,
    start: usize,
    current: usize,
    line: LineNumber,
}

impl Scanner {
    pub fn new(source: String) -> Self {
        Self {
            source,
            start: 0,
            current: 0,
            line: 1,
        }
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
            "(" => self.make_token(TokenType::LeftParen),
            ")" => self.make_token(TokenType::RightParen),
            "{" => self.make_token(TokenType::LeftBrace),
            "}" => self.make_token(TokenType::RightBrace),
            ";" => self.make_token(TokenType::Semicolon),
            "," => self.make_token(TokenType::Comma),
            "." => self.make_token(TokenType::Dot),
            "-" => self.make_token(TokenType::Minus),
            "+" => self.make_token(TokenType::Plus),
            "/" => self.make_token(TokenType::Slash),
            "*" => self.make_token(TokenType::Star),
            "!" => {
                if self.match_next("=") {
                    self.make_token(TokenType::BangEqual)
                } else {
                    self.make_token(TokenType::Bang)
                }
            }
            "=" => {
                if self.match_next("=") {
                    self.make_token(TokenType::EqualEqual)
                } else {
                    self.make_token(TokenType::Equal)
                }
            }
            "<" => {
                if self.match_next("=") {
                    self.make_token(TokenType::LessEqual)
                } else {
                    self.make_token(TokenType::Less)
                }
            }
            ">" => {
                if self.match_next("=") {
                    self.make_token(TokenType::GreaterEqual)
                } else {
                    self.make_token(TokenType::Greater)
                }
            }
            "\"" => self.string(),
            _ => self.error_token("Unexpected character."),
        }
    }

    fn is_alpha(c: &str) -> bool {
        return match c.len() {
            0 => false,
            _ => {
                let first = c.as_bytes()[0];
                (b'a'..=b'z').contains(&first) || (b'A'..=b'Z').contains(&first) || first == b'_'
            }
        };
    }

    fn is_digit(c: &str) -> bool {
        return match c.len() {
            0 => false,
            _ => {
                let first = c.as_bytes()[0];
                first >= '0' as u8 && first <= '9' as u8
            }
        };
    }

    fn is_at_end(&self) -> bool {
        self.current > self.source.len()
    }

    fn advance(&mut self) -> &str {
        self.current += 1;

        if self.current + 1 > self.source.len() {
            return ""; // TODO return Option<&str>?
        }
        &self.source[self.current - 1..self.current]
    }

    fn peek(&self) -> &str {
        &self.source[self.current..=self.current]
    }

    fn peek_next(&self) -> &str {
        if self.is_at_end() {
            return ""; // TODO return Option<&str>?
        }

        &self.source[self.current + 1..=self.current + 1]
    }

    fn match_next(&mut self, expected: &str) -> bool {
        return if self.is_at_end() {
            false
        } else {
            let current = self.peek();
            if current != expected {
                return false;
            }

            self.current += 1;
            true
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
                " " | "\r" | "\t" => {
                    self.advance();
                }
                "\n" => {
                    self.line += 1;
                    self.advance();
                }
                // Technically, line comments are not whitespace, but for rlox
                // these are also discarded, so we handle them here.
                "/" => {
                    if self.peek_next() == "/" {
                        while self.peek() != "\n" && !self.is_at_end() {
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
        let lexeme = &self.source[self.start..self.current - 1];
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
                        x if x == b'u' => self.check_keyword(lexeme, 2, "un", TokenType::Fun),
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

        if self.peek() == "." && Self::is_digit(self.peek_next()) {
            // Consume the .
            self.advance();

            while Self::is_digit(self.peek()) {
                self.advance();
            }
        }

        self.make_token(TokenType::Number)
    }

    fn string(&mut self) -> Token {
        while self.peek() != "\"" && !self.is_at_end() {
            if self.peek() == "\n" {
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
