#[cfg_attr(feature = "rlox_debug", derive(Debug))]
#[derive(PartialEq)]
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

pub struct Token<'a> {
    pub kind: TokenType,
    pub lexeme: &'a str,
    pub line: u32,
}
pub struct Scanner<'a> {
    source: &'a str,
    start: usize,
    current: usize,
    line: u32,
}

impl Scanner<'_> {
    pub fn new(source: &String) -> Scanner {
        Scanner {
            source: &source[..],
            start: 0,
            current: 0,
            line: 1,
        }
    }

    pub fn scan_token(&mut self) -> Token {
        self.start = self.current;

        if self.is_at_end() {
            return self.make_token(TokenType::EOF);
        }

        let c = self.advance();
        if self.is_alpha(c) {
            return self.identifier();
        }
        if self.is_digit(c) {
            return self.number();
        }

        return match c {
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
            "!" => self.make_token_if(self.match_next("="), TokenType::BangEqual, TokenType::Bang),
            "=" => self.make_token_if(
                self.match_next("="),
                TokenType::EqualEqual,
                TokenType::Equal,
            ),
            "<" => self.make_token_if(self.match_next("="), TokenType::LessEqual, TokenType::Less),
            ">" => self.make_token_if(
                self.match_next("="),
                TokenType::GreaterEqual,
                TokenType::Greater,
            ),
            "\"" => self.string(),
            _ => self.error_token("Unexpected character."),
        };
    }

    fn is_alpha(&self, c: &str) -> bool {
        return match c.len() {
            0 => false,
            _ => {
                let first = c.as_bytes()[0];
                (first >= 'a' as u8 && first <= 'z' as u8)
                    || (first >= 'A' as u8 && first <= 'Z' as u8)
                    || first == '_' as u8
            }
        };
    }

    fn is_digit(&self, c: &str) -> bool {
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
        &self.source[self.current..self.current + 1]
    }

    fn peek(&self) -> &str {
        &self.source[self.current..self.current + 1]
    }

    fn peek_next(&self) -> &str {
        if self.is_at_end() {
            return ""; // TODO return Option<&str>?
        }

        &self.source[self.current + 1..self.current + 2]
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
        Token {
            kind: token_type,
            lexeme: &self.source[self.start..self.current],
            line: self.line,
        }
    }

    fn make_token_if(
        &self,
        predicate: bool,
        true_token_type: TokenType,
        false_token_type: TokenType,
    ) -> Token {
        if predicate {
            return self.make_token(true_token_type);
        }

        self.make_token(false_token_type)
    }

    fn error_token(&self, message: &'static str) -> Token {
        Token {
            kind: TokenType::Error,
            lexeme: message,
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

        return TokenType::Identifier;
    }

    fn identifier_type(&self) -> TokenType {
        // We already read the whole identifier, now we check its type.
        let lexeme = &self.source[self.start..self.current];
        let bytes = lexeme.as_bytes();

        return match bytes[0] {
            x if x == 'a' as u8 => self.check_keyword(lexeme, 1, "nd", TokenType::And),
            x if x == 'c' as u8 => self.check_keyword(lexeme, 1, "lass", TokenType::Class),
            x if x == 'e' as u8 => self.check_keyword(lexeme, 1, "lse", TokenType::Else),
            x if x == 'f' as u8 => {
                if self.current - self.start > 1 {
                    return match bytes[1] {
                        x if x == 'a' as u8 => {
                            self.check_keyword(lexeme, 2, "lse", TokenType::False)
                        }
                        x if x == 'o' as u8 => self.check_keyword(lexeme, 2, "r", TokenType::For),
                        x if x == 'u' as u8 => self.check_keyword(lexeme, 2, "un", TokenType::Fun),
                        _ => TokenType::Identifier,
                    };
                } else {
                    TokenType::Identifier
                }
            }
            x if x == 'i' as u8 => self.check_keyword(lexeme, 1, "f", TokenType::If),
            x if x == 'n' as u8 => self.check_keyword(lexeme, 1, "il", TokenType::Nil),
            x if x == 'o' as u8 => self.check_keyword(lexeme, 1, "r", TokenType::Or),
            x if x == 'p' as u8 => self.check_keyword(lexeme, 1, "rint", TokenType::Print),
            x if x == 'r' as u8 => self.check_keyword(lexeme, 1, "eturn", TokenType::Return),
            x if x == 's' as u8 => self.check_keyword(lexeme, 1, "uper", TokenType::Super),
            x if x == 't' as u8 => {
                if self.current - self.start > 1 {
                    return match bytes[1] {
                        x if x == 'h' as u8 => self.check_keyword(lexeme, 2, "is", TokenType::This),
                        x if x == 'r' as u8 => self.check_keyword(lexeme, 2, "ue", TokenType::True),
                        _ => TokenType::Identifier,
                    };
                } else {
                    TokenType::Identifier
                }
            }
            x if x == 'v' as u8 => self.check_keyword(lexeme, 1, "ar", TokenType::Var),
            x if x == 'w' as u8 => self.check_keyword(lexeme, 1, "hile", TokenType::While),
            _ => TokenType::Identifier,
        };
    }

    fn identifier(&mut self) -> Token {
        while self.is_alpha(self.peek()) || self.is_digit(self.peek()) {
            self.advance();
        }

        self.make_token(self.identifier_type())
    }

    fn number(&mut self) -> Token {
        while self.is_digit(self.peek()) {
            self.advance();
        }

        if self.peek() == "." && self.is_digit(self.peek_next()) {
            // Consume the .
            self.advance();

            while self.is_digit(self.peek()) {
                self.advance();
            }
        }

        return self.make_token(TokenType::Number);
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
