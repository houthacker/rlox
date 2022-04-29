use crate::scanner::{Scanner, TokenType};

pub struct Compiler {}

impl Compiler {
    pub fn new() -> Compiler {
        Compiler {}
    }

    pub fn compile(&mut self, source: &String) {
        let mut scanner = Scanner::new(source);

        let mut line = 0;
        loop {
            let token = scanner.scan_token();
            if token.line != line {
                print!("{:4} ", token.line);
                line = token.line;
            } else {
                print!("   | ")
            }

            if cfg!(feature = "rlox_debug") {
                print!("{:2?} '{}'\n", token.kind, token.lexeme);
            }

            if token.kind == TokenType::EOF {
                break;
            }
        }
    }
}
