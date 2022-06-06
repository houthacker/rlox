#[cfg(not(target_env = "msvc"))]
use tikv_jemallocator::Jemalloc;

#[cfg(not(target_env = "msvc"))]
#[global_allocator]
static GLOBAL: Jemalloc = Jemalloc;

extern crate alloc;
extern crate core;

mod chunk;
mod compiler;
mod object;
mod scanner;
mod value;
mod vm;

#[cfg(feature = "rlox_debug")]
mod debug;

mod stack;
mod table;

use chunk::{Chunk, OpCode};
use compiler::Compiler;
use std::env;
use std::fs;
use std::io;
use vm::{InterpretResult, VM};

fn main() {
    let args: Vec<String> = env::args().collect();
    let argc = args.len();
    if argc == 1 {
        repl();
        println!("Done");
    } else if argc == 2 {
        run_file(&args[1]);
    } else {
        eprintln!("Usage: rlox [path]");
        std::process::exit(64);
    }
}

fn repl() {
    loop {
        let mut line = String::new();
        match io::stdin().read_line(&mut line) {
            Ok(_) => match VM::interpret(line) {
                InterpretResult::CompileError => {
                    eprintln!("Compilation failed.")
                }
                InterpretResult::RuntimeError => {
                    eprintln!("Runtime Error.")
                }
                InterpretResult::Ok => (),
            },
            Err(error) => {
                eprintln!("Could not read from stdin: {}", error);
            }
        }
    }
}

fn run_file(path: &String) {
    match fs::read_to_string(path) {
        Ok(contents) => match VM::interpret(contents) {
            InterpretResult::CompileError => std::process::exit(65),
            InterpretResult::RuntimeError => std::process::exit(70),
            InterpretResult::Ok => (),
        },
        Err(error) => {
            eprintln!("Could not read file {}: {}", path, error);
        }
    }
}
