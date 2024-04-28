extern crate anyhow;
extern crate core;
extern crate displaydoc;
extern crate thiserror;

use crate::ast::Ast;
use crate::parser::{Diagnostic, Parser};
use crate::scanner::Scanner;

pub mod ast;
pub mod errors;
pub mod parser;
pub mod scanner;
pub mod token;

// must return the ast or the list of errors
pub fn parse(code: &str) -> Result<Ast, Diagnostic> {
    let mut scanner = Scanner::new(code);
    let tokens = scanner.tokens();
    let mut parser = Parser::new(&tokens);
    parser.parse()
}

pub fn export_ast(code: &str, destination: String) {
    println!("Saving AST into: {}", destination);
    let mut scanner = Scanner::new(code);
    let tokens = scanner.tokens();
    let mut parser = Parser::new(&tokens);
    parser.export(&destination).unwrap();
}
