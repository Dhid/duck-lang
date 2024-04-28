#![allow(dead_code)]
extern crate anyhow;
extern crate displaydoc;
extern crate thiserror;
use std::process::exit;

mod compiler;
mod errors;
mod locals;

use crate::compiler::Compiler;
use compiler::compile_ast;
use duck_bytecode::bytecode::Instance;
use duck_syntax::parser::Diagnostic;
use duck_syntax::{export_ast, parse};

pub fn compile(code: &str) -> Result<Instance, Diagnostic> {
    let ast = parse(code);
    match ast {
        Ok(ast) => {
            // println!("AST: {:?}", ast);
            let mut compiler = Compiler::new(&ast);
            let compilation_result = compile_ast(&mut compiler);
            match compilation_result {
                Ok(_) => Ok(compiler.instance),
                Err(err) => {
                    println!("{}", err);
                    exit(255);
                }
            }
        }
        Err(errors) => {
            for err in errors.iter() {
                println!("{}", err);
            }
            Err(errors.clone())
        }
    }
}

pub fn save_ast(code: &str, destination: String) {
    export_ast(code, destination);
}
