extern crate clap;

use clap::arg;
use clap::command;
use clap::Parser;
use duck_compiler::compile;
use duck_compiler::save_ast;
use duck_vm::vm::vm::VMOptions;
use duck_vm::vm::vm::VM;
use std::fs;
use std::process::exit;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// Path of the source to execute
    #[arg(short, long)]
    input: String,

    /// Path of the output of compiled AST
    #[arg(short, long)]
    output: Option<String>,

    /// Disables the evaluation of the Contracts
    #[arg(short, long, action=clap::ArgAction::SetFalse, default_value_t = true)]
    contracts_eval: bool,

    /// Enables the printing of debug traces to standard output
    #[arg(short, long, default_value_t = false)]
    trace: bool,
}

fn main() {
    let args = Args::parse();
    let input_path = args.input.clone();
    let code = fs::read_to_string(input_path).expect("Cannot open file");
    let options = build_options(&args);
    match args.output {
        Some(path) => export_script(code.as_str(), path.clone()),
        None => run_script(code.as_str(), options),
    }
}

fn build_options(args: &Args) -> VMOptions {
    let mut options = VMOptions::default();
    options.contracts_eval = args.contracts_eval;
    options.trace = args.trace;
    options
}

pub fn export_script(code: &str, destination: String) {
    save_ast(code, destination);
}

pub fn run_script(code: &str, options: VMOptions) {
    let instance = compile(code);
    match instance {
        Ok(instance) => {
            let mut vm = VM::new(instance);
            vm.options = options;
            let result = vm.run();
            match result {
                Ok(_) => {
                    if vm.options.trace {
                        vm.print();
                    }
                }
                Err(e) => {
                    println!("Duck Error: {}", e);
                    if vm.options.trace {
                        vm.print();
                    }
                    exit(255);
                }
            }
        }
        Err(_) => {
            exit(255);
        }
    }
}
