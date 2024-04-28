use duck_bytecode::decode;
use duck_vm::vm::vm::DuckVM;
use std::env;

fn main() {
    println!("Welcome in DuckVM version 0.0.1");
    let args: Vec<String> = env::args().collect();
    let path = &args[1];
    let bytecode = decode(path.clone());
    match bytecode {
        Ok(bytecode) => {
            let mut vm = DuckVM::new(bytecode);
            let result = vm.run();
            match result {
                Ok(_) => {}
                Err(err) => {
                    println!("{}", err);
                }
            }
        }
        Err(err) => {
            println!("{}", err);
        }
    }
}
