use crate::vm::vm::VM;
use duck_std::*;

pub mod duck_std;

pub fn setup_stdlib(vm: &mut VM) {
    vm.add_native("quack".to_string(), 0, quack);
    vm.add_native("append_string".to_string(), 2, append_string);
}
