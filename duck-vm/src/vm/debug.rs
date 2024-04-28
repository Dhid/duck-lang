use duck_bytecode::bytecode::{ConstantIndex, Instruction};

pub fn disassemble_instruction(instr: &Instruction) {
    match instr {
        Instruction::Constant(index) => {
            simple_const(index);
        }
        _ => {
            simple_instr(instr);
        }
    }
}

pub fn simple_instr(instruction: &Instruction) {
    println!("Instruction: {:?}", instruction);
}

pub fn simple_const(constant: &ConstantIndex) {
    println!("Constant {:?}", constant);
}
