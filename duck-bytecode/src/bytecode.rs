use crate::bytecode::Instruction::{Jump, JumpIfFalse};
use std::{fmt::Display, slice::Iter};

pub type ConstantIndex = usize;
pub type InstructionIndex = usize;
pub type FunctionIndex = usize;
pub type ClosureIndex = usize;
pub type DataIndex = usize;
pub type IdentifierIndex = usize;
pub type StackIndex = usize;
pub type ChunkIndex = usize;
pub type UpvalueIndex = usize;
pub type Arity = usize;

#[derive(Debug, Clone)]
pub enum Instruction {
    True,
    False,
    Nil,
    Constant(ConstantIndex), // needed for Copy trait

    //arithmetic
    Negate,
    Sum,
    Sub,
    Div,
    Mul,

    // logical
    Not,
    Equal,
    Greater,
    Less,

    //jumps
    Jump(InstructionIndex),
    JumpIfFalse(InstructionIndex),

    Return,
    Print,
    Pop,
    Dup, // duplicates a value in the stack

    DefineGlobal(IdentifierIndex),
    GetGlobal(IdentifierIndex),
    SetGlobal(IdentifierIndex),

    GetLocal(StackIndex),
    SetLocal(StackIndex),
    GetUpvalue(UpvalueIndex),
    SetUpvalue(UpvalueIndex),
    CloseUpvalue,

    Call(Arity),        // how many params for a function call
    NewInstance(Arity), // how many params for a new Instance of Data
    Closure(ClosureIndex),
    Get(String),
    Set(String),
    String(String),

    // contracts
    Require,
    Ensure,
    CheckContract,
}

#[derive(Debug, Clone)]
pub enum Constant {
    Number(f64),
    String(String),
    Boolean(bool),
    Nil,
}

impl Display for Constant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        match self {
            Constant::Number(n) => {
                write!(f, "{}", n)
            }
            Constant::String(s) => {
                write!(f, "{}", s)
            }
            Constant::Boolean(b) => {
                write!(f, "{}", b)
            }
            Constant::Nil => {
                write!(f, "Nil")
            }
        }
    }
}

impl From<f64> for Constant {
    fn from(item: f64) -> Self {
        Constant::Number(item)
    }
}
impl From<&str> for Constant {
    fn from(item: &str) -> Self {
        Constant::String(String::from(item))
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub chunk_index: ChunkIndex,
    pub arity: usize,
}

impl Function {
    pub fn new(value: &crate::bytecode::Function) -> Self {
        Function {
            name: value.name.clone(),
            chunk_index: value.chunk_index,
            arity: value.arity,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Closure {
    pub function: Function,
    pub upvalues: Vec<Upvalue>,
}


#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ChunkInstructionType {
    Standard,
    Precondition,
    Postcondition,
}

#[derive(Debug)]
pub struct Chunk {
    instructions: Vec<Instruction>,
    pre_instructions: Vec<Instruction>,
    post_instructions: Vec<Instruction>,
}

pub struct Instance {
    pub chunks: Vec<Chunk>,
    constants: Pool<Constant>,
    pub identifiers: Pool<String>,
    closures: Pool<Closure>,
}

impl Display for Instance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        writeln!(f, "Chunks: {:?}", self.chunks).expect("Chunks not valid");
        writeln!(f, "Constants: {:?}", self.constants)
    }
}

impl Instance {
    pub fn new() -> Self {
        Instance {
            chunks: vec![],
            constants: Pool::new(),
            identifiers: Pool::new(),
            closures: Pool::new(),
        }
    }

    pub fn chunk(&self, index: usize) -> &Chunk {
        self.chunks
            .get(index)
            .expect(format!("Missing Chunk {}", index).as_str())
    }

    pub fn chunk_mut(&mut self, index: usize) -> &mut Chunk {
        self.chunks
            .get_mut(index)
            .expect(format!("Missing Chunk {}", index).as_str())
    }

    pub fn add_chunk(&mut self) -> usize {
        let chunk = Chunk::new();
        self.chunks.push(chunk);
        self.chunks.len() - 1
    }

    pub fn add_const(&mut self, constant: Constant) -> ConstantIndex {
        self.constants.add(constant)
    }

    pub fn add_closure(&mut self, closure: Closure) -> ClosureIndex {
        self.closures.add(closure)
    }

    pub fn get_const(&self, index: usize) -> &Constant {
        self.constants.value(index)
    }

    pub fn add_identifier(&mut self, identifier: String) -> IdentifierIndex {
        self.identifiers.add(identifier)
    }

    pub fn get_identifier(&self, index: usize) -> &String {
        self.identifiers.value(index)
    }

    pub fn get_closure(&self, index: usize) -> &Closure {
        self.closures.value(index)
    }

    pub fn print(&self) {
        println!("Chunks {:?}", self.chunks);
        println!();
        println!("Constants {:?}", self.constants);
        println!();
        println!("Identifiers {:?}", self.identifiers);
        println!();
        println!("Closures {:?}", self.closures);
    }
}

#[derive(Debug)]
pub struct Pool<T> {
    values: Vec<T>,
}

impl<T> Pool<T> {
    pub fn new() -> Self {
        Pool { values: vec![] }
    }

    pub fn add(&mut self, value: T) -> ConstantIndex {
        self.values.push(value);
        self.values.len() - 1
    }

    pub fn value(&self, index: usize) -> &T {
        &self.values[index]
    }

    pub fn iter(&self) -> Iter<T> {
        self.values.iter()
    }
}

impl Chunk {
    pub fn new() -> Self {
        Chunk {
            instructions: vec![],
            pre_instructions: vec![],
            post_instructions: vec![],
        }
    }

    pub fn instructions(&self, citype: ChunkInstructionType) -> &[Instruction] {
        match citype {
            ChunkInstructionType::Standard => &self.instructions,
            ChunkInstructionType::Precondition => &self.pre_instructions,
            ChunkInstructionType::Postcondition => &self.post_instructions,
        }
    }

    pub fn instructions_mut(&mut self, citype: ChunkInstructionType) -> &mut [Instruction] {
        match citype {
            ChunkInstructionType::Standard => &mut self.instructions,
            ChunkInstructionType::Precondition => &mut self.pre_instructions,
            ChunkInstructionType::Postcondition => &mut self.post_instructions,
        }
    }

    pub fn get_instructions(&self, citype: ChunkInstructionType) -> &Vec<Instruction> {
        match citype {
            ChunkInstructionType::Standard => &self.instructions,
            ChunkInstructionType::Precondition => &self.pre_instructions,
            ChunkInstructionType::Postcondition => &self.post_instructions,
        }
    }

    pub fn get_instructions_mut(&mut self, citype: ChunkInstructionType) -> &mut Vec<Instruction> {
        match citype {
            ChunkInstructionType::Standard => &mut self.instructions,
            ChunkInstructionType::Precondition => &mut self.pre_instructions,
            ChunkInstructionType::Postcondition => &mut self.post_instructions,
        }
    }

    pub fn patch_instruction_to(
        &mut self,
        index: InstructionIndex,
        to: InstructionIndex,
        citype: ChunkInstructionType,
    ) {
        match self.instructions_mut(citype)[index] {
            JumpIfFalse(ref mut placeholder) => *placeholder = to,
            Jump(ref mut placeholder) => *placeholder = to,
            _ => (),
        };
    }

    /// Returns the Instruction at the specified offset. This operation is unsafe
    #[inline]
    pub fn instr(&self, offset: usize, citype: ChunkInstructionType) -> Instruction {
        unsafe { self.instructions(citype).get_unchecked(offset).clone() }
    }

    pub fn add_std_instruction(&mut self, instruction: Instruction) -> InstructionIndex {
        self.get_instructions_mut(ChunkInstructionType::Standard)
            .push(instruction);
        self.instructions(ChunkInstructionType::Standard).len() - 1
    }

    pub fn add_instruction(
        &mut self,
        instruction: Instruction,
        citype: ChunkInstructionType,
    ) -> InstructionIndex {
        self.get_instructions_mut(citype).push(instruction);
        self.instructions(citype).len() - 1
    }

    pub fn pop_instruction(&mut self, citype: ChunkInstructionType) -> Instruction {
        let instr = self.get_instructions_mut(citype).pop();
        match instr {
            Some(instr) => instr,
            None => panic!("No instructions to pop"),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Upvalue {
    Local(StackIndex),
    Upvalue(UpvalueIndex),
}

impl Upvalue {
    pub fn index(&self) -> usize {
        match self {
            Upvalue::Local(index) => index.clone(),
            Upvalue::Upvalue(index) => index.clone(),
        }
    }
}
