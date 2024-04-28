use crate::stdlib::setup_stdlib;
use crate::vm::debug::disassemble_instruction;
use crate::vm::memory::{Closure, Value};
use anyhow::Result;
use duck_bytecode::bytecode::{
    self, ChunkInstructionType, Constant, Function, Instance, Instruction,
};
use std::cell::Cell;

use super::gc::{Gc, Heap};
use super::memory::DuckFn;
use super::module::Module;
use super::native::{Native, NativeCall, NativeFunction, NativeValue};
use super::{errors::VMError, memory::Upvalue};

const MAX_STACK_SIZE: usize = 65536;
const MAX_FRAME_SIZE: usize = 65536;

type VMResult = Result<InterpreterResult, VMError>;

#[derive(Debug, Default)]
pub struct VMOptions {
    pub trace: bool,
    pub contracts_eval: bool,
}

#[derive(Debug, Copy, Clone)]
pub enum InterpreterResult {
    Ok(Option<Value>),
    CompileError,
    RuntimeError,
}
#[derive(Debug)]
struct CallFrameChunk {
    pub chunk_type: ChunkInstructionType,
    pub pc: usize,
    pub base_counter: usize,
}

#[derive(Debug)]
pub struct CallFrame {
    closure: Gc<Closure>,
    frame_chunk: Option<CallFrameChunk>,
    pc: usize,
    base_counter: usize,
}

impl CallFrame {
    pub fn move_to_chunk(&mut self, citype: ChunkInstructionType, base_counter: usize) {
        self.frame_chunk = Some(CallFrameChunk {
            chunk_type: citype,
            pc: 0,
            base_counter,
        })
    }

    pub fn clear_chunk(&mut self) {
        self.frame_chunk = None;
    }

    pub fn current_chunk_instruction_type(&self) -> Option<ChunkInstructionType> {
        match &self.frame_chunk {
            Some(fc) => Some(fc.chunk_type),
            None => None,
        }
    }

    pub fn arity(&self) -> usize {
        self.closure.function.arity
    }
}

pub struct VM {
    pub instance: Instance,
    pub stack: Vec<Value>,
    pub module: Module,
    pub frames: Vec<CallFrame>,
    pub heap: Heap,
    pub options: VMOptions,
}

impl VM {
    pub fn new(instance: Instance) -> Self {
        let mut heap = Heap::new();
        let runtime_closure = heap.root(Closure {
            function: Function {
                name: "duck-runtime".to_string(),
                chunk_index: 0,
                arity: 0,
            },
            upvalues: vec![],
        });

        VM {
            instance,
            stack: vec![],
            // set trace as RUST_BACKTRACE
            module: Module::new("duck-runtime".to_string()),
            frames: vec![CallFrame {
                pc: 0,
                base_counter: 0,
                closure: runtime_closure,
                frame_chunk: None,
            }],
            heap,
            options: VMOptions::default(),
        }
    }

    fn top(&mut self) -> &mut Value {
        &mut self.stack[0]
    }

    fn peek(&self) -> Result<Value, VMError> {
        let value = self.stack.last();
        match value {
            Some(value) => Ok(*value),
            None => Err(VMError::StackEmpty { pc: self.pc() }),
        }
    }

    fn peek_value(&self) -> Option<Value> {
        match self.stack.last() {
            Some(val) => Some(*val),
            None => None,
        }
    }

    pub fn add_native(&mut self, identifier: String, arity: usize, fun: NativeFunction) {
        let native = Native::new(arity, fun);
        let allocation = self.heap.root(native);
        self.module.add_global(
            identifier.clone(),
            Value::Func(DuckFn::Native { func: allocation }),
        );
    }

    pub fn pop(&mut self) -> Result<Value, VMError> {
        let val = self.stack.pop();
        match val {
            Some(val) => {
                match val {
                    Value::String(s) => s.allocation().unroot(),
                    Value::Instance(i) => i.allocation().unroot(),
                    Value::Func(f) => match f {
                        DuckFn::Script { func } => func.allocation().unroot(),
                        DuckFn::Native { func } => func.allocation().unroot(),
                    },
                    _ => {}
                }
                Ok(val)
            }
            None => Err(VMError::StackEmpty { pc: self.pc() }),
        }
    }

    fn add_frame(&mut self, frame: CallFrame) {
        self.frames.push(frame);
    }

    fn frame(&self) -> &CallFrame {
        &self.frames.last().expect("No CallFrames found")
    }

    fn frame_mut(&mut self) -> &mut CallFrame {
        let index = self.frames.len() - 1;
        &mut self.frames[index]
    }

    fn set_pc(&mut self, pc: usize) -> usize {
        let frame = self.frame_mut();
        match &mut frame.frame_chunk {
            Some(frame_chunk) => frame_chunk.pc = pc,
            None => frame.pc = pc,
        }
        self.pc()
    }

    fn pc(&self) -> usize {
        let frame = self.frame();
        match &frame.frame_chunk {
            Some(frame_chunk) => frame_chunk.pc,
            None => self.frame().pc,
        }
    }

    fn base_counter(&self) -> usize {
        let frame = self.frame();
        match &frame.frame_chunk {
            Some(frame_chunk) => frame_chunk.base_counter,
            None => self.frame().base_counter,
        }
    }

    fn next_instr(&mut self) -> Option<Instruction> {
        let frame = self.frame();
        let chunk_index = frame.closure.function.chunk_index;
        let chunk = self.instance.chunk(chunk_index);
        match &frame.frame_chunk {
            Some(frame_chunk) => {
                let citype = frame_chunk.chunk_type;
                if frame_chunk.pc >= chunk.instructions(citype).len() {
                    return None;
                }
                let instr = chunk.instr(frame_chunk.pc, citype);
                self.set_pc(frame_chunk.pc + 1);

                Some(instr)
            }
            None => {
                if frame.pc >= chunk.instructions(ChunkInstructionType::Standard).len() {
                    return None;
                }
                let instr = chunk.instr(frame.pc, ChunkInstructionType::Standard);
                self.set_pc(frame.pc + 1);
                Some(instr)
            }
        }
    }

    fn has_instr(&mut self) -> bool {
        let frame = self.frame();
        let chunk_index = frame.closure.function.chunk_index;
        let chunk = self.instance.chunk(chunk_index);
        chunk.instructions(ChunkInstructionType::Standard).len() >= frame.pc
    }

    fn push(&mut self, value: Value) -> Result<(), VMError> {
        if self.stack.len() >= MAX_STACK_SIZE {
            return Err(VMError::StackOverflow {});
        }
        self.stack.push(value);
        Ok(())
    }

    fn peek_skip(&self, skip: usize) -> Result<&Value, VMError> {
        let value = self.stack.get(self.stack.len() - 1 - skip);
        match value {
            Some(value) => Ok(&value),
            None => Err(VMError::StackEmpty { pc: self.pc() }),
        }
    }

    pub fn print(&self) {
        self.instance.print();
        println!();
        println!("===================== VM TRACE =====================");
        println!();
        println!("Stack {:?}", self.stack);
        println!();
        println!("Frames {:?}", self.frames);
        println!();
        println!("Heap {:?}", self.heap);
    }

    pub fn add_constant(&mut self, constant: Constant) -> Result<(), VMError> {
        match constant {
            Constant::Number(n) => self.push(Value::Number(n))?,
            Constant::String(string) => {
                let allocation = self.heap.root(string);
                self.push(Value::String(allocation))?;
            }
            Constant::Boolean(boolean) => self.push(Value::Boolean(boolean))?,
            Constant::Nil => self.push(Value::Nil)?,
        }
        Ok(())
    }

    fn call(&mut self, callee: Value, call_arity: usize) -> Result<(), VMError> {
        match callee {
            Value::Func(func) => match func {
                DuckFn::Script { func } => {
                    let arity = func.function.arity;
                    if arity != call_arity {
                        return Err(VMError::IncorrectArity {
                            expected_arity: call_arity,
                            actual_arity: arity,
                        });
                    }
                    self.frames.push(CallFrame {
                        closure: func,
                        base_counter: self.stack.len() - arity,
                        pc: 0,
                        frame_chunk: None,
                    });
                }
                DuckFn::Native { mut func } => {
                    let arity = func.arity;
                    if call_arity != arity {
                        return Err(VMError::IncorrectArity {
                            expected_arity: call_arity,
                            actual_arity: arity,
                        });
                    }
                    for _ in 0..arity {
                        let arg = self.pop()?;
                        func.add_arg(arg);
                    }
                    let native_value = func.call()?;
                    match native_value {
                        Some(native_value) => {
                            let value = self.to_value(native_value);
                            self.push(value)?;
                        }
                        None => {}
                    }
                }
            },
            _ => {
                return Err(VMError::InvalidCallee { callee });
            }
        }

        Ok(())
    }

    pub fn run(&mut self) -> VMResult {
        setup_stdlib(self);
        loop {
            let instr = self.next_instr();
            if let Some(instr) = instr {
                if self.options.trace {
                    let chunk_type = &self.frame().frame_chunk;
                    match chunk_type {
                        Some(citype) => print!("{:?} ", citype.chunk_type),
                        None => {}
                    }
                    disassemble_instruction(&instr);
                }
                self.run_instruction(instr)?;
            } else {
                break;
            }
        }
        self.heap.collect();
        Ok(InterpreterResult::Ok(self.peek_value()))
    }

    fn run_instruction(&mut self, instr: Instruction) -> VMResult {
        match instr {
            Instruction::Print => {
                let expr_to_print = self.pop()?;
                println!("{}", expr_to_print);
            }
            Instruction::DefineGlobal(identifier_index) => {
                let value = self.pop()?;
                let name = self.instance.get_identifier(identifier_index);
                self.module.add_global(name.clone(), value);
            }
            Instruction::GetGlobal(identifier_index) => {
                let identifier = self.instance.get_identifier(identifier_index);
                let global = self.module.resolve_global(identifier);
                match global {
                    Some(global) => self.push(global)?,
                    None => {
                        return Err(VMError::IdentifierNotFound {
                            identifier: identifier.clone(),
                        });
                    }
                }
            }
            Instruction::SetGlobal(identifier_index) => {
                let value = self.peek()?;
                let identifier = self.instance.get_identifier(identifier_index);
                self.module.add_global(identifier.clone(), value);
            }
            Instruction::SetLocal(index) => {
                let index = self.base_counter() + index; // relative to frame
                let value = self.peek()?;
                if index >= self.stack.len() {
                    return Err(VMError::InvalidStackIndex {
                        index,
                        size: self.stack.len(),
                    });
                }
                self.stack[index] = value;
            }
            Instruction::GetLocal(index) => {
                let index = self.base_counter() + index;
                self.push(self.stack[index])?;
            }
            Instruction::JumpIfFalse(index) => {
                let condition_result = self.peek()?;
                if self.is_falsey(&condition_result) {
                    self.set_pc(index);
                }
            }
            Instruction::Jump(index) => {
                self.set_pc(index);
            }
            Instruction::Call(arity) => {
                let callee = *self.peek_skip(arity)?;
                self.call(callee, arity)?;
            }
            Instruction::Closure(index) => {
                let closure = self.instance.get_closure(index).clone();
                let upvalues = closure
                    .upvalues
                    .iter()
                    .map(|uval| match uval {
                        bytecode::Upvalue::Local(index) => {
                            let frame = &self.frames[self.frames.len() - 1];
                            let base = frame.base_counter;
                            let index = base + *index;

                            if let Some(upvalue) = self.find_open_upvalue_with_index(index) {
                                upvalue
                            } else {
                                let upvalue = Upvalue::Open(index);
                                self.heap.root(Cell::new(upvalue))
                            }
                        }
                        bytecode::Upvalue::Upvalue(index) => self.find_upvalue_by_index(*index),
                    })
                    .collect();
                let allocation = self.heap.root(Closure {
                    function: Function::new(&closure.function),
                    upvalues,
                });
                self.push(Value::Func(DuckFn::Script { func: allocation }))?;
            }
            Instruction::NewInstance(arity) => {
                let mut instance = super::memory::ObjInstance::default();
                for _ in 0..arity {
                    let prop = self.pop()?;
                    let prop_name = self.pop()?;
                    match prop_name {
                        Value::String(name) => {
                            //let allocated = self.heap.manage(Cell::new(prop));
                            instance.add_property(name.to_string(), Cell::new(prop));
                        }
                        _ => return Err(VMError::InvalidProperty { prop: prop_name }),
                    }
                }
                let value = self.heap.root(instance);
                self.push(Value::Instance(value))?;
            }
            Instruction::Return => {
                let result = self.pop()?;
                // pop function on stack
                let frame = self.frames.pop().expect("Missing Return frame");
                self.stack.truncate(frame.base_counter - 1);
                self.push(result)?;
            }
            Instruction::Ensure => {
                if self.options.contracts_eval == true {
                    match self.frame().current_chunk_instruction_type() {
                        Some(_) => {
                            self.frame_mut().clear_chunk();
                        }
                        None => {
                            self.move_to_chunk(ChunkInstructionType::Postcondition);
                        }
                    }
                }
            }
            Instruction::Require => {
                if self.options.contracts_eval == true {
                    match self.frame().current_chunk_instruction_type() {
                        Some(_) => {
                            self.frame_mut().clear_chunk();
                        }
                        None => {
                            self.move_to_chunk(ChunkInstructionType::Precondition);
                        }
                    }
                }
            }
            Instruction::CheckContract => {
                let value = self.pop()?;
                if self.is_falsey(&value) {
                    return Err(VMError::ContractNotValid {});
                }
            }
            Instruction::Get(property) => {
                let val = self.pop()?;
                match val {
                    Value::Instance(instance) => {
                        let prop_value = instance.get_property(&property);
                        match prop_value {
                            Some(v) => self.push(v)?,
                            None => return Err(VMError::UndefinedProperty { prop: property }),
                        }
                    }
                    _ => return Err(VMError::NotAValidInstance { found: val }),
                }
            }
            Instruction::Set(property) => {
                let val = self.pop()?;
                match val {
                    Value::Instance(instance) => {
                        let prop_value = instance.get_property(&property);
                        match prop_value {
                            Some(_) => {
                                let value_to_set = self.pop()?;
                                instance.set_property(&property, value_to_set);
                            }
                            None => return Err(VMError::UndefinedProperty { prop: property }),
                        }
                    }
                    _ => return Err(VMError::NotAValidInstance { found: val }),
                }
            }
            Instruction::Constant(index) => {
                let constant_value = self.instance.get_const(index).clone();
                self.add_constant(constant_value)?;
            }
            Instruction::Negate => match self.pop()? {
                Value::Number(n) => self.push(Value::Number(-n))?,
                _ => {
                    return Err(VMError::UnexpectedValueType {
                        value: self.peek()?,
                        operation: "Negate".to_string(),
                    })
                }
            },
            Instruction::String(str) => {
                let allocation: Gc<String> = self.heap.root(str.to_string());
                self.push(Value::String(allocation))?;
            }
            Instruction::Sum => {
                self.sum()?;
            }
            Instruction::Sub => {
                self.sub()?;
            }
            Instruction::Div => {
                self.div()?;
            }
            Instruction::Mul => {
                self.mul()?;
            }
            Instruction::Greater => {
                self.greater()?;
            }
            Instruction::Less => {
                self.less()?;
            }
            Instruction::Not => {
                self.not()?;
            }
            Instruction::Pop => {
                self.pop()?;
            }
            Instruction::Equal => {
                self.equal()?;
            }
            Instruction::SetUpvalue(index) => {
                let value = self.peek()?;
                let upvalue = self.frame().closure.upvalues.get(index).unwrap().clone();
                self.set_upvalue(&mut upvalue.get(), value);
            }
            Instruction::GetUpvalue(index) => {
                let upvalue = &self.frame().closure.upvalues[index];
                self.push(self.resolve_upvalue(&upvalue.get()))?;
            }
            Instruction::CloseUpvalue => {
                // close upvalue on the top of the stack and retain other closed upvalues
                let index = self.stack.len() - 1;
                self.close_upvalues(index);
                self.pop()?;
            }
            Instruction::True => {
                self.push(Value::Boolean(true))?;
            }
            Instruction::False => {
                self.push(Value::Boolean(false))?;
            }
            _ => return Err(VMError::VMError { instruction: instr }),
        }
        Ok(InterpreterResult::Ok(self.peek_value()))
    }

    fn move_to_chunk(&mut self, citype: ChunkInstructionType) {
        let base_counter: usize = self.frame().base_counter;
        self.frame_mut().move_to_chunk(citype, base_counter);
    }

    fn to_value(&mut self, native_value: NativeValue) -> Value {
        match native_value {
            NativeValue::Number(n) => Value::Number(n),
            NativeValue::String(s) => {
                let allocation: Gc<String> = self.heap.root(s.to_string());
                Value::String(allocation)
            }
        }
    }

    fn find_upvalue_by_index(&self, index: usize) -> Gc<Cell<Upvalue>> {
        let frame = &self.frames[self.frames.len() - 1];
        frame.closure.upvalues[index]
    }

    fn find_open_upvalue_with_index(&self, index: usize) -> Option<Gc<Cell<Upvalue>>> {
        for upvalue in self.frame().closure.upvalues.iter().rev() {
            if upvalue.get().is_open_with_index(index) {
                return Some(*upvalue);
            }
        }
        None
    }

    fn close_upvalues(&mut self, index: usize) {
        let value = self.stack[index].clone();
        for upvalue in self.frame().closure.upvalues.iter() {
            if upvalue.get().is_open_with_index(index) {
                upvalue.allocation().unroot();
                upvalue.replace(Upvalue::Closed(value));
            }
        }
        self.frame_mut()
            .closure
            .upvalues
            .retain(|u| u.get().is_open());
    }

    fn resolve_upvalue(&self, upvalue: &Upvalue) -> Value {
        match upvalue {
            Upvalue::Open(index) => self.stack[*index],
            Upvalue::Closed(value) => *value,
        }
    }

    fn is_falsey(&self, value: &Value) -> bool {
        match value {
            Value::Nil => true,
            Value::Number(_) => false,
            Value::String(_) => false,
            Value::Boolean(b) => !b,
            Value::Func(_) => false,
            Value::Instance(_) => false,
        }
    }

    fn get_binary_operands(&mut self) -> Result<(f64, f64), VMError> {
        let b = self.pop()?;
        let a = self.pop()?;
        Ok((a.into(), b.into()))
    }

    fn sum(&mut self) -> Result<(), VMError> {
        let operands = self.get_binary_operands()?;
        self.push(Value::Number(operands.0 + operands.1))?;
        Ok(())
    }

    fn sub(&mut self) -> Result<(), VMError> {
        let operands = self.get_binary_operands()?;
        self.push(Value::Number(operands.0 - operands.1))?;
        Ok(())
    }

    fn mul(&mut self) -> Result<(), VMError> {
        let operands = self.get_binary_operands()?;
        self.push(Value::Number(operands.0 * operands.1))?;
        Ok(())
    }

    fn div(&mut self) -> Result<(), VMError> {
        let operands = self.get_binary_operands()?;
        if operands.1 == 0.0 {
            return Err(VMError::DivByZero);
        }
        self.push(Value::Number(operands.0 / operands.1))?;
        Ok(())
    }

    fn greater(&mut self) -> Result<(), VMError> {
        let operands = self.get_binary_operands()?;
        let result = operands.0 > operands.1;
        self.push(Value::Boolean(result))?;
        Ok(())
    }

    fn less(&mut self) -> Result<(), VMError> {
        let operands = self.get_binary_operands()?;
        let result = operands.0 < operands.1;
        self.push(Value::Boolean(result))?;
        Ok(())
    }

    fn equal(&mut self) -> Result<(), VMError> {
        let operands = self.get_binary_operands()?;
        let result = operands.0 == operands.1;
        self.push(Value::Boolean(result))?;
        Ok(())
    }

    fn not(&mut self) -> Result<(), VMError> {
        let value = self.pop()?;
        self.push(Value::Boolean(self.is_falsey(&value)))?;
        Ok(())
    }

    fn set_upvalue(&mut self, upvalue: &mut Upvalue, new_value: Value) {
        match upvalue {
            Upvalue::Closed(_) => *upvalue = Upvalue::Closed(new_value),
            Upvalue::Open(index) => self.stack[*index] = new_value,
        }
    }
}
