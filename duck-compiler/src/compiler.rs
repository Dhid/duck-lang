use crate::errors::CompilerError;
use crate::locals::{Local, Locals};
use duck_bytecode::bytecode::{
    Chunk, ChunkIndex, ChunkInstructionType, Closure, Constant, Function, IdentifierIndex,
    Instance, Instruction, InstructionIndex, StackIndex, Upvalue,
};
use duck_syntax::ast::{
    Ast, BinaryOperator, Expr, Identifier, InstanceParam, LogicalOperator, Mutability,
    Postcondition, Precondition, Stmt, UnaryOperator,
};
use duck_syntax::scanner::WithSpan;
use std::collections::HashMap;

type CompilerResult = Result<(), CompilerError>;

pub struct CompilerContext {
    chunk_index: ChunkIndex,
    locals: Locals,
    upvalues: Vec<Upvalue>,
    current_chunk_type: ChunkInstructionType,
}

impl CompilerContext {
    pub fn new(chunk_index: ChunkIndex) -> Self {
        CompilerContext {
            chunk_index,
            locals: Locals::new(),
            upvalues: vec![],
            current_chunk_type: ChunkInstructionType::Standard,
        }
    }

    pub fn chunk_type(&self) -> ChunkInstructionType {
        self.current_chunk_type
    }

    pub fn set_chunk_type(&mut self, citype: ChunkInstructionType) {
        self.current_chunk_type = citype;
    }

    fn add_upvalue(&mut self, upvalue: Upvalue) -> StackIndex {
        // if an upvalue is found in current context, must exists in the locals stack
        for i in 0..self.upvalues.len() {
            let existing_upvalue = &self.upvalues[i];
            if upvalue == *existing_upvalue {
                return i;
            }
        }
        self.upvalues.push(upvalue);
        self.upvalues.len() - 1
    }

    fn resolve_local(&self, name: &str) -> Option<&Local> {
        self.locals.stack.iter().find(|&local| local.name == name)
    }
}

pub struct Compiler<'a> {
    ast: &'a Ast,
    identifiers: HashMap<Identifier, IdentifierIndex>,
    contexts: Vec<CompilerContext>,
    pub instance: Instance,
}

impl<'a> Compiler<'a> {
    #[allow(unused_must_use)]
    pub fn new(ast: &'a Ast) -> Self {
        Compiler {
            ast,
            identifiers: HashMap::new(),
            contexts: Vec::new(),
            instance: Instance::new(),
        }
    }

    fn current_chunk(&self) -> &Chunk {
        let chunk_index = self.current_context().chunk_index;
        self.instance.chunk(chunk_index)
    }

    fn current_chunk_type(&self) -> ChunkInstructionType {
        self.current_context().chunk_type()
    }

    pub fn set_chunk_type(&mut self, citype: ChunkInstructionType) {
        self.current_context_mut().set_chunk_type(citype);
    }

    fn current_chunk_mut(&mut self) -> &mut Chunk {
        let chunk_index = self.current_context().chunk_index;
        self.instance.chunk_mut(chunk_index)
    }

    fn add_instr(&mut self, instruction: Instruction) -> InstructionIndex {
        let current_chunk_type = self.current_chunk_type();
        let chunk = self.current_chunk_mut();
        chunk.add_instruction(instruction, current_chunk_type)
    }

    fn pop_instr(&mut self) -> Instruction {
        let current_chunk_type = self.current_chunk_type();
        let chunk = self.current_chunk_mut();
        chunk.pop_instruction(current_chunk_type)
    }

    fn chunk_index(&self) -> usize {
        self.current_context().chunk_index
    }

    fn current_context_mut(&mut self) -> &mut CompilerContext {
        self.contexts.last_mut().expect("Missing context")
    }

    fn current_context(&self) -> &CompilerContext {
        self.contexts.last().expect("Missing context")
    }

    fn is_scoped(&mut self) -> bool {
        self.current_context_mut().locals.actual_depth > 0
    }

    fn begin_context(&mut self) -> ChunkIndex {
        let chunk_index = self.instance.add_chunk();
        self.contexts.push(CompilerContext {
            chunk_index,
            locals: Locals::new(),
            upvalues: vec![],
            current_chunk_type: ChunkInstructionType::Standard,
        });
        chunk_index
    }

    fn end_context(&mut self) -> CompilerContext {
        self.contexts.pop().expect("No Contexts to end")
    }

    fn begin_scope(&mut self) -> usize {
        self.current_context_mut().locals.actual_depth += 1;
        self.current_context_mut().locals.actual_depth
    }

    fn end_scope(&mut self) {
        for local in self.current_context_mut().locals.end_scope().iter().rev() {
            if local.captured() {
                self.add_instr(Instruction::CloseUpvalue);
            } else {
                self.add_instr(Instruction::Pop);
            }
        }
    }

    fn depth(&mut self) -> usize {
        self.current_context_mut().locals.actual_depth
    }

    fn add_local(
        &mut self,
        identifier: Identifier,
        mutability: Mutability,
    ) -> Result<StackIndex, ()> {
        let slot = self.current_context().locals.stack.len();
        let local = Local::new(identifier.clone(), self.depth(), slot, mutability);
        // check if the local is already defined ath current depth
        let depth = self.depth();
        for local_at_depth in self
            .current_context_mut()
            .locals
            .stack
            .iter()
            .rev()
            .filter(|l| l.depth == depth)
        {
            if local_at_depth.name == identifier {
                return Err(());
            }
        }
        self.current_context_mut().locals.add(local)?;
        Ok(self.current_context_mut().locals.stack.len() - 1)
    }

    fn resolve_local(&self, identifier: &Identifier) -> Option<&Local> {
        for local in self.current_context().locals.stack.iter().rev() {
            if local.name == *identifier && local.is_initialized() {
                return Some(local);
            }
        }
        None
    }

    fn resolve_upvalue(&mut self, identifier: &Identifier) -> Option<StackIndex> {
        // find the context where the upvalue is in
        for i in (0..self.contexts.len() - 1).rev() {
            match self.contexts[i].resolve_local(identifier) {
                Some(local) => {
                    // add to higher context
                    let slot = local.slot;
                    // is set as local, just look at the stack during execution
                    self.contexts[i].locals.mark_captured(slot);
                    let mut upvalue = self.contexts[i + 1].add_upvalue(Upvalue::Local(slot));
                    for j in (i + 2)..self.contexts.len() {
                        // upvalues must be copied here in case of return of nested closures
                        // is referenced as a upvalue, so needed to lookup in the upper context
                        // during the execution
                        upvalue = self.contexts[j].add_upvalue(Upvalue::Upvalue(upvalue));
                    }
                    return Some(upvalue);
                }
                None => {}
            }
        }
        None
    }

    fn identifier(&self, identifier: &Identifier) -> usize {
        self.identifiers
            .get(identifier)
            .expect(format!("Missing identifier {}", identifier).as_str())
            .clone()
    }

    fn add_identifier(&mut self, identifier: Identifier) -> IdentifierIndex {
        if let Some(index) = self.identifiers.get(&identifier) {
            *index
        } else {
            let index = self.instance.add_identifier(identifier.clone());
            self.identifiers.insert(identifier, index);
            index
        }
    }

    fn mark_as_initialized(&mut self) {
        let index = self.current_context_mut().locals.stack.len() - 1;
        self.current_context_mut().locals.stack[index].mark_initialized();
    }

    fn declare_var(&mut self, identifier: &WithSpan<Identifier>, mutability: Mutability) {
        if self.is_scoped() {
            // it's a local
            let local = self.add_local(identifier.value.clone(), mutability);
            match local {
                Ok(_) => {}
                Err(_) => {
                    panic!("Local {} already defined", identifier.value);
                }
            }
        }
    }

    fn define_var(&mut self, identifier: &WithSpan<Identifier>) {
        if self.is_scoped() {
            self.mark_as_initialized();
        } else {
            // it's a global
            let identifier_index = self.add_identifier(identifier.value.clone());
            self.add_instr(Instruction::DefineGlobal(identifier_index));
        }
    }

    fn patch_jmp(&mut self, jmp_index: usize) {
        let current = self
            .current_chunk()
            .instructions(ChunkInstructionType::Standard)
            .len();
        let current_chunk_type = self.current_chunk_type();
        self.current_chunk_mut()
            .patch_instruction_to(jmp_index, current, current_chunk_type);
    }

    fn current_instruction_index(&self) -> InstructionIndex {
        let chunk_index = self.current_context().chunk_index;
        let chunk = self.instance.chunk(chunk_index);
        let current_chunk_type = self.current_chunk_type();
        chunk.instructions(current_chunk_type).len()
    }
}

pub fn compile_ast(compiler: &mut Compiler) -> CompilerResult {
    compiler.begin_context();
    for stmt in compiler.ast {
        compile_stmt(compiler, stmt)?;
    }
    compiler.end_context();
    Ok(())
}

fn compile_stmt(compiler: &mut Compiler, stmt: &WithSpan<Stmt>) -> CompilerResult {
    match &stmt.value {
        Stmt::Var(identifier, expr, mutability) => {
            compile_var(compiler, &identifier, &expr, *mutability)?
        }
        Stmt::Expr(expr) => compile_expr(compiler, &expr)?,
        Stmt::Print(expr) => {
            compile_expr(compiler, &expr)?;
            compiler.add_instr(Instruction::Print);
        }
        Stmt::If(condition, if_stmt, else_stmt) => {
            compile_if(compiler, condition, if_stmt, else_stmt)?;
        }
        Stmt::Block(statements) => compile_block(compiler, statements)?,
        Stmt::While(condition, statements) => compile_while(compiler, condition, statements)?,
        Stmt::Function(name, pre, post, arguments, body) => {
            compile_function(compiler, name, pre, post, arguments, body)?
        }
        Stmt::Return(expr) => {
            compile_return(compiler, expr)?;
        }
    }
    Ok(())
}

fn compile_function(
    compiler: &mut Compiler,
    name: &WithSpan<Identifier>,
    preconditions: &Vec<WithSpan<Precondition>>,
    postconditions: &Vec<WithSpan<Postcondition>>,
    arguments: &Vec<WithSpan<Identifier>>,
    body: &Vec<WithSpan<Stmt>>,
) -> CompilerResult {
    // declare function by name
    compiler.declare_var(name, Mutability::Const);
    if compiler.is_scoped() {
        compiler.mark_as_initialized();
    }
    compiler.begin_context();
    compiler.begin_scope();
    // declare arguments
    for arg in arguments {
        compiler.declare_var(arg, Mutability::Const); // TODO each argument has it's mutability
                                                      // requirement
        compiler.define_var(arg);
    }
    // compile contract before function body
    compile_precondition(compiler, preconditions)?;
    for stmt in body {
        compile_stmt(compiler, stmt)?;
    }
    compile_postcondition(compiler, postconditions)?;
    compiler.set_chunk_type(ChunkInstructionType::Standard);
    // return with None for void functions
    // skipped if another return already defined
    compile_return(compiler, &None)?;

    let context = compiler.end_context();
    let closure = Closure {
        function: Function {
            name: name.value.clone(),
            arity: arguments.len(),
            chunk_index: context.chunk_index,
        },
        upvalues: context.upvalues,
    };
    let closure_index = compiler.instance.add_closure(closure);
    compiler.add_instr(Instruction::Closure(closure_index));
    compiler.define_var(name);
    Ok(())
}

fn compile_precondition(
    compiler: &mut Compiler,
    preconditions: &Vec<WithSpan<Precondition>>,
) -> CompilerResult {
    if preconditions.len() > 0 {
        compiler.add_instr(Instruction::Require);
        for pre in preconditions.into_iter() {
            compiler.set_chunk_type(ChunkInstructionType::Precondition);
            match &pre.value {
                Precondition::Require(expr) => {
                    compile_expr(compiler, expr.as_ref())?;
                }
            }
            compiler.add_instr(Instruction::CheckContract);
        }
        compiler.add_instr(Instruction::Require);
        compiler.set_chunk_type(ChunkInstructionType::Standard);
    }
    Ok(())
}

fn compile_postcondition(
    compiler: &mut Compiler,
    postconditions: &Vec<WithSpan<Postcondition>>,
) -> CompilerResult {
    if postconditions.len() > 0 {
        for post in postconditions.into_iter() {
            compiler.set_chunk_type(ChunkInstructionType::Postcondition);
            match &post.value {
                Postcondition::Ensure(expr) => {
                    compile_expr(compiler, expr.as_ref())?;
                }
            }
            compiler.add_instr(Instruction::CheckContract);
            compiler.end_scope();
            compiler.add_instr(Instruction::Ensure);
        }
    } else {
        // add truly ensure
        compiler.set_chunk_type(ChunkInstructionType::Postcondition);
        compiler.add_instr(Instruction::True);
        compiler.add_instr(Instruction::CheckContract);
        compiler.add_instr(Instruction::Ensure);
    }
    Ok(())
}

fn compile_return(compiler: &mut Compiler, expr: &Option<Box<WithSpan<Expr>>>) -> CompilerResult {
    if let Some(expr) = expr {
        compile_expr(compiler, expr)?;
    }
    compiler.add_instr(Instruction::Ensure);
    compiler.add_instr(Instruction::Return);
    Ok(())
}

fn compile_while(
    compiler: &mut Compiler,
    condition: &Box<WithSpan<Expr>>,
    while_stmt: &Box<WithSpan<Stmt>>,
) -> CompilerResult {
    // compile condition
    // add jmp if false
    // compile while body
    // compile jmp to condition
    let condition_index = compiler.current_instruction_index();
    compile_expr(compiler, condition)?;
    let end_jmp = compiler.add_instr(Instruction::JumpIfFalse(0));
    compiler.add_instr(Instruction::Pop); // pop the result of the check
    compile_stmt(compiler, while_stmt)?;
    compiler.add_instr(Instruction::Jump(condition_index));
    compiler.add_instr(Instruction::Pop);
    compiler.patch_jmp(end_jmp);
    Ok(())
}

fn compile_if(
    compiler: &mut Compiler,
    condition: &Box<WithSpan<Expr>>,
    if_stmt: &Box<WithSpan<Stmt>>,
    else_stmt: &Option<Box<WithSpan<Stmt>>>,
) -> CompilerResult {
    compile_expr(compiler, condition)?;
    let then_index = compiler.add_instr(Instruction::JumpIfFalse(0));
    compiler.add_instr(Instruction::Pop);
    compile_stmt(compiler, if_stmt)?;

    if let Some(else_statement) = else_stmt {
        let else_index = compiler.add_instr(Instruction::Jump(0));
        compiler.patch_jmp(then_index);
        compiler.add_instr(Instruction::Pop);
        compile_stmt(compiler, &else_statement)?;
        compiler.patch_jmp(else_index);
    } else {
        compiler.patch_jmp(then_index);
    }
    Ok(())
}

fn compile_block(compiler: &mut Compiler, statements: &Vec<WithSpan<Stmt>>) -> CompilerResult {
    compiler.begin_scope();
    for stmt in statements {
        compile_stmt(compiler, stmt)?;
    }
    compiler.end_scope();
    Ok(())
}

fn compile_var(
    compiler: &mut Compiler,
    identifier: &WithSpan<Identifier>,
    expr: &Option<Box<WithSpan<Expr>>>,
    mutability: Mutability,
) -> CompilerResult {
    compiler.declare_var(identifier, mutability);
    match expr {
        Some(expr) => {
            compile_expr(compiler, expr)?;
        }
        None => {
            // set variable to Nil
            compiler.add_instr(Instruction::Nil);
        }
    };
    compiler.define_var(identifier);
    Ok(())
}

fn compile_expr(compiler: &mut Compiler, expr: &WithSpan<Expr>) -> CompilerResult {
    match expr.value.clone() {
        Expr::Number(n) => {
            let index = compiler.instance.add_const(Constant::Number(n));
            compiler.add_instr(Instruction::Constant(index));
        }
        Expr::String(string) => {
            let index = compiler.instance.add_const(Constant::String(string));
            compiler.add_instr(Instruction::Constant(index));
        }
        Expr::Nil => {
            let index = compiler.instance.add_const(Constant::Nil);
            compiler.add_instr(Instruction::Constant(index));
        }
        Expr::Boolean(boolean) => {
            let index = compiler.instance.add_const(Constant::Boolean(boolean));
            compiler.add_instr(Instruction::Constant(index));
        }
        Expr::Binary(left, op, right) => {
            compile_binary(compiler, left.as_ref(), &op, right.as_ref())?;
        }
        Expr::Grouping(grouped) => {
            compile_expr(compiler, grouped.as_ref())?;
        }
        Expr::Unary(op, unary) => {
            compile_expr(compiler, unary.as_ref())?;
            match op.value {
                UnaryOperator::Bang => {
                    compiler.add_instr(Instruction::Not);
                }
                UnaryOperator::Minus => {
                    compiler.add_instr(Instruction::Negate);
                }
            }
        }
        Expr::Logical(left, op, right) => {
            compile_logical(compiler, left.as_ref(), &op, right.as_ref())?;
        }
        Expr::Variable(identifier) => {
            let has_local = compiler.current_context().resolve_local(&identifier.value);
            if let Some(local) = has_local {
                compiler.add_instr(Instruction::GetLocal(local.slot));
            } else if let Some(upvalue) = compiler.resolve_upvalue(&identifier.value) {
                compiler.add_instr(Instruction::GetUpvalue(upvalue));
            } else {
                let index = compiler.add_identifier(identifier.value);
                compiler.add_instr(Instruction::GetGlobal(index));
            }
        }
        Expr::Assign(identifier, expr) => compile_assign(compiler, identifier, expr.as_ref())?,
        Expr::Call(identifier, arguments) => compile_call(compiler, identifier, arguments)?,
        Expr::Ret => {}
        Expr::Instance(params) => compile_instance(compiler, params)?,
        Expr::Get(expr, identifier) => compile_get(compiler, expr.as_ref(), identifier)?,
        Expr::Set(left, identifier, right) => {
            compile_set(compiler, left.as_ref(), identifier, right.as_ref())?
        }
    }
    Ok(())
}

fn compile_set(
    compiler: &mut Compiler,
    left: &WithSpan<Expr>,
    identifier: WithSpan<Identifier>,
    right: &WithSpan<Expr>,
) -> CompilerResult {
    compile_expr(compiler, right)?; // this will be the value under the Instance on the stack
    compile_expr(compiler, left)?; // this will produce a Instance on top of the stack
    compiler.add_instr(Instruction::Set(identifier.value));
    Ok(())
}

fn compile_get(
    compiler: &mut Compiler,
    left: &WithSpan<Expr>,
    identifier: WithSpan<Identifier>,
) -> CompilerResult {
    compile_expr(compiler, left)?;
    compiler.add_instr(Instruction::Get(identifier.value));
    Ok(())
}

fn compile_instance(
    compiler: &mut Compiler,
    params: Vec<WithSpan<InstanceParam>>,
) -> CompilerResult {
    let arity = params.len();
    // remember you are a stack
    for param in params.iter().rev() {
        compiler.add_instr(Instruction::String(param.value.identifier.value.clone()));
        compile_expr(compiler, &param.value.expr)?;
    }
    compiler.add_instr(Instruction::NewInstance(arity));
    Ok(())
}

fn compile_call(
    compiler: &mut Compiler,
    identifier: Box<WithSpan<Expr>>,
    arguments: Vec<WithSpan<Expr>>,
) -> CompilerResult {
    compile_expr(compiler, &identifier)?;
    let arity = arguments.len();
    for arg in arguments {
        compile_expr(compiler, &arg)?;
    }
    compiler.add_instr(Instruction::Call(arity));
    Ok(())
}

fn compile_assign(
    compiler: &mut Compiler,
    identifier: WithSpan<Identifier>,
    right: &WithSpan<Expr>,
) -> CompilerResult {
    compile_expr(compiler, right)?;
    let local = compiler.resolve_local(&identifier.value);
    if let Some(local) = local {
        compiler.add_instr(Instruction::SetLocal(local.slot));
    } else if let Some(upvalue) = compiler.resolve_upvalue(&identifier.value) {
        compiler.add_instr(Instruction::SetUpvalue(upvalue));
    } else {
        let identifier_index = compiler.identifier(&identifier.value);
        compiler.add_instr(Instruction::SetGlobal(identifier_index));
    }
    Ok(())
}

fn compile_logical(
    compiler: &mut Compiler,
    left: &WithSpan<Expr>,
    op: &WithSpan<LogicalOperator>,
    right: &WithSpan<Expr>,
) -> CompilerResult {
    match op.value {
        LogicalOperator::And => compile_and(compiler, left, right)?,
        LogicalOperator::Or => compile_or(compiler, left, right)?,
    }
    Ok(())
}

fn compile_and(
    compiler: &mut Compiler,
    left: &WithSpan<Expr>,
    right: &WithSpan<Expr>,
) -> CompilerResult {
    // compile left, and set jump if false at the end of the right is left is falsey
    compile_expr(compiler, left)?;
    compiler.add_instr(Instruction::JumpIfFalse(0));
    let jmp_index = compiler.current_instruction_index();
    compile_expr(compiler, right)?;
    compiler.patch_jmp(jmp_index);
    Ok(())
}

fn compile_or(
    compiler: &mut Compiler,
    left: &WithSpan<Expr>,
    right: &WithSpan<Expr>,
) -> CompilerResult {
    compile_expr(compiler, left)?;
    compiler.add_instr(Instruction::Not);
    compiler.add_instr(Instruction::JumpIfFalse(0));
    let jmp_index = compiler.current_instruction_index();
    compile_expr(compiler, right)?;
    compiler.patch_jmp(jmp_index);
    Ok(())
}

fn compile_binary(
    compiler: &mut Compiler,
    left: &WithSpan<Expr>,
    op: &WithSpan<BinaryOperator>,
    right: &WithSpan<Expr>,
) -> CompilerResult {
    compile_expr(compiler, left)?;
    compile_expr(compiler, right)?;
    match op.value {
        BinaryOperator::Plus => compiler.add_instr(Instruction::Sum),
        BinaryOperator::Minus => {
            compiler.add_instr(Instruction::Negate);
            compiler.add_instr(Instruction::Sum)
        }
        BinaryOperator::Star => compiler.add_instr(Instruction::Mul),
        BinaryOperator::Slash => compiler.add_instr(Instruction::Div),
        BinaryOperator::Greater => compiler.add_instr(Instruction::Greater),
        BinaryOperator::Less => compiler.add_instr(Instruction::Less),
        BinaryOperator::GreaterEqual => {
            compiler.add_instr(Instruction::Less);
            compiler.add_instr(Instruction::Not)
        }
        BinaryOperator::LessEqual => {
            compiler.add_instr(Instruction::Greater);
            compiler.add_instr(Instruction::Not)
        }
        BinaryOperator::EqualEqual => compiler.add_instr(Instruction::Equal),
        BinaryOperator::BangEqual => {
            compiler.add_instr(Instruction::Not);
            compiler.add_instr(Instruction::Equal)
        }
    };
    Ok(())
}
