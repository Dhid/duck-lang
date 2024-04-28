use crate::scanner::WithSpan;
use serde::{Deserialize, Serialize};

pub type Ast = Vec<WithSpan<Stmt>>;
pub type Identifier = String;

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone, Copy)]
pub enum Mutability {
    Const,
    Mut,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum Stmt {
    Expr(Box<WithSpan<Expr>>),
    Var(
        WithSpan<Identifier>,
        Option<Box<WithSpan<Expr>>>,
        Mutability,
    ), // option for var declaration or assignment
    Block(Vec<WithSpan<Stmt>>),
    While(Box<WithSpan<Expr>>, Box<WithSpan<Stmt>>),
    If(
        Box<WithSpan<Expr>>,
        Box<WithSpan<Stmt>>,
        Option<Box<WithSpan<Stmt>>>,
    ),
    Print(Box<WithSpan<Expr>>),
    Return(Option<Box<WithSpan<Expr>>>), // Option for void return
    Function(
        WithSpan<Identifier>,         // name
        Vec<WithSpan<Precondition>>,  //function list of preconditions
        Vec<WithSpan<Postcondition>>, //function list of postconditions
        Vec<WithSpan<Identifier>>,    // arguments
        Box<Vec<WithSpan<Stmt>>>,     // body
    ),
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone, Copy)]
pub enum Nullability {
    Nullable,
    NonNullable,
}

impl Into<bool> for Nullability {
    fn into(self) -> bool {
        match self {
            Nullability::Nullable => true,
            Nullability::NonNullable => false,
        }
    }
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum Precondition {
    Require(Box<WithSpan<Expr>>), // precondition
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum Postcondition {
    Ensure(Box<WithSpan<Expr>>), // postcondition
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum Expr {
    Number(f64),
    String(String),
    Boolean(bool),
    Nil,
    Variable(WithSpan<Identifier>),
    Ret,
    Unary(WithSpan<UnaryOperator>, Box<WithSpan<Expr>>),
    Grouping(Box<WithSpan<Expr>>),
    Binary(
        Box<WithSpan<Expr>>,
        WithSpan<BinaryOperator>,
        Box<WithSpan<Expr>>,
    ),
    Logical(
        Box<WithSpan<Expr>>,
        WithSpan<LogicalOperator>,
        Box<WithSpan<Expr>>,
    ),
    Assign(WithSpan<Identifier>, Box<WithSpan<Expr>>),
    Get(Box<WithSpan<Expr>>, WithSpan<Identifier>),
    Set(
        Box<WithSpan<Expr>>,
        WithSpan<Identifier>,
        Box<WithSpan<Expr>>,
    ),
    Call(Box<WithSpan<Expr>>, Vec<WithSpan<Expr>>),
    Instance(Vec<WithSpan<InstanceParam>>),
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub struct InstanceParam {
    pub identifier: WithSpan<Identifier>,
    pub expr: WithSpan<Expr>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum UnaryOperator {
    Minus,
    Bang,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum BinaryOperator {
    Slash,
    Star,
    Plus,
    Minus,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    BangEqual,
    EqualEqual,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub enum LogicalOperator {
    And,
    Or,
}
