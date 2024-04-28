use displaydoc::Display;
use duck_bytecode::bytecode::Instruction;
use thiserror::Error;

use super::memory::Value;

#[derive(Error, Debug, Display)]
pub enum VMError {
    /// >>> VM stack empty PC: {pc:?}
    StackEmpty { pc: usize },

    /// >>> VM stack overflow
    StackOverflow,

    /// >>> Invalid stack access at index {index:?}, stack size is {size:?}
    InvalidStackIndex { index: usize, size: usize },

    /// >>> VM error, instruction {instruction:?} not supported
    VMError { instruction: Instruction },

    /// >>> Identifier {identifier:?} not found
    IdentifierNotFound { identifier: String },

    /// >>> Identifier {identifier:?} not defined
    IdentifierNotDefined { identifier: String },

    /// >>> Unexpected value {value:?} for operation {operation:?}
    UnexpectedValueType { value: Value, operation: String },

    /// >>> Invalid first operand: {operand:?} for binary operation
    InvalidFirstOperandBinary { operand: Value },

    /// >>> Invalid second: {operand:?} operand for binary operation
    InvalidSecondOperandBinary { operand: Value },

    /// >>> Invalid callee, found: {callee:?}
    InvalidCallee { callee: Value },

    /// >>> Invalid property, found: {prop:?}
    InvalidProperty { prop: Value },

    /// >>> Invalid native function call
    InvalidNativeFunctionCall,

    /// >>> Contract not valid
    ContractNotValid,

    /// >>> Assumption not valid
    ExpectationNotValid,

    /// >>> Incorrect arity, expected {expected_arity:?} found {actual_arity:?}
    IncorrectArity {
        expected_arity: usize,
        actual_arity: usize,
    },

    /// >>> Undefined property {prop:?}
    UndefinedProperty { prop: String },

    /// >>> Not a valid object instance, found {found:?}
    NotAValidInstance { found: Value },

    /// Cannot import Module {path:?}
    CannotImportModule { path: String },

    /// Invalid division by zero
    DivByZero,
}
