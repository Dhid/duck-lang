use displaydoc::Display;
use thiserror::Error;
use crate::token::TokenType;

#[derive(Error, Debug, Display, Clone)]
pub enum ParserError {
    /// >>> Invalid char: expected {expected:?}, got {found:?}
    Expected { expected: String, found: String },

    /// >>> Expected declaration
    ExpectedDeclaration { found: String },

    /// >>> Unmatched infix: got {found:?}
    UnmatchedInfix { found: String },

    /// >>> Expected binary operation, got {found:?}
    ExpectedBinary { found: String },

    /// >>> Expected logical operation, got {found:?}
    ExpectedLogical { found: String },

    /// >>> Expected prefix, got {found:?}
    ExpectedPrefix { found: String },

    /// >>> Expected unary operation, got {found:?}
    ExpectedUnary { found: String },

    /// >>> Expected primary operation, got {found:?}
    ExpectedPrimary { found: String },

    /// >>> Error parsing: {message:?}
    Generic { message: String },

    /// >>> Invalid syntax for data property, found '{found:?}' expected 'const', 'mut' or identifier
    InvalidPropertySyntax { found: TokenType },
}
