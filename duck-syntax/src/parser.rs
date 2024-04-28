use crate::ast::Expr::{Grouping, Unary};
use crate::ast::{
    Ast, BinaryOperator, Expr, InstanceParam, LogicalOperator, Mutability, Postcondition,
    Precondition, UnaryOperator,
};
use crate::ast::{Identifier, Stmt};
use crate::scanner::{Position, WithSpan};
use crate::token::{Token, TokenType};
use std::fmt::Display;
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;

use crate::errors::ParserError;
use anyhow::Result;

pub type Diagnostic = Vec<SyntaxError>;

#[derive(Debug, Clone)]
pub struct SyntaxError {
    error: ParserError,
    line: u32,
    pos: u32,
}

impl Display for SyntaxError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        write!(f, "{} at line {}:{}", self.error, self.line, self.pos - 1)
    }
}

impl SyntaxError {
    pub fn new(error: ParserError, token: &Token) -> Self {
        SyntaxError {
            error,
            line: token.pos.line,
            pos: token.pos.end.0,
        }
    }
}

type ExprResult = Result<WithSpan<Expr>, ()>;
type StmtResult = Result<WithSpan<Stmt>, ()>;

pub struct Parser<'a> {
    tokens: &'a [Token],
    cursor: usize,
    errors: Diagnostic,
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
enum Precedence {
    None,
    Assign, // =
    Or,
    And,
    Equality,   // == !=
    Comparison, // < <= > >=
    Term,       // + -
    Factor,     // * /
    Unary,      // ! -
    Call,       // ()
                //Primary,
}

impl<'a> From<TokenType> for Precedence {
    fn from(token: TokenType) -> Precedence {
        match token {
            TokenType::Equal => Precedence::Assign,
            TokenType::Or => Precedence::Or,
            TokenType::And => Precedence::And,
            TokenType::BangEqual | TokenType::EqualEqual => Precedence::Equality,
            TokenType::Less
            | TokenType::LessEqual
            | TokenType::Greater
            | TokenType::GreaterEqual => Precedence::Comparison,
            TokenType::Plus | TokenType::Minus => Precedence::Term,
            TokenType::Star | TokenType::Slash => Precedence::Factor,
            TokenType::Bang => Precedence::Unary, // Minus is already specified, but I think this is only for infix ops
            TokenType::LeftParen => Precedence::Call,
            TokenType::Dot => Precedence::Call,
            _ => Precedence::None,
        }
    }
}

static EOF_TOKEN: Token = Token {
    token_type: TokenType::Eof,
    pos: Position::default(),
};

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        if tokens.len() == 0 {
            panic!("No tokens to scan.");
        }
        Parser {
            tokens,
            cursor: 0,
            errors: Vec::new(),
        }
    }

    fn advance(&mut self) -> &'a Token {
        match self.tokens.get(self.cursor) {
            Some(token) => {
                self.cursor += 1;
                &token
            }
            None => &EOF_TOKEN,
        }
    }

    fn check(&self, token_type: TokenType) -> bool {
        self.peek().token_type == token_type
    }

    pub fn expect(&mut self, expected: TokenType) -> Result<&'a Token, ()> {
        let token = self.advance();
        if token.token_type == expected {
            Ok(token)
        } else {
            self.errors.push(SyntaxError::new(
                ParserError::Expected {
                    expected: expected.to_string(),
                    found: token.to_string(),
                },
                token,
            ));
            Err(())
        }
    }

    fn expect_identifier(&mut self) -> Result<WithSpan<Identifier>, ()> {
        let token = self.advance().clone();
        let identifier = match &token.token_type {
            TokenType::Identifier(identifier) => WithSpan::new(identifier.clone(), self.peek().pos),
            _ => {
                self.error(
                    ParserError::Expected {
                        expected: "Identifier".to_string(),
                        found: token.to_string(),
                    },
                    &token,
                );
                return Err(());
            }
        };

        Ok(identifier)
    }

    fn peek(&self) -> &Token {
        match self.tokens.get(self.cursor) {
            Some(token) => &token,
            None => &EOF_TOKEN,
        }
    }

    fn is_at_end(&self) -> bool {
        self.peek() == &EOF_TOKEN
    }

    fn error(&mut self, error: ParserError, token: &Token) {
        self.errors.push(SyntaxError::new(error, token));
    }

    pub fn parse(&mut self) -> Result<Ast, Diagnostic> {
        let mut ast: Ast = Vec::new();
        while !self.is_at_end() {
            let stmt = self.parse_stmt();
            match stmt {
                Ok(stmt) => {
                    ast.push(stmt);
                }
                Err(_) => {
                    return Err(self.errors.clone());
                }
            }
        }

        Ok(ast)
    }

    pub fn export(&mut self, destination: &impl AsRef<Path>) -> Result<(), Diagnostic> {
        let ast = self.parse()?;
        let exported_ast = serde_json::to_string(&ast).unwrap();
        let mut output = File::create(destination).unwrap();
        write!(output, "{}", exported_ast).unwrap();
        Ok(())
    }

    fn parse_stmt(&mut self) -> StmtResult {
        match &self.peek().token_type {
            TokenType::Const | TokenType::Mut => self.declaration(),
            TokenType::Print => self.print_stmt(),
            TokenType::LeftBrace => self.parse_block(),
            TokenType::If => self.parse_if(),
            TokenType::While => self.parse_while(),
            TokenType::Fn => self.parse_fun(),
            TokenType::Return => self.parse_return(),
            _ => self.parse_expr(),
        }
    }

    fn parse_return(&mut self) -> StmtResult {
        self.expect(TokenType::Return)?;
        let mut span = Position::default();
        if self.check(TokenType::Semicolon) {
            self.expect(TokenType::Semicolon)?;
            return Ok(WithSpan::new(Stmt::Return(None), span));
        }
        let expr = self.expression(Precedence::None)?;
        span = expr.span;
        self.expect(TokenType::Semicolon)?;
        Ok(WithSpan::new(Stmt::Return(Some(Box::new(expr))), span))
    }

    fn parse_fun(&mut self) -> StmtResult {
        self.expect(TokenType::Fn)?;
        let fun_identifier = self.expect_identifier()?;
        let mut span = fun_identifier.span;
        self.expect(TokenType::LeftParen)?;
        let mut params: Vec<WithSpan<Identifier>> = Vec::new();
        if !self.check(TokenType::RightParen) {
            params.push(self.expect_identifier()?);
            while self.check(TokenType::Comma) {
                self.expect(TokenType::Comma)?;
                let identifier = self.expect_identifier()?;
                span = span.union(identifier.span);
                params.push(identifier);
            }
        }
        self.expect(TokenType::RightParen)?;
        let mut preconditions: Vec<WithSpan<Precondition>> = Vec::new();
        let mut postconditions: Vec<WithSpan<Postcondition>> = Vec::new();
        if self.check(TokenType::Colons) {
            self.expect(TokenType::Colons)?;
            // parse contract
            while self.check(TokenType::Require) || self.check(TokenType::Ensure) {
                let assumption = self.advance();
                match assumption.token_type {
                    TokenType::Require => {
                        preconditions.push(self.parse_require(&assumption)?);
                    }
                    TokenType::Ensure => {
                        postconditions.push(self.parse_ensure(&assumption)?);
                    }
                    _ => return Err(()),
                }
                if self.check(TokenType::Comma) {
                    self.expect(TokenType::Comma)?;
                } else {
                    break;
                }
            }
        }
        let mut fun_body: Vec<WithSpan<Stmt>> = Vec::new();
        self.expect(TokenType::LeftBrace)?;
        while !self.check(TokenType::RightBrace) {
            let stmt = self.parse_stmt()?;
            span = span.union(stmt.span);
            fun_body.push(stmt);
        }
        self.expect(TokenType::RightBrace)?;
        Ok(WithSpan::new(
            Stmt::Function(
                fun_identifier,
                preconditions,
                postconditions,
                params,
                Box::new(fun_body),
            ),
            span,
        ))
    }

    fn parse_require(&mut self, assumption: &Token) -> Result<WithSpan<Precondition>, ()> {
        let mut span = assumption.pos;
        let expr = self.expression(Precedence::None)?;
        span = span.union(expr.span);
        Ok(WithSpan::new(Precondition::Require(Box::new(expr)), span))
    }

    fn parse_ensure(&mut self, assumption: &Token) -> Result<WithSpan<Postcondition>, ()> {
        let mut span = assumption.pos;
        let expr = self.expression(Precedence::None)?;
        span = span.union(expr.span);
        Ok(WithSpan::new(Postcondition::Ensure(Box::new(expr)), span))
    }

    fn parse_arguments(&mut self) -> Result<Vec<WithSpan<Expr>>, ()> {
        let mut args = Vec::new();
        if !self.check(TokenType::RightParen) {
            args.push(self.expression(Precedence::None)?);
            while self.check(TokenType::Comma) {
                self.expect(TokenType::Comma)?;
                args.push(self.expression(Precedence::None)?);
            }
        }
        Ok(args)
    }

    fn parse_while(&mut self) -> StmtResult {
        self.expect(TokenType::While)?;
        // parse condition
        self.expect(TokenType::LeftParen)?;
        let condition = self.expression(Precedence::None)?;
        self.expect(TokenType::RightParen)?;
        let while_stmt = self.parse_stmt()?;
        let span = condition.span.union(while_stmt.span);
        Ok(WithSpan::new(
            Stmt::While(Box::new(condition), Box::new(while_stmt)),
            span,
        ))
    }

    fn parse_if(&mut self) -> StmtResult {
        self.expect(TokenType::If)?;
        // parse condition
        self.expect(TokenType::LeftParen)?;
        let condition = self.expression(Precedence::None)?;
        self.expect(TokenType::RightParen)?;
        // parse then statements
        let if_stmt = self.parse_stmt()?;
        let mut span = condition.span.union(if_stmt.span);
        let else_stmt: Option<Box<WithSpan<Stmt>>> = {
            if self.check(TokenType::Else) {
                self.expect(TokenType::Else)?;
                let else_stmt = self.parse_stmt()?;
                span = span.union(else_stmt.span);
                Some(Box::new(else_stmt))
            } else {
                None
            }
        };
        // parse else statements
        Ok(WithSpan::new(
            Stmt::If(Box::new(condition), Box::new(if_stmt), else_stmt),
            span,
        ))
    }

    fn declaration(&mut self) -> StmtResult {
        let token = self.peek().clone();
        match token.token_type {
            TokenType::Const => self.const_declaration(),
            TokenType::Mut => self.mut_declaration(),
            _ => {
                self.error(
                    ParserError::ExpectedDeclaration {
                        found: token.to_string(),
                    },
                    &token,
                );
                Err(())
            }
        }
    }

    fn parse_block(&mut self) -> StmtResult {
        self.expect(TokenType::LeftBrace)?;
        let mut statements: Vec<WithSpan<Stmt>> = Vec::new();
        let mut block_span = Position::default();
        while !self.check(TokenType::RightBrace) {
            let stmt = self.parse_stmt()?;
            block_span = block_span.union(stmt.span);
            statements.push(stmt);
        }
        self.expect(TokenType::RightBrace)?;
        Ok(WithSpan::new(Stmt::Block(statements), block_span))
    }

    fn const_declaration(&mut self) -> StmtResult {
        self.expect(TokenType::Const)?;
        let token = self.advance();
        self.var_declaration(token, Mutability::Const)
    }

    fn mut_declaration(&mut self) -> StmtResult {
        self.expect(TokenType::Mut)?;
        let token = self.advance();
        self.var_declaration(token, Mutability::Mut)
    }

    fn var_declaration(&mut self, token: &Token, mutability: Mutability) -> StmtResult {
        match &token.token_type {
            TokenType::Identifier(identifier) => {
                self.expect(TokenType::Equal)?;
                let expr = self.expression(Precedence::None)?;
                let declaration_span = token.pos.union(expr.span);
                let var = WithSpan::new(
                    Stmt::Var(
                        WithSpan::new(identifier.clone(), token.pos.clone()),
                        Some(Box::new(expr)),
                        mutability,
                    ),
                    declaration_span,
                );
                self.expect(TokenType::Semicolon)?;
                Ok(var)
            }
            _ => {
                self.error(
                    ParserError::ExpectedDeclaration {
                        found: self.peek().to_string(),
                    },
                    token,
                );
                Err(())
            }
        }
    }

    fn print_stmt(&mut self) -> StmtResult {
        self.expect(TokenType::Print)?;
        // only expressions are printable
        let expr = self.expression(Precedence::None)?;
        let span = expr.span.clone();
        self.expect(TokenType::Semicolon)?;

        Ok(WithSpan::new(Stmt::Print(Box::new(expr)), span))
    }

    fn parse_expr(&mut self) -> StmtResult {
        let expr = self.expression(Precedence::None)?;
        let span = expr.span.clone();
        self.expect(TokenType::Semicolon)?;
        Ok(WithSpan::new(Stmt::Expr(Box::new(expr)), span))
    }

    fn expression(&mut self, precedence: Precedence) -> ExprResult {
        let mut expr = self.parse_prefix()?; // parsing unary, grouping ...
        while !self.is_at_end() {
            // stop until a lower precedence or end of file
            let next_precedence = Precedence::from(self.peek().token_type.clone());
            if precedence >= next_precedence {
                // next precedence is lower, stop
                break;
            }
            expr = self.parse_infix(expr)?; // paring binary, comparisons ecc...
        }
        Ok(expr)
    }

    fn parse_infix(&mut self, left: WithSpan<Expr>) -> ExprResult {
        let token = self.peek().clone();
        match token.token_type {
            TokenType::Less
            | TokenType::Greater
            | TokenType::LessEqual
            | TokenType::GreaterEqual
            | TokenType::BangEqual
            | TokenType::EqualEqual
            | TokenType::Plus
            | TokenType::Minus
            | TokenType::Slash
            | TokenType::Star => self.parse_binary(left),
            TokenType::And | TokenType::Or => self.parse_logical(left),
            TokenType::Equal => self.parse_assign(left),
            TokenType::LeftParen => self.parse_call(left),
            TokenType::Dot => self.parse_dot(left),
            _ => {
                self.error(
                    ParserError::UnmatchedInfix {
                        found: token.to_string(),
                    },
                    &token,
                );
                Err(())
            }
        }
    }

    fn parse_dot(&mut self, left: WithSpan<Expr>) -> ExprResult {
        self.expect(TokenType::Dot)?;
        let property_identifier = self.expect_identifier()?;
        let span = property_identifier.span;
        Ok(WithSpan::new(
            Expr::Get(Box::new(left), property_identifier),
            span,
        ))
    }

    fn parse_call(&mut self, left: WithSpan<Expr>) -> ExprResult {
        self.expect(TokenType::LeftParen)?;
        let arguments = self.parse_arguments()?;
        let right = self.expect(TokenType::RightParen)?;
        let span = left.span.union(right.pos);
        Ok(WithSpan::new(Expr::Call(Box::new(left), arguments), span))
    }

    fn parse_assign(&mut self, left: WithSpan<Expr>) -> ExprResult {
        self.expect(TokenType::Equal)?;
        let right = self.expression(Precedence::None)?;
        let span = left.span.union(right.span);
        match &left.value {
            Expr::Variable(var) => {
                // it's a variable, do an assign
                Ok(WithSpan::new(
                    Expr::Assign(var.clone(), Box::new(right)),
                    span,
                ))
            }
            Expr::Get(expr, identifier) => Ok(WithSpan::new(
                Expr::Set(expr.clone(), identifier.clone(), Box::new(right)),
                span,
            )),
            _ => unimplemented!(
                "Get expression not yet implemented for Expr: {:?}",
                &left.value
            ), // here the "get" of a property from an instance of a Class
        }
    }

    fn parse_binary(&mut self, left: WithSpan<Expr>) -> ExprResult {
        let precedence = Precedence::from(self.peek().token_type.clone());
        let token = self.advance();
        let binary_operator = match token.token_type {
            TokenType::Less => BinaryOperator::Less,
            TokenType::Slash => BinaryOperator::Slash,
            TokenType::Star => BinaryOperator::Star,
            TokenType::Plus => BinaryOperator::Plus,
            TokenType::Minus => BinaryOperator::Minus,
            TokenType::Greater => BinaryOperator::Greater,
            TokenType::GreaterEqual => BinaryOperator::GreaterEqual,
            TokenType::LessEqual => BinaryOperator::LessEqual,
            TokenType::BangEqual => BinaryOperator::BangEqual,
            TokenType::EqualEqual => BinaryOperator::EqualEqual,
            _ => {
                self.error(
                    ParserError::UnmatchedInfix {
                        found: token.to_string(),
                    },
                    token,
                );
                return Err(());
            }
        };
        let right = self.expression(precedence)?;

        let binary_span = left.span.union(right.span);
        Ok(WithSpan::new(
            Expr::Binary(
                Box::new(left),
                WithSpan::new(binary_operator, token.pos),
                Box::new(right),
            ),
            binary_span,
        ))
    }

    fn parse_logical(&mut self, left: WithSpan<Expr>) -> ExprResult {
        let precedence = Precedence::from(self.peek().token_type.clone());
        let token = self.advance();
        let operator = match token.token_type {
            TokenType::Or => LogicalOperator::Or,
            TokenType::And => LogicalOperator::And,
            _ => {
                self.error(
                    ParserError::ExpectedLogical {
                        found: token.to_string(),
                    },
                    token,
                );
                return Err(());
            }
        };
        let right = self.expression(precedence)?;

        let span = left.span.union(right.span);
        Ok(WithSpan::new(
            Expr::Logical(
                Box::new(left),
                WithSpan::new(operator, token.pos),
                Box::new(right),
            ),
            span,
        ))
    }

    fn parse_prefix(&mut self) -> ExprResult {
        let token = self.peek().clone();
        match token.token_type {
            TokenType::Number(_)
            | TokenType::Null
            | TokenType::True
            | TokenType::False
            | TokenType::Identifier(_)
            | TokenType::Ret
            | TokenType::String(_) => self.parse_primary(),
            TokenType::Hash => self.parse_data_instance(),
            TokenType::Bang | TokenType::Minus => self.parse_unary(),
            TokenType::LeftParen => self.parse_grouping(),
            _ => {
                self.error(
                    ParserError::ExpectedPrefix {
                        found: token.clone().to_string(),
                    },
                    &token,
                );
                Err(())
            }
        }
    }

    fn parse_grouping(&mut self) -> ExprResult {
        let left_paren = self.expect(TokenType::LeftParen)?;
        let expr = self.expression(Precedence::None)?;
        let right_paren = self.expect(TokenType::RightParen)?;
        let span_grouping = right_paren.pos.union(left_paren.pos);

        Ok(WithSpan::new(Grouping(Box::new(expr)), span_grouping))
    }

    fn parse_unary(&mut self) -> ExprResult {
        let token = self.advance();
        let op = match &token.token_type {
            TokenType::Bang => Ok(WithSpan::new(UnaryOperator::Bang, token.pos)),
            TokenType::Minus => Ok(WithSpan::new(UnaryOperator::Minus, token.pos)),
            _ => {
                self.error(
                    ParserError::ExpectedUnary {
                        found: token.to_string(),
                    },
                    token,
                );
                Err(())
            }
        }?;
        let right = self.expression(Precedence::Unary)?;
        // the span of the unary is from the very start to the last point of the parsed right expression
        let union_span = right.span.union(token.pos);

        Ok(WithSpan::new(Unary(op, Box::new(right)), union_span))
    }

    fn parse_data_instance(&mut self) -> ExprResult {
        self.expect(TokenType::Hash)?;
        self.expect(TokenType::LeftBrace)?;
        let mut constructor_params: Vec<WithSpan<InstanceParam>> = Vec::new();
        while !self.check(TokenType::RightBrace) {
            let param_name = self.expect_identifier()?;
            self.expect(TokenType::Colons)?;
            let param_expr = self.expression(Precedence::None)?;
            let span = param_expr.span;
            if !self.check(TokenType::RightBrace) {
                self.expect(TokenType::Comma)?;
            }
            constructor_params.push(WithSpan::new(
                InstanceParam {
                    identifier: param_name,
                    expr: param_expr,
                },
                span,
            ));
        }
        let end_token = self.expect(TokenType::RightBrace)?;
        constructor_params.sort_by(|a, b| {
            a.value
                .identifier
                .value
                .partial_cmp(&b.value.identifier.value)
                .unwrap()
        });
        Ok(WithSpan {
            span: end_token.pos,
            value: Expr::Instance(constructor_params),
        })
    }

    fn parse_primary(&mut self) -> ExprResult {
        let token = self.advance();
        match &token.token_type {
            TokenType::Number(n) => Ok(WithSpan::new(Expr::Number(*n), token.pos)),
            TokenType::String(s) => Ok(WithSpan::new(Expr::String(s.clone()), token.pos)),
            TokenType::Null => Ok(WithSpan::new(Expr::Nil, token.pos)),
            TokenType::False => Ok(WithSpan::new(Expr::Boolean(false), token.pos)),
            TokenType::True => Ok(WithSpan::new(Expr::Boolean(true), token.pos)),
            TokenType::Ret => Ok(WithSpan::new(Expr::Ret, token.pos)),
            TokenType::Identifier(identifier) => Ok(WithSpan::new(
                Expr::Variable(WithSpan::new(identifier.clone(), token.pos)),
                token.pos,
            )),
            _ => {
                self.error(
                    ParserError::ExpectedPrimary {
                        found: token.to_string(),
                    },
                    token,
                );
                Err(())
            }
        }
    }
}
