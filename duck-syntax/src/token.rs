use crate::scanner::Position;
use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    Colons,
    Hash,

    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals.
    Identifier(String),
    String(String),
    Number(f64),

    // Keywords.
    And,
    Else,
    False,
    Fn,
    For,
    If,
    Null,
    Nullable,
    Or,
    Print,
    Return,
    Ret, // used on contract to identify the value returned from a function
    True,
    Const,
    Mut, // explicit mutability
    While,
    Import,
    Data,

    // Other.
    Eof,
    UnterminatedString,
    Unknown(char),

    // Contract keywords
    Require, //precondition
    Ensure,  // postcondition
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub pos: Position,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        write!(f, "{}", self.token_type)
    }
}

impl TokenType {
    pub fn to_string(&self) -> String {
        format!("{}", self)
    }
}

impl Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        write!(
            f,
            "{}",
            match self {
                TokenType::LeftParen => "'('",
                TokenType::RightParen => "')'",
                TokenType::LeftBrace => "'{'",
                TokenType::RightBrace => "'}'",
                TokenType::Comma => "','",
                TokenType::Colons => "':'",
                TokenType::Dot => "'.'",
                TokenType::Minus => "'-'",
                TokenType::Plus => "'+'",
                TokenType::Semicolon => "';'",
                TokenType::Slash => "'/'",
                TokenType::Star => "'*'",
                TokenType::Bang => "'!'",
                TokenType::BangEqual => "'!='",
                TokenType::Equal => "'='",
                TokenType::EqualEqual => "'=='",
                TokenType::Greater => "'>'",
                TokenType::GreaterEqual => "'>='",
                TokenType::Less => "'<'",
                TokenType::LessEqual => "'<='",
                TokenType::Identifier(_) => "identifier",
                TokenType::String(_) => "string",
                TokenType::Number(_) => "number",
                TokenType::And => "'and'",
                TokenType::Else => "'else'",
                TokenType::False => "'false'",
                TokenType::Fn => "'fn'",
                TokenType::For => "'for'",
                TokenType::If => "'if'",
                TokenType::Null => "null",
                TokenType::Nullable => "nullable",
                TokenType::Or => "'or'",
                TokenType::Print => "'print'",
                TokenType::Return => "'return'",
                TokenType::Ret => "'ret'",
                TokenType::True => "'true'",
                TokenType::Const => "'const'",
                TokenType::Mut => "'mut'",
                TokenType::While => "'while'",
                TokenType::Import => "'import'",
                TokenType::Eof => "<EOF>",
                TokenType::UnterminatedString => "<Unterminated String>",
                TokenType::Require => "'require'",
                TokenType::Ensure => "'ensure'",
                TokenType::Data => "'data'",
                TokenType::Hash => "'#'",
                TokenType::Unknown(_) => "<Unknown>",
            }
        )
    }
}
