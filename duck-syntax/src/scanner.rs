use crate::token::Token;
use crate::token::TokenType;
use serde::{Deserialize, Serialize};
use std::cmp::{max, min};
use std::fmt::Display;
use std::iter::Peekable;
use std::str::Chars;

#[derive(Serialize, Deserialize, Debug, Clone, Copy, PartialEq)]
pub struct CharPos(pub u32);

impl Display for CharPos {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        write!(f, "{}", self.0)
    }
}

impl CharPos {
    fn advance(&mut self, c: char) {
        self.0 += c.len_utf8() as u32;
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, Copy, PartialEq)]
pub struct Position {
    pub start: CharPos,
    pub end: CharPos,
    pub line: u32,
}

impl Position {
    pub const fn default() -> Position {
        Position {
            start: CharPos(0),
            end: CharPos(0),
            line: 0,
        }
    }

    pub fn union(&self, span: Position) -> Position {
        Position {
            start: CharPos(min(self.start.0, span.start.0)),
            end: CharPos(max(self.end.0, self.end.0)),
            line: self.line,
        }
    }
}

impl Display for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        write!(f, "{} {}", self.start, self.end)
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, Copy, PartialEq)]
pub struct WithSpan<T> {
    pub span: Position,
    pub value: T,
}

impl<T> WithSpan<T> {
    pub fn new(value: T, span: Position) -> Self {
        WithSpan { value, span }
    }
}

pub struct Scanner<'a> {
    pos: CharPos,
    // the current char of the code is stored as an iterator over chars
    it: Peekable<Chars<'a>>,
    line: u32,
}

impl<'a> Scanner<'a> {
    pub fn new(code: &str) -> Scanner {
        Scanner {
            pos: CharPos(0),
            it: code.chars().peekable(),
            line: 1,
        }
    }

    pub fn tokens(&mut self) -> Vec<Token> {
        let mut tokens: Vec<Token> = Vec::new();
        loop {
            let token = self.next_token();
            if token.token_type == TokenType::Eof {
                break;
            }
            tokens.push(token);
        }
        tokens
    }

    pub fn next(&mut self) -> Option<char> {
        let next_char = self.it.next();
        match next_char {
            Some(c) => {
                self.pos.advance(c);
            }
            None => {
                // do nothing
            }
        }
        next_char
    }

    pub fn peek(&mut self) -> Option<&char> {
        self.it.peek()
    }

    fn make_token(&self, token_type: TokenType, len: u32) -> Token {
        let start = self.pos.clone().0;
        Token {
            token_type,
            pos: Position {
                start: CharPos(start),
                end: CharPos(start + len),
                line: self.line,
            },
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.consume_while(|c| c == ' ' || c == '\r' || c == '\t'); // skip whitespace
        let next_char = self.next();
        if let Some(c) = next_char {
            let len = c.len_utf8() as u32;
            match c {
                '(' => return self.make_token(TokenType::LeftParen, len),
                ')' => return self.make_token(TokenType::RightParen, len),
                '{' => return self.make_token(TokenType::LeftBrace, len),
                '}' => return self.make_token(TokenType::RightBrace, len),
                ';' => return self.make_token(TokenType::Semicolon, len),
                ',' => return self.make_token(TokenType::Comma, len),
                '.' => return self.make_token(TokenType::Dot, len),
                '-' => return self.make_token(TokenType::Minus, len),
                '+' => return self.make_token(TokenType::Plus, len),
                '#' => return self.make_token(TokenType::Hash, len),
                '/' => {
                    if let Some(_) = self.consume_if(|ch| ch == '/') {
                        self.consume_while(|ch| ch != '\n');
                        return self.next_token();
                    } else {
                        return self.make_token(TokenType::Slash, len);
                    }
                }
                '*' => return self.make_token(TokenType::Star, len),
                ':' => return self.make_token(TokenType::Colons, len),
                '!' => {
                    if let Some(_) = self.consume_if(|ch| ch == '=') {
                        return self.make_token(TokenType::BangEqual, len);
                    } else {
                        return self.make_token(TokenType::Bang, len);
                    }
                }
                '=' => {
                    if let Some(_) = self.consume_if(|ch| ch == '=') {
                        return self.make_token(TokenType::EqualEqual, len);
                    } else {
                        return self.make_token(TokenType::Equal, len);
                    }
                }
                '<' => {
                    if let Some(_) = self.consume_if(|ch| ch == '=') {
                        return self.make_token(TokenType::LessEqual, len);
                    } else {
                        return self.make_token(TokenType::Less, len);
                    }
                }
                '>' => {
                    if let Some(_) = self.consume_if(|ch| ch == '=') {
                        return self.make_token(TokenType::GreaterEqual, len);
                    } else {
                        return self.make_token(TokenType::Greater, len);
                    }
                }
                '"' => {
                    let string: String = self.consume_while(|ch| ch != '"').into_iter().collect();
                    // next is to skip last "
                    match self.next() {
                        Some(_) => return self.make_token(TokenType::String(string), len),
                        None => return self.make_token(TokenType::UnterminatedString, len),
                    }
                }
                '@' => return self.contract_keyword(),
                c if c.is_numeric() => return self.number(c),
                c if c.is_ascii_alphanumeric() => return self.identifier(c),
                '\n' => {
                    self.line += 1;
                    self.pos = CharPos(0);
                    return self.next_token();
                }
                _ => {
                    panic!("Unexpected character '{:?}'", c);
                }
            }
        }
        self.make_token(TokenType::Eof, 0)
    }

    fn number(&mut self, c: char) -> Token {
        let mut number_as_string = String::new();
        number_as_string.push(c);
        let num: String = self
            .consume_while(|ch| ch.is_numeric())
            .into_iter()
            .collect();
        number_as_string.push_str(num.as_str());
        if let Some(is_dot) = self.peek() {
            if is_dot.clone() == '.' {
                // parse decimal part
                number_as_string.push_str(".");
                let decimal: String = self
                    .consume_while(|ch| ch.is_numeric())
                    .into_iter()
                    .collect();
                number_as_string.push_str(decimal.as_str());
            }
        }
        self.make_token(
            TokenType::Number(number_as_string.parse::<f64>().unwrap()),
            c.len_utf8() as u32,
        )
    }

    fn identifier(&mut self, c: char) -> Token {
        let mut identifier = String::new();
        identifier.push(c);
        let chars: String = self
            .consume_while(|ch| ch.is_ascii_alphanumeric() || ch == '_')
            .into_iter()
            .collect();
        let len = chars.len();
        identifier.push_str(chars.as_str());
        let keyword = self.keyword(&identifier);
        match keyword {
            Some(token) => token,
            None => self.make_token(TokenType::Identifier(identifier), len as u32),
        }
    }

    fn contract_keyword(&mut self) -> Token {
        let mut identifier = String::new();
        let chars: String = self
            .consume_while(|ch| ch.is_ascii_alphanumeric() || ch == '_')
            .into_iter()
            .collect();
        identifier.push_str(chars.as_str());
        let keyword = self.keyword(&identifier);
        match keyword {
            Some(token) => token,
            None => panic!("Missing contract keyword after '@'"),
        }
    }

    fn keyword(&mut self, identifier: &str) -> Option<Token> {
        // TODO do something better, please
        let len = identifier.bytes().len() as u32;
        match identifier {
            "and" => return Some(self.make_token(TokenType::And, len)),
            "else" => return Some(self.make_token(TokenType::Else, len)),
            "false" => return Some(self.make_token(TokenType::False, len)),
            "for" => return Some(self.make_token(TokenType::For, len)),
            "fn" => return Some(self.make_token(TokenType::Fn, len)),
            "if" => return Some(self.make_token(TokenType::If, len)),
            "null" => return Some(self.make_token(TokenType::Null, len)),
            "or" => return Some(self.make_token(TokenType::Or, len)),
            "print" => return Some(self.make_token(TokenType::Print, len)),
            "return" => return Some(self.make_token(TokenType::Return, len)),
            "ret" => return Some(self.make_token(TokenType::Ret, len)),
            "true" => return Some(self.make_token(TokenType::True, len)),
            "const" => return Some(self.make_token(TokenType::Const, len)),
            "mut" => return Some(self.make_token(TokenType::Mut, len)),
            "while" => return Some(self.make_token(TokenType::While, len)),
            "import" => return Some(self.make_token(TokenType::Import, len)),
            "ensure" => return Some(self.make_token(TokenType::Ensure, len)),
            "require" => return Some(self.make_token(TokenType::Require, len)),
            "data" => return Some(self.make_token(TokenType::Data, len)),
            _ => return None,
        };
    }

    fn consume_while<F>(&mut self, cb: F) -> Vec<char>
    where
        F: Fn(char) -> bool,
    {
        let mut chars = Vec::new();
        while let Some(&c) = self.peek() {
            if cb(c) {
                chars.push(c);
                self.next();
            } else {
                break;
            }
        }
        chars
    }

    fn consume_if<F>(&mut self, cb: F) -> Option<char>
    where
        F: Fn(char) -> bool,
    {
        if let Some(&c) = self.peek() {
            if cb(c) {
                return self.next();
            }
        }
        None
    }
}
