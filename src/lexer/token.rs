use std::fmt::{Display, Formatter};
use std::ops::RangeBounds;
use num_traits::FromPrimitive;

#[derive(Clone, Debug, Copy, Ord, PartialOrd, Eq, PartialEq)]
pub enum TokenType {
    NumberLiteral { size: usize },
    Ident,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    PLUS,
    MINUS,
    ASTER,
    SLASH,
    UNDERSCORE,
    ASSIGN,
    EQ,
    GE,
    LE,
    NEQ,
    GT,
    LT,
    BPRINT,
    MK,
    IF,
    ELSE,
    STMT,
    EOF,
}

#[derive(Clone, Debug)]
pub struct Token {
    r#type: TokenType,
    value: String,
    pos: (usize, usize) //line, col
}
impl Token {
    pub fn new(r#type: TokenType, value: String, pos: (usize, usize)) -> Self {
        Self {
            r#type,
            value,
            pos
        }
    }
    pub fn get_type(&self) -> TokenType { self.r#type }
    pub fn get_pos(&self) -> (usize, usize) { self.pos }
    pub fn get_value_clone(&self) -> String { self.value.clone() }
    pub fn get_value(self) -> String { self.value }
    pub fn eof_token() -> Self {
        Self {
            r#type: TokenType::EOF,
            value: "\0".to_string(),
            pos: (9999, 9999)
        }
    }
    pub fn is_statement(&self) -> bool { (TokenType::BPRINT..TokenType::STMT).contains(&self.r#type) }
    pub fn is_expression(&self) -> bool { (TokenType::NumberLiteral{size: 1}..TokenType::Ident).contains(&self.r#type) }

}
impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Token[type: {:?}; value: {}; pos: {:?}", self.r#type, self.value, self.pos)
    }
}

impl Display for TokenType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenType::NumberLiteral { .. } => { write!(f, "NumberLiteral") }
            TokenType::Ident => { write!(f, "Ident") }
            TokenType::LPAREN => { write!(f, "(") }
            TokenType::RPAREN => { write!(f, ")") }
            TokenType::LBRACE => { write!(f, "{{") }
            TokenType::RBRACE => { write!(f, "}}") }
            TokenType::PLUS => { write!(f, "+") }
            TokenType::MINUS => { write!(f, "-") }
            TokenType::ASTER => { write!(f, "*") }
            TokenType::SLASH => { write!(f, "/") }
            TokenType::UNDERSCORE => { write!(f, "_") }
            TokenType::ASSIGN => { write!(f, "=") }
            TokenType::BPRINT => { write!(f, "__builtin_print") }
            TokenType::MK => { write!(f, "mk") }
            TokenType::STMT => { write!(f, "STMT") }
            TokenType::EOF => { write!(f, "EOF") }
            TokenType::EQ => { write!(f, "==") }
            TokenType::NEQ => { write!(f, "!=") }
            TokenType::GT => { write!(f, ">") }
            TokenType::LT => { write!(f, "<") }
            TokenType::GE => { write!(f, ">=") }
            TokenType::LE => { write!(f, "<=") }
            TokenType::IF => { write!(f, "if") }
            TokenType::ELSE => { write!(f, "else") }
        }
    }
}