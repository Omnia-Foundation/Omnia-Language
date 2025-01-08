use std::collections::hash_map::Values;
use std::io::Stderr;
use std::ops::RangeBounds;
use crate::lexer::token::TokenType::{ARITHMETIC_E, ARITHMETIC_S, ASSIGNMENT_E, ASSIGNMENT_S, COND_E, COND_S, EOF, KEYWORDS_E, KEYWORDS_S, MINUS, OPERATORS_E, OPERATORS_S, OTHERS_E, OTHERS_S, STDDATATYPES_E, STDDATATYPES_S, UNARY_E, UNARY_S};

#[derive(PartialOrd, PartialEq, Clone, Debug)]
pub enum TokenType {
    OPERATORS_S,
    ARITHMETIC_S,
    STAR,       // *
    SLASH,      // /
    PLUS,       // +
    MINUS,      // -
    REM,        // %
    ARITHMETIC_E,
    AND,        // &&
    OR,         // ||
    COND_S,
    LS,         // <
    GT,         // >
    EQ,         // ==
    NEQ,        // !=
    LEQ,        // <=
    GEQ,        // >=
    COND_E,
    ASSIGNMENT_S,
    ASSIGN,     // =
    ANDASSIGN,  // &=
    ORASSIGN,   // |=
    PLUSASSIGN, // +=
    MINUSASSIGN,// -=
    MULASSIGN,  // *=
    DIVASSIGN,  // /=
    REMASSIGN,  // %=
    ASSIGNMENT_E,
    UNARY_S,
    AMPERSAND,  // &
    NOT,        // !
    INC,        // ++
    DEC,        // --
    UNARY_E,
    ACCESS,     // ::
    ARROW,      // ->
    DOLLAR,     // $
    FEQ,        // =!
    LAMBDA,     // >>
    POWER,      // ^
    OPERATORS_E,

    OTHERS_S,
    LPAREN,     // (
    LBRACK,     // [
    LBRACE,     // {
    COMMA,      // ,
    PERIOD,     // .
    UNDERSCORE, // _

    RPAREN,     // )
    RBRACK,     // ]
    RBRACE,     // }
    SEMICOLON,  // ;
    COLON,      // :
    QUESTION,   // ?
    OTHERS_E,

    STDDATATYPES_S,
    INT,
    UINT,
    BYTE,
    UBYTE,
    LONG,
    ULONG,
    DECIMAL,
    PTR,
    STRING,
    IDENT,
    CHAR,
    BOOL,
    NULL,
    ANY,
    STDDATATYPES_E,

    KEYWORDS_S,
    STATIC,
    ANYKW,
    NULLKW,
    REQUIRES,
    WAITS,
    ALLOWS,
    PLUG,
    VISIBLE,
    STRUCT,
    EXT,
    FUNC,
    FOR,
    OPEN,
    OVERRIDE,
    MK,
    EXPORT,
    HIDDEN,
    IF,
    ELSE,
    SWITCH,
    DISRUPT,
    SKIP,
    RETURN,
    STRINGKW,
    CHARKW,
    BOOLKW,
    INTKW,
    UINTKW,
    BYTEKW,
    UBYTEKW,
    LONGKW,
    ULONGKW,
    DECIMALKW,
    KEYWORDS_E,

    EOF
}
impl TokenType {
    pub fn is_keyword(t: &TokenType) -> bool {
        (KEYWORDS_S..KEYWORDS_E).contains(t)
    }
    pub fn is_stddatatype(t: &TokenType) -> bool {
        (STDDATATYPES_S..STDDATATYPES_E).contains(t)
    }
    pub fn is_operator(t: &TokenType) -> bool {
        (OPERATORS_S..OPERATORS_E).contains(t)
    }
    pub fn is_arithmetic_operator(t: &TokenType) -> bool { (ARITHMETIC_S..ARITHMETIC_E).contains(t) }
    pub fn is_unary_operator(t: &TokenType) -> bool { (UNARY_S..UNARY_E).contains(t) || t == &MINUS }
    pub fn is_conditional_operator(t: &TokenType) -> bool { (COND_S..COND_E).contains(t) }
    pub fn is_assignment_operator(t: &TokenType) -> bool { (ASSIGNMENT_S..ASSIGNMENT_E).contains(t) }
    pub fn is_eof(t: &TokenType) -> bool {
        &EOF == t
    }
    pub fn is_other(t: &TokenType) -> bool {
        (OTHERS_S..OTHERS_E).contains(t)
    }
}

#[derive(Clone, PartialEq)]
pub struct Token {
    pub t_type: TokenType,
    pub t_value: String,
    pub t_pos: usize
}

impl Token {
    pub fn new(token_type: TokenType, value: String, pos: usize) -> Token {
        Self {
            t_type: token_type,
            t_value: value,
            t_pos: pos
        }
    }
    pub fn to_string(&mut self) -> String {
        format!("Token with type [ {:?} ] value [ {} ] pos [ {} ]", self.t_type, self.t_value, self.t_pos)
    }
}