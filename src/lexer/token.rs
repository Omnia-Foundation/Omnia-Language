use std::collections::hash_map::Values;
use std::fmt::{Display, Formatter};
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

impl Display for TokenType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenType::STAR => { write!(f, "*") }
            TokenType::SLASH => { write!(f, "/") }
            TokenType::PLUS => { write!(f, "+") }
            MINUS => { write!(f, "-") }
            TokenType::REM => { write!(f, "%") }
            TokenType::AND => { write!(f, "&&") }
            TokenType::OR => { write!(f, "||") }
            TokenType::LS => { write!(f, "<") }
            TokenType::GT => { write!(f, ">") }
            TokenType::EQ => { write!(f, "==") }
            TokenType::NEQ => { write!(f, "!=") }
            TokenType::LEQ => { write!(f, "<=") }
            TokenType::GEQ => { write!(f, ">=") }
            TokenType::ASSIGN => { write!(f, "=") }
            TokenType::ANDASSIGN => { write!(f, "&=") }
            TokenType::ORASSIGN => { write!(f, "|=") }
            TokenType::PLUSASSIGN => { write!(f, "+=") }
            TokenType::MINUSASSIGN => { write!(f, "-=") }
            TokenType::MULASSIGN => { write!(f, "*=") }
            TokenType::DIVASSIGN => { write!(f, "/=") }
            TokenType::REMASSIGN => { write!(f, "%=") }
            TokenType::AMPERSAND => { write!(f, "&") }
            TokenType::NOT => { write!(f, "!") }
            TokenType::INC => { write!(f, "++") }
            TokenType::DEC => { write!(f, "--") }
            TokenType::ACCESS => { write!(f, "::") }
            TokenType::ARROW => { write!(f, "->") }
            TokenType::DOLLAR => { write!(f, "$") }
            TokenType::FEQ => { write!(f, "=!") }
            TokenType::LAMBDA => { write!(f, ">>") }
            TokenType::POWER => { write!(f, "^") }
            TokenType::LPAREN => { write!(f, "(") }
            TokenType::LBRACK => { write!(f, "[") }
            TokenType::LBRACE => { write!(f, "{{") }
            TokenType::COMMA => { write!(f, ",") }
            TokenType::PERIOD => { write!(f, ".") }
            TokenType::UNDERSCORE => { write!(f, "_") }
            TokenType::RPAREN => { write!(f, ")") }
            TokenType::RBRACK => { write!(f, "]") }
            TokenType::RBRACE => { write!(f, "}}") }
            TokenType::SEMICOLON => { write!(f, ";") }
            TokenType::COLON => { write!(f, ":") }
            TokenType::QUESTION => { write!(f, "?") }
            TokenType::INT => { write!(f, "l::int") }
            TokenType::UINT => { write!(f, "l::uint") }
            TokenType::BYTE => { write!(f, "l::byte") }
            TokenType::UBYTE => { write!(f, "l::ubyte") }
            TokenType::LONG => { write!(f, "l::long") }
            TokenType::ULONG => { write!(f, "l::ulong") }
            TokenType::DECIMAL => { write!(f, "l::decimal") }
            TokenType::PTR => { write!(f, "l::pointer") }
            TokenType::IDENT => { write!(f, "l::ident") }
            TokenType::CHAR => { write!(f, "l::char") }
            TokenType::BOOL => { write!(f, "l::bool") }
            TokenType::NULL => { write!(f, "l::null") }
            TokenType::ANY => { write!(f, "l::any") }
            TokenType::STATIC => { write!(f, "kw::static") }
            TokenType::ANYKW => { write!(f, "kw::any") }
            TokenType::NULLKW => { write!(f, "kw::null") }
            TokenType::REQUIRES => { write!(f, "kw::requires") }
            TokenType::WAITS => { write!(f, "kw::waits") }
            TokenType::ALLOWS => { write!(f, "kw::allows") }
            TokenType::PLUG => { write!(f, "kw::plug") }
            TokenType::VISIBLE => { write!(f, "kw::visible") }
            TokenType::STRUCT => { write!(f, "kw::struct") }
            TokenType::EXT => { write!(f, "kw::ext") }
            TokenType::FUNC => { write!(f, "kw::func") }
            TokenType::FOR => { write!(f, "kw::for") }
            TokenType::OPEN => { write!(f, "kw::open") }
            TokenType::OVERRIDE => { write!(f, "kw::override") }
            TokenType::MK => { write!(f, "kw::mk") }
            TokenType::EXPORT => { write!(f, "kw::export") }
            TokenType::HIDDEN => { write!(f, "kw::hidden") }
            TokenType::IF => { write!(f, "kw::if") }
            TokenType::ELSE => { write!(f, "kw::else") }
            TokenType::SWITCH => { write!(f, "kw::switch") }
            TokenType::DISRUPT => { write!(f, "kw::disrupt") }
            TokenType::SKIP => { write!(f, "kw::skip") }
            TokenType::RETURN => { write!(f, "kw::return") }
            TokenType::CHARKW => { write!(f, "kw::char") }
            TokenType::BOOLKW => { write!(f, "kw::bool") }
            TokenType::INTKW => { write!(f, "kw::int") }
            TokenType::UINTKW => { write!(f, "kw::uint") }
            TokenType::BYTEKW => { write!(f, "kw::byte") }
            TokenType::UBYTEKW => { write!(f, "kw::ubyte") }
            TokenType::LONGKW => { write!(f, "kw::long") }
            TokenType::ULONGKW => { write!(f, "kw::ulong") }
            TokenType::DECIMALKW => { write!(f, "kw::decimal") }
            EOF => { write!(f, "eof") }
            _ => { write!(f, "unexpected") }
        }
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
    pub fn eof() -> Token {
        Self {
            t_type: EOF,
            t_value: String::new(),
            t_pos: 0xFFDD
        }
    }
    pub fn to_string(&self) -> String {
        format!("Token with type [ {:?} ] value [ {} ] pos [ {} ]", self.t_type, self.t_value, self.t_pos)
    }
}
impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Token with type [ {:?} ] value [ {} ] pos [ {} ]", self.t_type, self.t_value, self.t_pos)
    }
}