

//! # Omnia Lexer
//! ***
//!
//! This file contains [Lexical Analyzer][Lexer] of the Omnia Language. The lexer accepts a string with a code as input, and outputs a [Vec] of [tokens][Token] at the output
//!
//!
//!
//!
use std::collections::HashMap;
use rustring_builder::StringBuilder;
use crate::lexer::token::{Token, TokenType};
use crate::lexer::token::TokenType::{ACCESS, ALLOWS, AMPERSAND, AND, ANDASSIGN, ANYKW, ARROW, ASSIGN, BYTEKW, CHARARR, CHARARRKW, CHARKW, COLON, COMMA, DEC, DECIMAL, DECIMALKW, DISRUPT, DIVASSIGN, DOLLAR, ELSE, EOF, EQ, EXPORT, EXT, FEQ, FOR, FUNC, GEQ, GT, HIDDEN, IDENT, IF, INC, INT, INTKW, LAMBDA, LBRACE, LBRACK, LEQ, LONGKW, LPAREN, LS, MINUS, MINUSASSIGN, MK, MULASSIGN, NEQ, NOT, NULLKW, OMNIKW, OPEN, OR, ORASSIGN, OVERRIDE, PERIOD, PIPE, PLUG, PLUS, PLUSASSIGN, POWER, QUESTION, RBRACE, RBRACK, REM, REMASSIGN, REQUIRES, RETURN, RPAREN, SEMICOLON, SHL, SHR, SKIP, SLASH, STAR, STATIC, STRUCT, SWITCH, UBYTEKW, UINTKW, ULONGKW, UNDERSCORE, VISIBLE, WAITS, XOR};
pub mod token;
#[derive(Clone)]
pub struct Lexer {
    l_pos: usize,
    l_input: String,
    l_size: usize,
    l_cur: char,
    l_output: Vec<Token>,
    l_buffer: StringBuilder,
    l_keywords: HashMap<String, TokenType>
}

impl Lexer {
    pub fn new(input: String) -> Lexer {
        let l_input = input;
        let size = l_input.clone().len();
        let pos = 0usize;
        let cur = l_input.clone().chars().nth(pos).unwrap();
        Self {
            l_pos: pos,
            l_input,
            l_size: size,
            l_cur: cur,
            l_output: Vec::new(),
            l_buffer: StringBuilder::new(),
            l_keywords: HashMap::from([
                (String::from("any"), ANYKW),
                (String::from("null"), NULLKW),
                (String::from("requires"), REQUIRES),
                (String::from("waits"), WAITS),
                (String::from("allows"), ALLOWS),
                (String::from("plug"), PLUG),
                (String::from("visible"), VISIBLE),
                (String::from("struct"), STRUCT),
                (String::from("ext"), EXT),
                (String::from("func"), FUNC),
                (String::from("for"), FOR),
                (String::from("open"), OPEN),
                (String::from("override"), OVERRIDE),
                (String::from("mk"), MK),
                (String::from("export"), EXPORT),
                (String::from("hidden"), HIDDEN),
                (String::from("if"), IF),
                (String::from("else"), ELSE),
                (String::from("switch"), SWITCH),
                (String::from("disrupt"), DISRUPT),
                (String::from("skip"), SKIP),
                (String::from("return"), RETURN),
                (String::from("int"), INTKW),
                (String::from("uint"), UINTKW),
                (String::from("byte"), BYTEKW),
                (String::from("ubyte"), UBYTEKW),
                (String::from("long"), LONGKW),
                (String::from("ulong"), ULONGKW),
                (String::from("decimal"), DECIMALKW),
                (String::from("omni"), OMNIKW),
                (String::from("char"), CHARKW),
                (String::from("char[]"), CHARARRKW),
                (String::from("static"), STATIC)
            ])
        }
    }
    pub fn tokenize(&mut self) -> Vec<Token> {
        while self.has_next() {
            match self.l_cur {
                '&' => { if self.peek(1) == '&' { self.push_token(AND); self.next() }
                else if self.peek(1) == '=' { self.push_token(ANDASSIGN); self.next() }
                else { self.push_token(AMPERSAND); } self.next() }
                '*' => { if self.peek(1) == '=' { self.push_token(MULASSIGN); self.next() }
                else { self.push_token(STAR) } self.next() }
                '/' => { if self.peek(1) == '/' { while self.l_cur != '\n' { self.next() } }
                else if self.peek(1) == '=' { self.push_token(DIVASSIGN); self.next() }
                else if self.peek(1) == '*' {
                    loop {
                        if self.l_cur == '*' && self.peek(1) == '/' {
                            self.next();
                            self.next();
                            break
                        }
                        self.next()
                    } }
                else { self.push_token(SLASH); } self.next(); }
                '+' => { if self.peek(1) == '+' { self.push_token(INC); self.next(); }
                else if self.peek(1) == '=' { self.push_token(PLUSASSIGN); self.next() }
                else { self.push_token(PLUS) }; self.next() }
                '-' => { if self.peek(1) == '-' { self.push_token(DEC); self.next(); }
                else if self.peek(1) == '=' { self.push_token(MINUSASSIGN); self.next(); }
                else if self.peek(1) == '>' { self.push_token(ARROW); self.next() }
                else { self.push_token(MINUS) }; self.next() }
                '|' => { if self.peek(1) == '|' { self.push_token(OR); self.next() }
                else if self.peek(1) == '=' { self.push_token(ORASSIGN); self.next() }
                else if self.peek(1) == '>' { self.push_token(SHR); self.next() }
                else { self.push_token(PIPE) } ; self.next() }
                '<' => { if self.peek(1) == '=' { self.push_token(LEQ); self.next() }
                else if self.peek(1) == '|' { self.push_token(SHL); self.next() }
                else { self.push_token(LS) }; self.next() }
                '>' => { if self.peek(1) == '=' { self.push_token(GEQ); self.next() }
                else if self.peek(1) == '>' { self.push_token(LAMBDA); self.next() }
                else { self.push_token(GT) }; self.next() }
                '=' => { if self.peek(1) == '=' { self.push_token(EQ); self.next() }
                else if self.peek(1) == '!' { self.push_token(FEQ); self.next() }
                else { self.push_token(ASSIGN) } self.next() }
                '\'' => { self.next(); self.l_output.push(Token::new(TokenType::CHAR, String::from(self.l_cur), self.l_pos)); self.next(); }
                '"' => { self.tokenize_chararr() }
                '!' => { if self.peek(1) == '=' { self.push_token(NEQ); self.next() } else { self.push_token(NOT) }; self.next() }
                '%' => { if self.peek(1) == '=' { self.push_token(REMASSIGN); self.next() } else { self.push_token(REM); } self.next()  }
                ':' => { if self.peek(1) == ':' { self.push_token(ACCESS); self.next() } else { self.push_token(COLON) }; self.next() }
                '$' => { self.push_token(DOLLAR); self.next() }
                '^' => { self.push_token(POWER); self.next() }
                '(' => { self.push_token(LPAREN); self.next() }
                '[' => { self.push_token(LBRACK); self.next() }
                '{' => { self.push_token(LBRACE); self.next() }
                ',' => { self.push_token(COMMA); self.next() }
                '.' => { self.push_token(PERIOD); self.next() }
                '_' => { self.push_token(UNDERSCORE); self.next() }
                ')' => { self.push_token(RPAREN); self.next() }
                ']' => { self.push_token(RBRACK); self.next() }
                '}' => { self.push_token(RBRACE); self.next() }
                ';' => { self.push_token(SEMICOLON); self.next() }
                '?' => { self.push_token(QUESTION); self.next() }
                '~' => { self.push_token(XOR); self.next() }
                '\0' => { self.push_token(EOF); break }
                _ => {
                    if self.l_cur.is_whitespace() { self.next() }
                    else if self.l_cur.is_alphabetic() { self.tokenize_ident() }
                    else if self.l_cur.is_digit(10) { self.tokenize_number() }
                    else { panic!("Unexpected char {} at pos {}", self.l_cur, self.l_pos) }
                }


            }
        }

        let mut output: Vec<Token> = Vec::new();
        for val in self.l_output.iter().cloned() {
            output.push(val)
        }
        output
    }
    fn tokenize_chararr(&mut self) {
        self.l_buffer.clear();
        self.next();
        while self.l_cur != '"' {
            self.l_buffer.push(self.l_cur);
            self.next()
        }
        self.next();
        let value = self.l_buffer.to_string();
        self.l_output.push(Token::new(CHARARR, value, self.l_pos))

    }
    fn tokenize_ident(&mut self) {
        self.l_buffer.clear();
        while self.l_cur.is_alphabetic()
            || self.l_cur == '_'
            || self.l_cur.is_digit(10)
            || self.l_cur == '['
            || self.l_cur == ']' {
            self.l_buffer.push(self.l_cur);
            self.next()
        }
        let value = self.l_buffer.to_string();
        self.l_output.push(Token::new(self.l_keywords.get(&value).unwrap_or(&IDENT).clone(), value, self.l_pos))
    }
    fn tokenize_number(&mut self) {
        let mut is_float: bool = false;
        self.l_buffer.clear();
        while self.l_cur.is_digit(10)
            || self.l_cur == '.'
            || self.l_cur == 'f'
        {
            if self.l_cur == 'f' {
                is_float = true;
                self.next();
                break;
            }
            self.l_buffer.push(self.l_cur);
            self.next();
        }
        let value = self.l_buffer.to_string();
        if value.clone().contains(".") && !is_float {
            self.l_output.push(Token::new(DECIMAL, value, self.l_pos))
        }else if value.clone().contains(".") && is_float {
            self.l_output.push(Token::new(DECIMAL, value, self.l_pos))
        }else {
            self.l_output.push(Token::new(INT, value, self.l_pos))
        }
    }

    fn push_token(&mut self, t: TokenType) {
        let val = match &t {
            STAR => String::from("*"),
            SLASH => String::from("/"),
            PLUS => String::from("+"),
            MINUS => String::from("-"),
            AMPERSAND => String::from("&"),
            AND => String::from("&&"),
            OR => String::from("||"),
            LS => String::from("<"),
            GT => String::from(">"),
            EQ => String::from("=="),
            ASSIGN => String::from("="),
            NOT => String::from("!"),
            INC => String::from("++"),
            DEC => String::from("--"),
            NEQ => String::from("!="),
            LEQ => String::from("<="),
            GEQ => String::from(">="),
            ANDASSIGN => String::from("&="),
            ORASSIGN => String::from("|="),
            PLUSASSIGN => String::from("+="),
            MINUSASSIGN => String::from("-="),
            MULASSIGN => String::from("*="),
            DIVASSIGN => String::from("/="),
            REM => String::from("%"),
            ACCESS => String::from("::"),
            ARROW => String::from("->"),
            DOLLAR => String::from("$"),
            FEQ => String::from("=!"),
            LAMBDA => String::from(">>"),
            POWER => String::from("^"),
            LPAREN => String::from("("),
            LBRACK => String::from("["),
            LBRACE => String::from("{"),
            COMMA => String::from(","),
            PERIOD => String::from("."),
            RPAREN => String::from(")"),
            RBRACK => String::from("]"),
            RBRACE => String::from("}"),
            SEMICOLON => String::from(";"),
            COLON => String::from(":"),
            QUESTION => String::from("?"),
            UNDERSCORE => String::from("_"),
            XOR => String::from("~"),
            PIPE => String::from("|"),
            SHL => String::from("<|"),
            SHR => String::from("|>"),
            _ => panic!("Unexpected/unsupported token type {:?}", t)
        };
        self.l_output.push(Token::new(t, val, self.l_pos))
    }
    fn next(&mut self) {
        self.l_pos += 1;
        self.l_cur = self.l_input.chars().nth(self.l_pos).unwrap_or('\0')
    }
    fn peek(&mut self, offset: usize) -> char {
        let final_pos = self.l_pos + offset;
        self.l_input.clone().chars().nth(final_pos).unwrap_or('\0')
    }

    fn has_next(&mut self) -> bool {
        self.l_input.clone().chars().nth(self.l_pos+1).is_some()
    }
}
