use std::collections::HashMap;
use num_traits::FromPrimitive;
use crate::core::errors::Failure;
use crate::core::utils::string_utils::{Cursor, StringBuilder};
use crate::lexer::token::{Token, TokenType};

pub(crate) mod token;



pub struct Lexer {
    input: Cursor,
    buffer: StringBuilder,
    output: Vec<Token>,
    keywords: HashMap<String, TokenType>,
    line: usize,
    col: usize,
    __line_offset: isize,
    __src_table: Vec<String>
}
impl Lexer {
    pub fn new(code: String) -> Self {
        let src_table: Vec<String> = code.clone().split("\n").map(|x| x.to_string()).collect();
        let input = Cursor::new(code);
        let buffer = StringBuilder::new();
        let output = Vec::<Token>::new();
        let (line, col) = (0usize, 0usize);
        let keywords = HashMap::from([
            ("__builtin_print".to_string(), TokenType::BPRINT),
            ("mk".to_string(), TokenType::MK),
            ("if".to_string(), TokenType::IF),
            ("else".to_string(), TokenType::ELSE)]);
        Self {
            input,
            buffer,
            output,
            keywords,
            line,
            col,
            __line_offset: -1,
            __src_table: src_table
        }
    }

    pub fn tokenize(&mut self) -> Result<(Vec<Token>, Vec<String>), Failure> {
        while let Some(ch) = self.input.current() {
            // println!("processing char {} : {}", ch, self.__line_offset);
            if !self.additional(&ch) {
                if ch.is_digit(10) { self.tokenize_digit() }
                else if ch == '_' {
                    // println!("processing char UNDER {}", self.input.current().unwrap());
                    if let Some(c) = self.input.seek(1) {
                        if c == '_' { self.tokenize_ident(); }
                        else { self.output.push(Token::new(TokenType::UNDERSCORE, "_".to_string(), (self.line, self.col))); self.input.next(); self.__line_offset += 1; }
                    }
                }
                else if ch.is_alphabetic() { self.tokenize_ident() }
                else { /* println!("Error {}", self.input.current().unwrap()); */ return Err( Failure::LexerUnexpectedChar { src: self.input.get_current_line(), span: (self.input.get_pos() - 2, self.line).into(), ch } ) }
            } else {
                self.input.next();
                self.__line_offset += 1;
                self.col += 1;
            }
        }
        self.output.push(Token::eof_token());
        Ok((self.output.clone(), self.__src_table.clone()))
    }
    fn tokenize_ident(&mut self) {
        // println!("processing char IDENT {}", self.input.current().unwrap());
        self.buffer.clear();
        while let Some(ch) = self.input.current() {
            if !ch.is_alphabetic() && ch != '_' {
                break
            }
            self.buffer.push_c(ch);
            self.input.next();
            self.__line_offset += 1;
        }
        let value = self.buffer.to_string();
        self.output.push(Token::new(self.keywords.get(&value).cloned().unwrap_or(TokenType::Ident), value, (self.line, self.col)));
        self.col += 1;
    }
    fn tokenize_digit(&mut self) {
        // println!("processing char DIGIT {}", self.input.current().unwrap());
        self.buffer.clear();
        while let Some(ch) = self.input.current() {
            if !ch.is_digit(10) && ch != '_' {
                break
            }
            self.buffer.push_c(ch);
            self.input.next();
            self.__line_offset += 1;
        }
        let value = self.buffer.to_string();
        self.output.push(Token::new(TokenType::NumberLiteral { size: 32 }, value.replace("_", ""), (self.line, self.col)));
        self.col += 1;
    }
    fn additional(&mut self, ch: &char) -> bool {
        // println!("processing char ADD {}", ch);
        if ch.is_whitespace() { self.col -= 1; return true }
        match ch {
            '(' => { self.output.push(Token::new(TokenType::LPAREN, "(".to_string(), (self.line, self.col))); true }
            ')' => { self.output.push(Token::new(TokenType::RPAREN, ")".to_string(), (self.line, self.col))); true }
            '+' => { self.output.push(Token::new(TokenType::PLUS, "+".to_string(), (self.line, self.col))); true }
            '-' => { self.output.push(Token::new(TokenType::MINUS, "-".to_string(), (self.line, self.col))); true }
            '/' => { self.output.push(Token::new(TokenType::SLASH, "/".to_string(), (self.line, self.col))); true }
            '*' => { self.output.push(Token::new(TokenType::ASTER, "*".to_string(), (self.line, self.col))); true }
            '=' => { if let Some(c) = self.input.seek(1) {
                if c == '=' {
                    self.output.push(Token::new(TokenType::EQ, "==".to_string(), (self.line, self.col)));
                    self.input.next();
                    self.__line_offset += 1;
                    self.col += 1; true
                } else {
                    self.output.push(Token::new(TokenType::ASSIGN, "=".to_string(), (self.line, self.col))); true
                }
            } else {
                self.output.push(Token::new(TokenType::ASSIGN, "=".to_string(), (self.line, self.col))); true
            } }
            '\t' => { self.__line_offset += 1; true }
            '\n' => { self.line += 1; self.col = 0; self.__line_offset = 0; true }
            '>' => { if let Some(c) = self.input.seek(1) {
                if c == '=' {
                    self.output.push(Token::new(TokenType::GE, ">=".to_string(), (self.line, self.col)));
                    self.input.next();
                    self.__line_offset += 1;
                    self.col += 1; true
                } else {
                    self.output.push(Token::new(TokenType::GT, ">".to_string(), (self.line, self.col))); true
                }
            } else {
                self.output.push(Token::new(TokenType::GT, ">".to_string(), (self.line, self.col))); true
            } }
            '<' => { if let Some(c) = self.input.seek(1) {
                if c == '=' {
                    self.output.push(Token::new(TokenType::LE, "<=".to_string(), (self.line, self.col)));
                    self.input.next();
                    self.__line_offset += 1;
                    self.col += 1; true
                } else {
                    self.output.push(Token::new(TokenType::LT, "<".to_string(), (self.line, self.col))); true
                }
            } else {
                self.output.push(Token::new(TokenType::LT, "<".to_string(), (self.line, self.col))); true
            } }
            '!' => { self.output.push(Token::new(TokenType::NEQ, "!=".to_string(), (self.line, self.col)));
                self.input.next();
                self.__line_offset += 1;
                self.col += 1; true }
            '{' => {
                self.output.push(Token::new(TokenType::LBRACE, "{".to_string(), (self.line, self.col))); true
            }
            '}' => {
                self.output.push(Token::new(TokenType::RBRACE, "}".to_string(), (self.line, self.col))); true
            }
            _ => false
        }
    }
}