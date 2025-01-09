pub mod ast;

use std::collections::VecDeque;
use crate::core::omnia_types::Type;
use crate::core::utils::stringutils::StringBuilder;
use crate::lexer::token::{Token, TokenType};
use crate::lexer::token::TokenType::{ASSIGN, BOOL, BOOLKW, BYTE, BYTEKW, CHAR, CHARKW, DECIMAL, DECIMALKW, FUNC, IDENT, IF, INT, INTKW, LBRACE, LONG, LONGKW, LPAREN, PLUG, RETURN, RPAREN, SEMICOLON, UBYTE, UBYTEKW, UINT, UINTKW, ULONG, ULONGKW};
use crate::parser::ast::ast_vm::{ASTNode, BlockStatementNode, ByteNode, Expression, LiteralExpression, PlugStatementNode, ReturnStatementNode, Statement, UByteNode, VariableAccessExpressionNode, VariableCreationStatementNode};

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
    len: usize,
    output: Option<BlockStatementNode>,
    buffer: Vec<&'static Token>
}

impl Parser {
    pub fn new(mut tokens: Vec<Token>) -> Parser {
        let mut pos = 0usize;
        let mut len = (&tokens).len();
        Self {
            tokens,
            pos,
            len,
            output: None,
            buffer: Vec::new()
        }
    }
    pub fn parse(&mut self) -> BlockStatementNode {
        let mut root = BlockStatementNode::new();

        while self.has_next_token() {
            root.add_node(ASTNode::Statement(self.statement()))
        }
        root
    }
    fn statement(&mut self) -> Statement {
        self.buffer.push(self.get_cur());
        if self.r#match(&DECIMALKW)
            || self.r#match(&BYTEKW)
            || self.r#match(&UBYTEKW)
            || self.r#match(&INTKW)
            || self.r#match(&UINTKW)
            || self.r#match(&LONGKW)
            || self.r#match(&ULONGKW)
            || self.r#match(&CHARKW)
            || self.r#match(&BOOLKW) {
            return self.variable_creation_statement()
        }
        if self.r#match(&RETURN) {
            return self.return_statement()
        }
        if self.r#match(&PLUG) {
            return self.plug_statement()
        }
        if self.r#match(&LBRACE) {
            return self.statement()
        }
        if self.r#match(&IF) {
            return self.if_statement()
        }
        if self.r#match(&FUNC) {
            return self.function_declaration_statement()
        }
        if self.r#match(&IDENT) {
            self.call_or_return_or_assignment()
        }
    }
    fn variable_creation_statement(&mut self) -> Statement {
        let buffered = self.buffer.pop();
        let r#type = &buffered.unwrap().t_type;
        let name = self.require(&IDENT);
        self.require(&ASSIGN);
        let value = self.expression();
        Statement::VariableCreation(Box::from(VariableCreationStatementNode::new(name.t_value, Type::from_token_type(r#type).expect("Unexpected token type"), value)))
    }
    fn return_statement(&mut self) -> Statement {
        let value = self.expression();
        self.pos += 1;
        Statement::Return(Box::from(ReturnStatementNode::new(ASTNode::Expression(value))))
    }
    fn plug_statement(&mut self) -> Statement {
        let mut sb = StringBuilder::new();
        let mut cur = self.get_cur();
        sb.push_string(cur.t_value.clone());
        while !self.r#match(&SEMICOLON) {
            let cur = self.get_cur();
            sb.push_string(cur.t_value.clone())
        };
        Statement::Plug(Box::from(PlugStatementNode::new(sb.pack())))

    }
    fn expression(&mut self) -> Expression {
        self.mathematics()
    }
    fn mathematics(&mut self) -> Expression {
        let res = self.primary();
    }
    fn primary(&mut self) -> Expression {
        if self.r#match(&LPAREN) {
            let value = self.expression();
            self.require(&RPAREN);
            return value;
        }
        self.buffer.push(self.get_cur());
        if self.r#match(&IDENT) {
            let buffered = self.buffer.pop().unwrap();
            return Expression::VariableAccess(Box::from(VariableAccessExpressionNode::new(buffered.t_value.clone())));
        }
        if self.r#match(&DECIMAL)
            || self.r#match(&BYTE)
            || self.r#match(&UBYTE)
            || self.r#match(&INT)
            || self.r#match(&UINT)
            || self.r#match(&LONG)
            || self.r#match(&ULONG)
            || self.r#match(&CHAR)
            || self.r#match(&BOOL)
        {
            self.literal()
        }
    }
    fn literal(&mut self) -> Expression {
        let buffered = self.buffer.pop().unwrap();
        match &buffered.t_type {
            &BYTE => {
                Expression::Literal(Box::from(LiteralExpression::Byte(ByteNode::new(buffered.t_value.parse::<i8>().unwrap()))))
            }
            &UBYTE => {
                Expression::Literal(Box::from(LiteralExpression::UByte(UByteNode::new(buffered.t_value.parse::<u8>().unwrap()))))
            }

            _ => {
                panic!("Unexpected type")
            }
        }
    }
    fn peek(&self, offset: usize) -> Option<&Token> {
        self.tokens.get(self.pos+offset)
    }
    fn get_cur(&self) -> &Token {
        if let Some(t) = self.tokens.get(self.pos) {
            t
        } else {
            &Token::eof()
        }
    }
    fn r#match(&mut self, expected: &TokenType) -> bool {

        if let Some(cur) = self.tokens.get(self.pos) {
            if &cur.t_type == expected {
                self.pos += 1;
                true
            } else {
                self.log_error(format!("Expected token with type {}, got {}", expected, &cur.t_type));
                false
            }
        } else {
            self.log_error("size is out of bounds".to_string());
            false
        }
    }
    fn require(&mut self, required: &TokenType) -> Token {
        if let Some(cur) = self.tokens.get(self.pos) {
            if &cur.t_type == required {
                self.pos += 1;
                cur.clone()
            } else {
                panic!("OmniaParser error:: required token with type {}, got {}", required, &cur.t_type)
            }
        } else {
            panic!("OmniaParser error:: size is out of bounds")
        }
    }
    pub fn has_next_token(&self) -> bool {
        self.pos < self.len
    }
    fn log_error(&self, msg: String) {
        eprintln!("OmniaParser::{{ {} }}", msg)
    }
}
#[derive(Debug)]
pub enum CompilerError {
    TypeError(String)
}