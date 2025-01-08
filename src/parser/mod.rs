pub mod ast;
use crate::lexer::token::Token;
use crate::parser::ast::ast_vm::{ASTNode, BlockStatementNode};

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
    len: usize,
    cur: &'static Token,
    output: Option<BlockStatementNode>
}

impl Parser {
    pub fn new(mut tokens: Vec<Token>) -> Parser {
        let mut pos = 0usize;
        let mut len = (&tokens).len();
        let mut cur = (&tokens).get(pos).unwrap();
        Self {
            tokens,
            pos,
            len,
            cur,
            output: None
        }
    }
    fn log_error(&self, msg: &str) {
        eprintln!("OmniaParser::{{ {} }}", msg)
    }
}