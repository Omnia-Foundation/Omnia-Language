pub mod ast;

use std::collections::VecDeque;
use std::process::exit;
use crate::core::omnia_types::Type;
use crate::core::omnia_types::Type::NULL;
use crate::core::utils::stringutils::StringBuilder;
use crate::lexer::token::{Token, TokenType};
use crate::lexer::token::TokenType::{ANDASSIGN, ARROW, ASSIGN, BOOL, BOOLKW, BYTE, BYTEKW, CHAR, CHARKW, COLON, COMMA, DECIMAL, DECIMALKW, DIVASSIGN, EOF, FUNC, IDENT, IF, INT, INTKW, LBRACE, LONG, LONGKW, LPAREN, MINUS, MINUSASSIGN, MULASSIGN, NULLKW, ORASSIGN, PLUG, PLUS, PLUSASSIGN, RBRACE, REMASSIGN, RETURN, RPAREN, SEMICOLON, SLASH, STAR, UBYTE, UBYTEKW, UINT, UINTKW, ULONG, ULONGKW};
use crate::parser::ast::ast_vm::{ASTNode, ArgumentExpressionNode, AssignmentOperator, AssignmentStatementNode, BinaryExpressionNode, BinaryOperation, BlockStatementNode, ByteNode, CharNode, DecimalNode, Expression, FunctionCallNode, FunctionDeclarationStatementNode, IntNode, LiteralExpression, LongNode, PlugStatementNode, ReturnStatementNode, Statement, UByteNode, UIntNode, ULongNode, VariableAccessExpressionNode, VariableCreationStatementNode};

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
    len: usize,
    output: Option<BlockStatementNode>,
    buffer: Vec<Token>
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
        println!("Processing token {}", self.get_cur());
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
            let statement = self.variable_creation_statement();
            self.require(&SEMICOLON);
            return statement
        }
        if self.r#match(&RETURN) {
            let statement = self.return_statement();
            self.require(&SEMICOLON);
            return statement
        }
        if self.r#match(&PLUG) {
            return self.plug_statement()
        }
        if self.r#match(&LBRACE) {
            return self.block()
        }
        // if self.r#match(&IF) {
        //     return self.if_statement()           //TODO!
        // }
        if self.r#match(&FUNC) {
            return self.function_declaration_statement()
        }
        if self.r#match(&IDENT) {
            self.call_or_return_or_assignment()
        }
        else {
            panic!("Unexpected token or use of an expression as a command [{}] at pos {}", self.buffer.pop().unwrap().t_type, self.pos)
        }
    }
    fn block(&mut self) -> Statement {
        let mut statements: BlockStatementNode = BlockStatementNode::new();
        loop {
            if self.r#match(&EOF) {
                panic!("OmniaParser error:: unexpected end-of-file while processing BlockStatement")
            }
            if self.r#match(&RBRACE) {
                break
            }
            statements.add_node(ASTNode::Statement(self.statement()))
        }
        Statement::Block(Box::from(statements))
    }
    fn call_or_return_or_assignment(&mut self) -> Statement {
        let buffered = self.buffer.pop().unwrap();
        self.buffer.push(self.get_cur());
        if self.r#match(&LPAREN) {
            self.buffer.pop();
            let name = buffered.t_value.clone();
            if self.r#match(&RPAREN) {
                return Statement::FunctionCall(Box::from(FunctionCallNode::new(name, None)))
            }
            let mut args: Vec<ASTNode> = Vec::new();
            loop {
                if self.r#match(&RPAREN) {
                    break
                }
                let arg = self.expression();
                args.push(ASTNode::Expression(arg));
                if self.r#match(&RPAREN) {
                    break
                }
                self.require(&COMMA);

            }
            self.require(&SEMICOLON);
            return Statement::FunctionCall(Box::from(FunctionCallNode::new(name, Some(args))))
        }
        if self.match_any(vec![ASSIGN, PLUSASSIGN, MINUSASSIGN, MULASSIGN, DIVASSIGN, REMASSIGN, ANDASSIGN, ORASSIGN]) {
            let name = buffered.t_value.clone();
            let op = &self.buffer.pop().unwrap().t_type;
            let value = self.expression();
            self.require(&SEMICOLON);
            return Statement::Assignment(Box::from(AssignmentStatementNode::new(name, AssignmentOperator::from_token_type(op).unwrap(), value)))
        }
        // self.require(&SEMICOLON);
        self.pos -= 1;
        let expr = self.expression();
        Statement::Return(Box::from(ReturnStatementNode::new(ASTNode::Expression(expr))))
    }
    fn function_declaration_statement(&mut self) -> Statement {
        let mut name = self.require(&IDENT);
        self.require(&LPAREN);
        let args = self.arguments();
        if self.r#match(&ARROW) {  // returns type
            let ret_type = self.require_any(vec![BYTEKW, UBYTEKW, INTKW, UINTKW, LONGKW, ULONGKW, DECIMALKW, CHARKW, BOOLKW, NULLKW]);
            let body = self.statement();
            Statement::FunctionDeclaration(Box::from(FunctionDeclarationStatementNode::new(name.t_value, args, Type::from_token_type(&ret_type.t_type).unwrap(), body)))
        } else {
            let body = self.statement();
            Statement::FunctionDeclaration(Box::from(FunctionDeclarationStatementNode::new(name.t_value, args, NULL, body)))
        }

    }
    fn arguments(&mut self) -> Vec<Expression> {
        let mut args: Vec<Expression> = Vec::new();
        loop {
            if self.r#match(&RPAREN) {
                break
            }
            let name = self.require(&IDENT).t_value.clone();
            self.require(&COLON);
            let r#type = self.require_any(vec![BYTEKW, UBYTEKW, INTKW, UINTKW, LONGKW, ULONGKW, DECIMALKW, CHARKW, BOOLKW]);
            args.push(Expression::Argument(Box::from(ArgumentExpressionNode::new(name, Type::from_token_type(&r#type.t_type).unwrap()))));
            if self.r#match(&RPAREN) {
                break
            }
            self.require(&COMMA);
        }
        args
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
        self.buffer.pop();
        let value = self.expression();
        self.pos += 1;
        Statement::Return(Box::from(ReturnStatementNode::new(ASTNode::Expression(value))))
    }
    fn plug_statement(&mut self) -> Statement {
        self.buffer.pop();
        println!("Processing plug");
        let mut sb = StringBuilder::new();
        let mut cur = self.get_cur();
        sb.push_string(cur.t_value.clone());
        while !self.r#match(&SEMICOLON) {
            let cur = self.get_cur();
            self.pos += 1;
            sb.push_string(cur.t_value.clone())
        };
        Statement::Plug(Box::from(PlugStatementNode::new(sb.pack())))

    }
    fn expression(&mut self) -> Expression {
        self.additive()
    }
    fn additive(&mut self) -> Expression {
        let mut left = self.multiplicative();
        loop {
            self.buffer.push(self.get_cur());
            if self.r#match(&PLUS) || self.r#match(&MINUS) {
                let oper = BinaryOperation::from_token_type(&self.buffer.pop().unwrap().t_type).unwrap();
                let right = self.multiplicative();
                left = Expression::Binary(Box::from(BinaryExpressionNode::new(left, oper, right)))
            }
            self.buffer.pop();
            break
        }
        left
    }
    fn multiplicative(&mut self) -> Expression {
        let mut left = self.primary();
        loop {
            self.buffer.push(self.get_cur());
            if self.r#match(&STAR) || self.r#match(&SLASH) {
                let oper = BinaryOperation::from_token_type(&self.buffer.pop().unwrap().t_type).unwrap();
                let right = self.primary();
                left = Expression::Binary(Box::from(BinaryExpressionNode::new(left, oper, right)))
            }
            self.buffer.pop();
            break
        }
        left
    }
    fn primary(&mut self) -> Expression {
        self.buffer.push(self.get_cur());
        if self.r#match(&DECIMAL) {
            let buffered = &self.buffer.pop().unwrap().t_value;
            return Expression::Literal(Box::from(LiteralExpression::Decimal(DecimalNode::new(buffered.parse::<f64>().unwrap()))))
        }
        if self.r#match(&BYTE) {
            let buffered = &self.buffer.pop().unwrap().t_value;
            return Expression::Literal(Box::from(LiteralExpression::Byte(ByteNode::new(buffered.parse::<i8>().unwrap()))))
        }
        if self.r#match(&UBYTE) {
            let buffered = &self.buffer.pop().unwrap().t_value;
            return Expression::Literal(Box::from(LiteralExpression::UByte(UByteNode::new(buffered.parse::<u8>().unwrap()))))
        }
        if self.r#match(&INT) {
            let buffered = &self.buffer.pop().unwrap().t_value;
            return Expression::Literal(Box::from(LiteralExpression::Int(IntNode::new(buffered.parse::<i32>().unwrap()))))
        }
        if self.r#match(&UINT) {
            let buffered = &self.buffer.pop().unwrap().t_value;
            return Expression::Literal(Box::from(LiteralExpression::UInt(UIntNode::new(buffered.parse::<u32>().unwrap()))))
        }
        if self.r#match(&LONG) {
            let buffered = &self.buffer.pop().unwrap().t_value;
            return Expression::Literal(Box::from(LiteralExpression::Long(LongNode::new(buffered.parse::<i64>().unwrap()))))
        }
        if self.r#match(&ULONG) {
            let buffered = &self.buffer.pop().unwrap().t_value;
            return Expression::Literal(Box::from(LiteralExpression::ULong(ULongNode::new(buffered.parse::<u64>().unwrap()))))
        }
        if self.r#match(&CHAR) {
            let buffered = &self.buffer.pop().unwrap().t_value;
            let mut chars = buffered.chars();
            if chars.clone().count() > 1 {
                panic!("char literal [{}] contains more than 1 char!", buffered)
            }
            return Expression::Literal(Box::from(LiteralExpression::Char(CharNode::new(chars.nth(0).unwrap()))))
        }
        // if self.r#match(&BOOL) {                                                                                                         //not yet implemented
        //     let buffered = &self.buffer.pop().unwrap().t_value;
        //     return Expression::Literal(Box::from(LiteralExpression::(LongNode::new(buffered.parse::<i64>().unwrap()))))
        // }
        if self.r#match(&IDENT) {
            let buffered = self.buffer.pop().unwrap();
            if self.r#match(&LPAREN) {
                let name = buffered.t_value.clone();
                if self.r#match(&RPAREN) {
                    return Expression::FunctionCall(Box::from(FunctionCallNode::new(name, None)))
                }
                let mut args: Vec<ASTNode> = Vec::new();
                loop {
                    if self.r#match(&RPAREN) {
                        break
                    }
                    let arg = self.expression();
                    args.push(ASTNode::Expression(arg));
                    if self.r#match(&RPAREN) {
                        break
                    }
                    self.require(&COMMA);

                }
                return Expression::FunctionCall(Box::from(FunctionCallNode::new(name, Some(args))))
            }
            return Expression::VariableAccess(Box::from(VariableAccessExpressionNode::new(buffered.t_value.clone())));
        }
        if self.r#match(&LPAREN) {
            let value = self.expression();
            self.require(&RPAREN);
            value
        }
        else {
            panic!("Unexpected token {} at pos {}", self.buffer.pop().unwrap(), self.pos)
        }
    }

    // fn primary(&mut self) -> Expression {
    //     if self.r#match(&LPAREN) {
    //         let value = self.expression();
    //         self.require(&RPAREN);
    //         return value;
    //     }
    //     self.buffer.push(self.get_cur());
    //     if self.r#match(&IDENT) {
    //         let buffered = self.buffer.pop().unwrap();
    //         return Expression::VariableAccess(Box::from(VariableAccessExpressionNode::new(buffered.t_value.clone())));
    //     }
    //     if self.r#match(&DECIMAL)
    //         || self.r#match(&BYTE)
    //         || self.r#match(&UBYTE)
    //         || self.r#match(&INT)
    //         || self.r#match(&UINT)
    //         || self.r#match(&LONG)
    //         || self.r#match(&ULONG)
    //         || self.r#match(&CHAR)
    //         || self.r#match(&BOOL)
    //     {
    //         self.literal()
    //     }
    // }
    // fn literal(&mut self) -> Expression {
    //     let buffered = self.buffer.pop().unwrap();
    //     match &buffered.t_type {
    //         &BYTE => {
    //             Expression::Literal(Box::from(LiteralExpression::Byte(ByteNode::new(buffered.t_value.parse::<i8>().unwrap()))))
    //         }
    //         &UBYTE => {
    //             Expression::Literal(Box::from(LiteralExpression::UByte(UByteNode::new(buffered.t_value.parse::<u8>().unwrap()))))
    //         }
    //
    //         _ => {
    //             panic!("Unexpected type")
    //         }
    //     }
    // }
    fn peek(&self, offset: usize) -> Option<&Token> {
        self.tokens.get(self.pos+offset)
    }
    fn get_cur(&self) -> Token {
        if let Some(t) = self.tokens.get(self.pos) {
            t.clone()
        } else {
            Token::eof()
        }
    }
    fn r#match(&mut self, expected: &TokenType) -> bool {

        if let Some(cur) = self.tokens.get(self.pos) {
            if &(cur.t_type) == expected {
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
    fn match_any(&mut self, expected: Vec<TokenType>) -> bool {
        let mut result = false;
        if let Some(cur) = self.tokens.get(self.pos) {
            expected.iter().for_each(|x| {
                if x == &cur.t_type {
                    self.pos += 1;
                    result = true;
                    return;
                }
            });
            if !result {
                self.log_error(format!("Expected token with type one of {:?}, got {}", expected, &cur.t_type));
                return false
            }
            true
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
                panic!("OmniaParser error:: required token with type {}, got {} at pos {}", required, &cur.t_type, self.pos)
            }
        } else {
            panic!("OmniaParser error:: size is out of bounds")
        }
    }
    fn require_any(&mut self, required: Vec<TokenType>) -> Token {
        let mut result = false;
        let mut token: Option<Token> = None;
        if let Some(cur) = self.tokens.get(self.pos) {
            required.iter().for_each(|x| {
                if x == &cur.t_type {
                    self.pos += 1;
                    token = Some(cur.clone());
                    result = true;
                    return;
                }
            });
            if !result || token.is_none() {
                panic!("OmniaParser error:: required token with type one of {:?}, got {} at pos {}", required, &cur.t_type, self.pos)
            }
            token.unwrap()
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