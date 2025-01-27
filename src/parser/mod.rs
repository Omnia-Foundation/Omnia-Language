pub mod ast;

use std::collections::VecDeque;
use std::process::exit;
use crate::core::omnia_types::Type;
use crate::core::omnia_types::Type::NULL;
use crate::core::utils::numeric_utils::omni::f128;
use crate::core::utils::stringutils::StringBuilder;
use crate::lexer::token::{Token, TokenType};
use crate::lexer::token::TokenType::{AMPERSAND, AND, ANDASSIGN, ARROW, ASSIGN, BOOL, BOOLKW, BYTE, BYTEKW, CHAR, CHARARR, CHARKW, COLON, COMMA, DEC, DECIMAL, DECIMALKW, DIVASSIGN, ELSE, EOF, EQ, EXT, FUNC, GEQ, GT, IDENT, IF, INC, INT, INTKW, LAMBDA, LBRACE, LEQ, LONG, LONGKW, LPAREN, LS, MINUS, MINUSASSIGN, MK, MULASSIGN, NEQ, NULLKW, OMNI, OMNIKW, OR, ORASSIGN, PIPE, PLUG, PLUS, PLUSASSIGN, POWER, RBRACE, REM, REMASSIGN, RETURN, RPAREN, SEMICOLON, SHL, SHR, SLASH, STAR, STRUCT, UBYTE, UBYTEKW, UINT, UINTKW, ULONG, ULONGKW, VISIBLE, XOR};
use crate::parser::ast::nodes;
use crate::parser::ast::nodes::{ASTNode, ArgumentExpressionNode, AssignmentOperator, AssignmentStatementNode, BinaryExpressionNode, BinaryOperation, BitwiseExpressionNode, BitwiseOperation, BlockStatementNode, ByteNode, CharArrNode, CharNode, ComparativeExpressionNode, ConditionalOperation, DecimalNode, Expression, FieldExpressionNode, FunctionCallNode, FunctionDeclarationStatementNode, IfStatementNode, IntNode, LambdaDeclarationStatementNode, LiteralExpression, LogicalExpressionNode, LogicalOperation, LongNode, OmniNode, PlugStatementNode, ReturnStatementNode, Statement, StructDeclarationStatementNode, UByteNode, UIntNode, ULongNode, UnaryExpressionNode, UnaryOperation, VariableAccessExpressionNode, VariableCreationStatementNode};


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
            || self.r#match(&BOOLKW)
            || self.r#match(&MK) {
            let statement = self.variable_creation_statement();
            self.require(&SEMICOLON);
            return statement
        }
        if self.r#match(&VISIBLE) {
            if self.r#match(&FUNC) {
                return self.function_declaration_statement(true)
            }
            if self.r#match(&STRUCT) {
                return self.struct_declaration_statement(true)
            }
            // if self.r#match(&EXT) {
            //     if self.r#match(&FUNC) {
            //
            //     }
            //     else {
            //         panic!("OmniaParser error:: keyword `visible` is not applicable for using with struct extension blocks (\"raw\" `ext`, with `ext func` all is ok!)")
            //     }
            // }
            else {
                panic!("OmniaParser error:: you can use `visible` keyword only before structs, functions or ext functions declarations")
            }
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
        if self.r#match(&IF) {
            return self.if_statement()
        }
        if self.r#match(&FUNC) {
            return self.function_declaration_statement(false)
        }
        if self.r#match(&IDENT) {
            return self.call_or_return_or_assignment()
        }
        if self.r#match(&STRUCT) {
            self.struct_declaration_statement(false)
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

    fn struct_declaration_statement(&mut self, is_visible: bool) -> Statement {
        let name = self.require(&IDENT).t_value.clone();
        let mut fields: Vec<FieldExpressionNode> = Vec::new();
        self.require(&LBRACE);
        loop {
            if self.r#match(&RBRACE) {
                break
            }
            let field_name = self.require(&IDENT).t_value.clone();
            self.require(&COLON);
            let r#type = Type::from_token_type(&self.require_any(vec![BYTEKW, UBYTEKW, INTKW, UINTKW, CHARKW, LONGKW, ULONGKW, DECIMALKW, OMNIKW]).t_type).unwrap();
            fields.push(FieldExpressionNode::new(field_name, r#type));
            if self.r#match(&RBRACE) {
                break
            }
            self.require(&COMMA);
        }
        Statement::StructDeclaration(Box::from(StructDeclarationStatementNode::new(name, fields, is_visible)))
    }
    fn if_statement(&mut self) -> Statement {
        let condition = self.expression();
        let then = self.statement();
        if self.r#match(&ELSE) {
            let r#else = self.statement();
            return Statement::If(Box::from(IfStatementNode::new(condition, then, Some(r#else))))
        }
        Statement::If(Box::from(IfStatementNode::new(condition, then, None)))
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
            if !self.r#match(&RBRACE) {
                self.require(&SEMICOLON);
            } else {
                self.pos -= 1;
            }
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
    fn function_declaration_statement(&mut self, is_visible: bool) -> Statement {
        let mut name = self.require(&IDENT);
        self.require(&LPAREN);
        let args = self.arguments();
        if self.r#match(&ARROW) {  // returns type
            let ret_type = self.require_any(vec![BYTEKW, UBYTEKW, INTKW, UINTKW, LONGKW, ULONGKW, DECIMALKW, OMNIKW, CHARKW, BOOLKW, NULLKW]);
            let body = self.statement();
            Statement::FunctionDeclaration(Box::from(FunctionDeclarationStatementNode::new(name.t_value, args, Type::from_token_type(&ret_type.t_type).unwrap(), body, is_visible)))
        } else {
            let body = self.statement();
            Statement::FunctionDeclaration(Box::from(FunctionDeclarationStatementNode::new(name.t_value, args, NULL, body, is_visible)))
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
            let r#type = self.require_any(vec![BYTEKW, UBYTEKW, INTKW, UINTKW, LONGKW, ULONGKW, DECIMALKW, OMNIKW, CHARKW, BOOLKW]);
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
        if self.r#match(&LPAREN) {
            if r#type != &MK {
                panic!("OmniaParser error:: cannot declare a lambda in a variable with specified type")
            }
            let mut args: Vec<Expression> = Vec::new();
            loop {
                if self.r#match(&RPAREN) {
                    break
                }
                let name = self.require(&IDENT).t_value.clone();
                self.require(&COLON);
                let r#type = self.require_any(vec![BYTEKW, UBYTEKW, INTKW, UINTKW, LONGKW, ULONGKW, DECIMALKW, OMNIKW, CHARKW, BOOLKW]);
                args.push(Expression::Argument(Box::from(ArgumentExpressionNode::new(name, Type::from_token_type(&r#type.t_type).unwrap()))));
                if self.r#match(&RPAREN) {
                    break
                }
                self.require(&COMMA);

            }
            self.require(&ARROW);
            let r_type = self.require_any(vec![BYTEKW, UBYTEKW, INTKW, UINTKW, LONGKW, ULONGKW, DECIMALKW, OMNIKW, CHARKW, BOOLKW]);
            self.require(&LAMBDA);
            let body = self.statement();
            return  Statement::LambdaDeclaration(Box::from(LambdaDeclarationStatementNode::new(name.t_value, args, Type::from_token_type(&r_type.t_type).unwrap(), body)))
        }
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
        self.bitwise()
    }

    fn bitwise(&mut self) -> Expression {
        let mut left = self.logical();
        loop {
            self.buffer.push(self.get_cur());
            if self.match_any(vec![AMPERSAND, PIPE, XOR, SHL, SHR]) {
                let oper = BitwiseOperation::from_token_type(&self.buffer.pop().unwrap().t_type).unwrap();
                let right = self.logical();
                left = Expression::Bitwise(Box::from(BitwiseExpressionNode::new(left, oper, right)))
            }
            self.buffer.pop();
            break
        }
        left
    }
    fn logical(&mut self) -> Expression {
        let mut left = self.comparative();
        loop {
            self.buffer.push(self.get_cur());
            if self.r#match(&AND) || self.r#match(&OR) {
                let oper = LogicalOperation::from_token_type(&self.buffer.pop().unwrap().t_type).unwrap();
                let right = self.comparative();
                left = Expression::Logical(Box::from(LogicalExpressionNode::new(left, oper, right)))
            }
            self.buffer.pop();
            break
        }
        left
    }
    fn comparative(&mut self) -> Expression {
        let mut left = self.additive();
        loop {
            self.buffer.push(self.get_cur());
            if self.match_any(vec![GT, LS, EQ, LEQ, GEQ, NEQ]) {
                let oper = ConditionalOperation::from_token_type(&self.buffer.pop().unwrap().t_type).unwrap();
                let right = self.additive();
                left = Expression::Comparative(Box::from(ComparativeExpressionNode::new(left, oper, right)))
            }
            self.buffer.pop();
            break
        }
        left
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
        let mut left = self.exponential();
        loop {
            self.buffer.push(self.get_cur());
            if self.r#match(&STAR) || self.r#match(&SLASH) ||self.r#match(&REM) {
                let oper = BinaryOperation::from_token_type(&self.buffer.pop().unwrap().t_type).unwrap();
                let right = self.exponential();
                left = Expression::Binary(Box::from(BinaryExpressionNode::new(left, oper, right)))
            }
            self.buffer.pop();
            break
        }
        left
    }

    fn exponential(&mut self) -> Expression {
        let mut left = self.unary();
        loop {
            self.buffer.push(self.get_cur());
            if self.r#match(&POWER) {
                let right = self.unary();
                left = Expression::Binary(Box::from(BinaryExpressionNode::new(left, BinaryOperation::Power, right)))
            }
            self.buffer.pop();
            break
        }
        left
    }

    fn unary(&mut self) -> Expression {
        let mut left = self.primary();
        loop {
            self.buffer.push(self.get_cur());
            if self.r#match(&INC) || self.r#match(&DEC) {
                let oper = UnaryOperation::from_token_type(&self.buffer.pop().unwrap().t_type).unwrap();
                left = Expression::Unary(Box::from(UnaryExpressionNode::new(left, oper)))
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
        if self.r#match(&OMNI) {
            let buffered  = &self.buffer.pop().unwrap().t_value;
            return Expression::Literal(Box::from(LiteralExpression::Omni(OmniNode::new(buffered.parse::<f128>().unwrap()))))
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
        if self.r#match(&CHARARR) {
            let buffered = &self.buffer.pop().unwrap().t_value;
            return Expression::Literal(Box::from(LiteralExpression::Chararr(CharArrNode::new(buffered.clone()))))
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