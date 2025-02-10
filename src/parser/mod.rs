use crate::core::errors::Failure;
use crate::lexer::token::{Token, TokenType};
use crate::parser::ast::{AssignmentStatementNode, BPrintStatementNode, BinaryExpressionNode, BinaryOperator, BlockStatementNode, ConditionalExpressionNode, ConditionalOperator, Expression, IfStatementNode, LiteralExpression, Statement, VariableAccessExpressionNode, VariableDeclarationStatementNode};

pub mod ast;

pub struct Parser {
    input: Vec<Token>,
    __pos: usize,
    __src_table: Vec<String>
}

impl Parser {
    pub fn new(input: Vec<Token>, src_table: Vec<String>) -> Self {
        let __pos = 0usize;
        Self {
            input,
            __pos,
            __src_table: src_table
        }
    }

    pub fn parse(&mut self) -> Result<Statement, Failure> {
        let mut prog = BlockStatementNode::new(Vec::new());
        while !self.m(TokenType::EOF) {
            match self.statement() {
                Ok(s) => prog.add_stmt(s),
                Err(e) => return Err(e)
            }
        }

        Ok(Statement::Block(Box::from(prog)))
    }
    fn statement(&mut self) -> Result<Statement, Failure> {
        if self.m(TokenType::BPRINT) {
            return self.bprint()
        }
        if self.m(TokenType::MK) {
            return self.variable_decl()
        }
        if self.m(TokenType::Ident) {
            return self.assignment()
        }
        if self.m(TokenType::IF) {
            return self.r#if()
        }
        if self.m(TokenType::LBRACE) {
            return self.block()
        }
        else {
            let cur = self.get_current().unwrap();
            if cur.is_expression() {
                Err(Failure::ParserUsingExpressionAsStatement { src: self.__src_table.get(cur.get_pos().0).cloned().unwrap(), span: cur.get_pos().into(), expr: cur.get_value() })
            } else {
                Err(Failure::ParserUnexpectedToken { src: self.__src_table.get(cur.get_pos().0).cloned().unwrap(), span: cur.get_pos().into(), token: cur })
            }
        }
    }

    fn block(&mut self) -> Result<Statement, Failure> {
        let mut body: Vec<Statement> = Vec::new();
        while !self.m(TokenType::RBRACE) {
            body.push(self.statement()?)
        };
        Ok(Statement::Block(Box::from(BlockStatementNode::new(body))))
    }

    fn r#if(&mut self) -> Result<Statement, Failure> {
        let expr = self.expression()?;
        let then = self.statement()?;
        if self.m(TokenType::ELSE) {
            let r#else = self.statement()?;
            Ok(Statement::If(Box::from(IfStatementNode::new(expr, then, Some(r#else)))))
        } else {
            Ok(Statement::If(Box::from(IfStatementNode::new(expr, then, None))))
        }
    }
    fn variable_decl(&mut self) -> Result<Statement, Failure> {
        let name = self.r(TokenType::Ident)?;
        self.r(TokenType::ASSIGN)?;
        let val = self.expression()?;
        Ok(Statement::VariableDeclaration(Box::from(VariableDeclarationStatementNode::new(name.get_value_clone(), val))))
    }
    fn assignment(&mut self) -> Result<Statement, Failure> {
        let name = self.get_prev().unwrap().get_value();
        self.r(TokenType::ASSIGN)?;
        let val = self.expression()?;
        Ok(Statement::Assignment(Box::from(AssignmentStatementNode::new(name, val))))
    }

    fn bprint(&mut self) -> Result<Statement, Failure> {
        self.r(TokenType::LPAREN)?;
        let expr = self.expression();
        self.r(TokenType::RPAREN)?;
        match expr {
            Ok(e) => Ok(Statement::BPrintStatement(Box::from(BPrintStatementNode::new(e)))),
            Err(f) => Err(f)
        }
    }
    fn expression(&mut self) -> Result<Expression, Failure> {
        self.comparative()
    }
    fn comparative(&mut self) -> Result<Expression, Failure> {
        let mut left = self.additive()?;
        loop {
            if self.m(TokenType::EQ)
                || self.m(TokenType::NEQ)
                || self.m(TokenType::GT)
                || self.m(TokenType::LT)
                || self.m(TokenType::GE)
                || self.m(TokenType::LE) {
                let oper = ConditionalOperator::from_token(&self.get_prev().unwrap());
                let right = self.additive()?;
                left = Expression::Conditional(Box::from(ConditionalExpressionNode::new(left, oper?, right)));
                continue
            }
            break
        }
        Ok(left)
    }
    fn additive(&mut self) -> Result<Expression, Failure> {
        let mut left = self.multiplicative();
        match left {
            Ok(mut l) => {
                loop {
                    if self.m(TokenType::PLUS) || self.m(TokenType::MINUS) {
                        let oper = BinaryOperator::from_token(&self.get_prev().unwrap());
                        let right = self.multiplicative();
                        match right {
                            Ok(r) => {
                                l = Expression::Binary(Box::from(BinaryExpressionNode::new(l, oper?, r)));
                                continue
                            }
                            Err(f) => return Err(f)
                        }
                    }
                    break;
                }
                Ok(l)
            }
            Err(f) => Err(f)
        }
    }

    fn multiplicative(&mut self) -> Result<Expression, Failure> {
        let mut left = self.primary();
        match left {
            Ok(mut l) => {
                loop {
                    if self.m(TokenType::ASTER) || self.m(TokenType::SLASH) {
                        let oper = BinaryOperator::from_token(&self.get_prev().unwrap());
                        let right = self.primary();
                        match right {
                            Ok(r) => {
                                l = Expression::Binary(Box::from(BinaryExpressionNode::new(l, oper?, r)));
                                continue
                            }
                            Err(f) => return Err(f)
                        }
                    }
                    break;
                }
                Ok(l)
            }
            Err(f) => Err(f)
        }
    }
    fn primary(&mut self) -> Result<Expression, Failure> {
        if self.m(TokenType::NumberLiteral { size: 32 }) {
            let lit = self.get_prev().unwrap();
            return Ok(Expression::Literal(LiteralExpression::Integer(lit.get_value_clone().parse::<i32>().expect(format!("Cannot format {} as integer", lit.get_value()).as_str()))))
        }
        if self.m(TokenType::LPAREN) {
            let expr = self.expression();
            return match expr {
                Ok(e) => {
                    self.r(TokenType::RPAREN)?;
                    Ok(e)
                }
                Err(f) => Err(f)
            }
        }
        if self.m(TokenType::Ident) {
            let name = self.get_prev().unwrap();
            return Ok(Expression::VariableAccess(Box::from(VariableAccessExpressionNode::new(name.get_value_clone()))))
        }
        else {
            let cur = self.get_current().unwrap();
            Err(Failure::ParserUnexpectedToken { src: self.__src_table.get(cur.get_pos().0).cloned().unwrap(), span: cur.get_pos().into(), token: cur })
        }
    }

    fn get_prev(&self) -> Option<Token> { self.input.get(self.__pos-1).cloned() }
    fn get_current(&self) -> Option<Token> { self.input.get(self.__pos).cloned() }
    fn step(&mut self) { self.__pos += 1 }
    fn m(&mut self, expected: TokenType) -> bool {
        //! `match` function
        if let Some(t) = self.get_current() {
            match t.get_type() == expected {
                true => { self.step(); true }
                false => false
            }
        } else {
            panic!()
        }
    }
    fn r(&mut self, required: TokenType) -> Result<Token, Failure> {
        //! `require` function
        if let Some(t) = self.get_current() {
            match t.get_type() == required {
                true => { self.step(); Ok(t) }
                false => Err( Failure::ParserUnexpectedTokenWithType { src: self.__src_table.get(t.get_pos().0).cloned().unwrap(), span: t.get_pos().into(), required, got: t.get_type() } )
            }
        } else {
            panic!()
        }
    }
}