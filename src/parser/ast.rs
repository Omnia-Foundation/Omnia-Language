use std::fmt::{write, Display, Formatter};
use crate::core::errors::Failure;
use crate::lexer::token::{Token, TokenType};

pub enum Node {
    Statement(Statement),
    Expression(Expression)
}

impl Display for Node {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Node::Statement(s) => write!(f, "{}", s),
            Node::Expression(e) => write!(f, "{}", e)
        }
    }
}

#[derive(Clone)]
pub enum Statement {
    Block(Box<BlockStatementNode>),
    BPrintStatement(Box<BPrintStatementNode>),
    VariableDeclaration(Box<VariableDeclarationStatementNode>),
    Assignment(Box<AssignmentStatementNode>),
    If(Box<IfStatementNode>)
}
impl Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::BPrintStatement(b) => write!(f, "{}", b),
            Statement::Block(b) => write!(f, "{}", b),
            Statement::VariableDeclaration(v) => write!(f, "{}", v),
            Statement::Assignment(a) => write!(f, "{}", a),
            Statement::If(i) => write!(f, "{}", i)
        }
    }
}
#[derive(Clone)]
pub enum Expression {
    Binary(Box<BinaryExpressionNode>),
    Literal(LiteralExpression),
    VariableAccess(Box<VariableAccessExpressionNode>),
    Conditional(Box<ConditionalExpressionNode>)
    
}
impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Literal(l) => write!(f, "{}", l),
            Expression::Binary(b) => write!(f, "{}", b),
            Expression::VariableAccess(v) => write!(f, "{}", v),
            Expression::Conditional(c) => write!(f, "{}", c)
        }
    }
}

#[derive(Clone)]
pub struct VariableAccessExpressionNode {
    name: String
}
impl VariableAccessExpressionNode {
    pub fn new(name: String) -> Self {
        Self {
            name
        }
    }

    pub fn get_name(&self) -> &String { &self.name }
    pub fn get_name_clone(&self) -> String { self.name.clone() }
}

impl Display for VariableAccessExpressionNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "VariableAccess(name: {})", self.name)
    }
}

#[derive(Clone)]
pub enum LiteralExpression {
    Integer(i32)
}
impl Display for LiteralExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            LiteralExpression::Integer(i) => write!(f, "Literal(Integer({}))", i)
        }
    }
}

#[derive(Clone)]
pub struct BlockStatementNode {
    body: Vec<Statement>
}
impl Display for BlockStatementNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "BlockStatement([\n\t")?;
        for (i, val) in self.body.iter().enumerate() {
            if i > 0 {
                write!(f, "\n\t")?;
            }
            write!(f, "{}", val)?
        };
        write!(f, "\n\t])")
    }
}

impl BlockStatementNode {
    pub fn new(body: Vec<Statement>) -> Self {
        Self {
            body
        }
    }
    pub fn add_stmt(&mut self, stmt: Statement) { self.body.push(stmt) }
    pub fn get_stmts(self) -> Vec<Statement> { self.body }
    pub fn get_stmts_ref(&self) -> &Vec<Statement> { &self.body }
}

#[derive(Clone)]
pub struct AssignmentStatementNode {
    name: String,
    new_value: Expression
}
impl AssignmentStatementNode {
    pub fn new(name: String, new_value: Expression) -> Self {
        Self {
            name,
            new_value
        }
    }
    pub fn get_name(&self) -> &String { &self.name }
    pub fn get_name_clone(&self) -> String { self.name.clone() }
    pub fn get_value(&self) -> Expression { self.new_value.clone() }
    pub fn get_value_ref(&self) -> &Expression { &self.new_value }

}
impl Display for AssignmentStatementNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "AssignmentStatement(name: {}, new_value: {})", self.name, self.new_value)
    }
}

#[derive(Clone)]
pub struct BPrintStatementNode {
    value: Expression
}
impl Display for BPrintStatementNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "BuiltinPrint({})", self.value)
    }
}
impl BPrintStatementNode {
    pub fn new(value: Expression) -> Self {
        Self {
            value
        }
    }
    pub fn get_value(self) -> Expression { self.value }
    pub fn get_value_ref(&self) -> &Expression { &self.value }
}
#[derive(Clone)]
pub enum BinaryOperator {
    Plus,
    Minus,
    Mul,
    Div
}


impl Display for BinaryOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryOperator::Plus => write!(f, "+"),
            BinaryOperator::Minus => write!(f, "-"),
            BinaryOperator::Mul => write!(f, "*"),
            BinaryOperator::Div => write!(f, "/")
        }
    }
}
impl BinaryOperator {
    pub fn from_token(token: &Token) -> Result<BinaryOperator, Failure> {
        match token.get_type() {
            TokenType::PLUS => {
                Ok(BinaryOperator::Plus)
            }
            TokenType::MINUS => {
                Ok(BinaryOperator::Minus)
            }
            TokenType::ASTER => {
                Ok(BinaryOperator::Mul)
            }
            TokenType::SLASH => {
                Ok(BinaryOperator::Div)
            }
            _ => {
                Err(Failure::UtilsUnexpectedTokenForBinaryOperation { src: "".to_string(), span: token.get_pos().into(), token: token.clone() })
            }
        }
    }
}
#[derive(Clone)]
pub enum ConditionalOperator {
    Equals,
    NotEquals,
    Greater,
    Lesser,
    GreaterEquals,
    LesserEquals
}

impl Display for ConditionalOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ConditionalOperator::Equals => { write!(f, "==") }
            ConditionalOperator::NotEquals => { write!(f, "!=") }
            ConditionalOperator::Greater => { write!(f, ">") }
            ConditionalOperator::Lesser => { write!(f, "<") }
            ConditionalOperator::GreaterEquals => { write!(f, ">=") }
            ConditionalOperator::LesserEquals => { write!(f, "<=") }
        }
    }
}
impl ConditionalOperator {
    pub fn from_token(token: &Token) -> Result<Self, Failure> {
        match token.get_type() {
            TokenType::EQ => {
                Ok(ConditionalOperator::Equals)
            }
            TokenType::NEQ => {
                Ok(ConditionalOperator::NotEquals)
            }
            TokenType::GT => {
                Ok(ConditionalOperator::Greater)
            }
            TokenType::LT => {
                Ok(ConditionalOperator::Lesser)
            }
            TokenType::GE => {
                Ok(ConditionalOperator::GreaterEquals)
            }
            TokenType::LE => {
                Ok(ConditionalOperator::Equals)
            }
            _ => {
                Err(Failure::UtilsUnexpectedTokenForConditionalOperation { span: token.get_pos().into(), token: token.clone() })
            }
        }
    }
}

#[derive(Clone)]
pub struct ConditionalExpressionNode {
    pub(crate) left: Expression,
    pub(crate) oper: ConditionalOperator,
    pub(crate) right: Expression
}
impl Display for ConditionalExpressionNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "ConditionalExpression(left: {}, {}, right: {})", self.left, self.oper, self.right)
    }
}
impl ConditionalExpressionNode {
    pub fn new(left: Expression, oper: ConditionalOperator, right: Expression) -> Self {
        Self {
            left,
            oper,
            right
        }
    }
}

#[derive(Clone)]
pub struct BinaryExpressionNode {
    pub(crate) left: Expression,
    pub(crate) oper: BinaryOperator,
    pub(crate) right: Expression
}

impl Display for BinaryExpressionNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "BinaryExpression(left: {}, {}, right: {})", self.left, self.oper, self.right)
    }
}
impl BinaryExpressionNode {
    pub fn new(left: Expression, oper: BinaryOperator, right: Expression) -> Self {
        Self {
            left,
            oper,
            right
        }
    }

    pub fn get(self) -> (Expression, BinaryOperator, Expression) { (self.left, self.oper, self.right) }
}
#[derive(Clone)]
pub struct IfStatementNode {
    cond: Expression,
    then: Statement,
    r#else: Option<Statement>
}
impl IfStatementNode {
    pub fn new(cond: Expression, then: Statement, r#else: Option<Statement>) -> Self {
        Self {
            cond,
            then,
            r#else
        }
    }
    pub fn get_cond_ref(&self) -> &Expression { &self.cond }
    pub fn get_cond_clone(&self) -> Expression { self.cond.clone() }
    pub fn get_then_ref(&self) -> &Statement { &self.then }
    pub fn get_then_clone(&self) -> Statement { self.then.clone() }
    pub fn get_else_ref(&self) -> &Option<Statement> { &self.r#else }
    pub fn get_else_clone(&self) -> Option<Statement> { self.r#else.clone() }
}
impl Display for IfStatementNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "IfStatement(cond: {}, then: {}, else: {})", self.cond, self.then, match &self.r#else {
            Some(e) => {
                format!("{}", e)
            }
            None => {
                "None".to_string()
            }
        })
    }
}

#[derive(Clone)]
pub struct VariableDeclarationStatementNode {
    name: String,
    value: Expression
}

impl VariableDeclarationStatementNode {
    pub fn new(name: String, value: Expression) -> Self {
        Self {
            name,
            value
        }
    }
    pub fn get_value_ref(&self) -> &Expression { &self.value }
    pub fn get_name(&self) -> &String { &self.name }
    pub fn get_name_clone(&self) -> String { self.name.clone() }
}

impl Display for VariableDeclarationStatementNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "VariableDeclarationStatement(name: {}, value: {})", self.name, self.value)
    }
}