use num_traits::FromPrimitive;
use crate::core::omnia_types::{OmniaByte, OmniaChar, OmniaDecimal, OmniaInt, OmniaLong, OmniaSpan, OmniaUByte, OmniaUInt, OmniaULong, OmniaValue, Type};
use crate::lexer::token::TokenType;
use crate::parser::ast::ast_vm::AssignmentOperator::{ANDA, ASSIGN, DIVA, MINUSA, MULA, ORA, PLUSA, REMA};
use crate::parser::ast::ast_vm::BinaryOperation::{Add, Div, Mul, Rem, Sub};
use crate::parser::ast::ast_vm::ConditionalOperation::{Eq, Greater, GreaterEq, Less, LessEq, NotEq};
use crate::parser::ast::ast_vm::UnaryOperation::{Dec, Inc, Neg, Not};
use crate::parser::CompilerError;
use crate::parser::CompilerError::TypeError;

pub enum  ASTNode {
    Expression(Expression),
    Statement(Statement)
}

pub enum BinaryOperation {
    Add,
    Sub,
    Mul,
    Div,
    Rem
}
pub enum UnaryOperation {
    Inc,
    Dec,
    Not,
    Neg
}
pub enum ConditionalOperation {
    Greater,
    Less,
    GreaterEq,
    LessEq,
    Eq,
    NotEq
}
pub enum Literal {
    Byte(i8),
    Int(i32),
    Long(i64),
    UByte(u8),
    UInt(u32),
    ULong(u64),
    Decimal(f64),
    Char(char),
    

}
pub enum LiteralExpression {
    Byte(ByteNode),
    Int(IntNode),
    Long(LongNode),
    UByte(UByteNode),
    UInt(UIntNode),
    ULong(ULongNode),
    Decimal(DecimalNode),
    Char(CharNode)
}
pub enum Expression {
    Literal(Box<LiteralExpression>),
    Binary(Box<BinaryOperation>),
    Unary(Box<UnaryExpressionNode>),
    VariableAccess(Box<VariableAccessExpressionNode>)
}
pub enum Statement {
    If(Box<IfStatementNode>),
    Block(Box<BlockStatementNode>),
    Plug(Box<PlugStatementNode>),
    FunctionCall(Box<FunctionCallStatementNode>),
    FunctionDeclaration(Box<FunctionDeclarationStatementNode>),
    Assignment(Box<AssignmentStatementNode>),
    VariableCreation(Box<VariableCreationStatementNode>),
    Return(Box<ReturnStatementNode>)
}
impl ConditionalOperation {
    pub fn from_string(string: &str) -> Result<ConditionalOperation, CompilerError> {
        match string {
            ">" => {
                Ok(Greater)
            }
            "<" => {
                Ok(Less)
            }
            ">=" => {
                Ok(GreaterEq)
            }
            "<=" => {
                Ok(LessEq)
            }
            "==" => {
                Ok(Eq)
            }
            "!=" => {
                Ok(NotEq)
            }
            &_ => {
                Err(TypeError(format!("Unexpected conditional operator in string {}", string)))
            }
        }
    }
    pub fn from_token_type(t: &TokenType) -> Result<ConditionalOperation, CompilerError> {
        if !TokenType::is_conditional_operator(t) {
            return Err(TypeError(format!("Unexpected token type ({}) for conditional operator", t)))
        }
        match t {
            TokenType::GT => {
                Ok(Greater)
            }
            TokenType::LS => {
                Ok(Less)
            }
            TokenType::GEQ => {
                Ok(GreaterEq)
            }
            TokenType::LEQ => {
                Ok(LessEq)
            }
            TokenType::EQ => {
                Ok(Eq)
            }
            TokenType::NEQ => {
                Ok(NotEq)
            }
            &_ => {
                Err(TypeError(format!("Unexpected conditional operator in token {}", t)))
            }
        }
    }
}

impl UnaryOperation {
    pub fn from_string(string: &str) -> Result<UnaryOperation, CompilerError> {
        match string {
            "++" => {
                Ok(Inc)
            }
            "--" => {
                Ok(Dec)
            }
            "!" => {
                Ok(Not)
            }
            "-" => {
                Ok(Neg)
            }
            &_ => {
                Err(TypeError(format!("Unexpected unary operator in string {}", string)))
            }
        }
    }
    pub fn from_token_type(t: &TokenType) -> Result<UnaryOperation, CompilerError> {
        if !TokenType::is_unary_operator(t) {
            return Err(TypeError(format!("Unexpected token type ({}) for unary operator", t)))
        }
        match t {
            TokenType::INC => {
                Ok(Inc)
            }
            TokenType::DEC => {
                Ok(Dec)
            }
            TokenType::NOT => {
                Ok(Not)
            }
            TokenType::MINUS => {
                Ok(Neg)
            }
            &_ => {
                Err(TypeError(String::new()))
            }
        }
    }
}

impl BinaryOperation {
    pub fn from_string(string: &str) -> Result<BinaryOperation, CompilerError> {
        match string {
            "+" => {
                Ok(Add)
            }
            "-" => {
                Ok(Sub)
            }
            "*" => {
                Ok(Mul)
            }
            "/" => {
                Ok(Div)
            }
            "%" => {
                Ok(Rem)
            }
            &_ => {
                Err(TypeError(format!("Unexpected binary operator in string {}", string)))
            }
        }
    }
    pub fn from_token_type(t: &TokenType) -> Result<BinaryOperation, CompilerError> {
        if !TokenType::is_arithmetic_operator(t) {
            return Err(TypeError(format!("Unexpected token type ({}) for binary operator", t)))
        }
        match t {
            TokenType::PLUS => {
                Ok(Add)
            }
            TokenType::MINUS => {
                Ok(Sub)
            }
            TokenType::STAR => {
                Ok(Mul)
            }
            TokenType::SLASH => {
                Ok(Div)
            }
            TokenType::REM => {
                Ok(Rem)
            }
            &_ => {
                Err(TypeError(String::new()))
            }
        }
    }
}
pub enum AssignmentOperator {
    ASSIGN,
    PLUSA,
    MINUSA,
    MULA,
    DIVA,
    REMA,
    ANDA,
    ORA
}
impl AssignmentOperator {
    pub fn from_string(string: &str) -> Result<AssignmentOperator, CompilerError> {
        match string {
            "=" => {
                Ok(ASSIGN)
            }
            "+=" => {
                Ok(PLUSA)
            }
            "-=" => {
                Ok(MINUSA)
            }
            "*=" => {
                Ok(MULA)
            }
            "/=" => {
                Ok(DIVA)
            }
            "%=" => {
                Ok(REMA)
            }
            "&=" => {
                Ok(ANDA)
            }
            "|=" => {
                Ok(ORA)
            }
            &_ => {
                Err(TypeError(format!("Unexpected assignment operator in string {}", string)))
            }
        }
    }
    pub fn from_token_type(t: &TokenType) -> Result<AssignmentOperator, CompilerError> {
        if !TokenType::is_assignment_operator(t) {
            return Err(TypeError(format!("Unexpected token type ({}) for assignment operator", t)))
        }
        match t {
            TokenType::ASSIGN => {
                Ok(ASSIGN)
            }
            TokenType::PLUSASSIGN => {
                Ok(PLUSA)
            }
            TokenType::MINUSASSIGN => {
                Ok(MINUSA)
            }
            TokenType::MULASSIGN => {
                Ok(MULA)
            }
            TokenType::DIVASSIGN => {
                Ok(DIVA)
            }
            TokenType::REMASSIGN => {
                Ok(REMA)
            }
            TokenType::ANDASSIGN => {
                Ok(ANDA)
            }
            TokenType::ORASSIGN => {
                Ok(ORA)
            }
            &_ => {
                Err(TypeError(String::new()))
            }
        }
    }
}

pub struct IntNode {
    value: OmniaInt
}
pub struct ByteNode {
    value: OmniaByte
}
pub struct LongNode {
    value: OmniaLong
}
pub struct DecimalNode {
    value: OmniaDecimal
}
pub struct UByteNode {
    value: OmniaUByte
}
pub struct UIntNode {
    value: OmniaUInt
}
pub struct ULongNode {
    value: OmniaULong
}
pub struct CharNode {
    value: OmniaChar
}
pub struct SpanNode<T: FromPrimitive + Clone> {
    value: OmniaSpan<T>
}

pub struct BinaryExpressionNode {
    left: Expression,
    op: BinaryOperation,
    right: Expression
}
pub struct UnaryExpressionNode {
    value: Expression,
    op: UnaryOperation
}

pub struct VariableAccessExpressionNode {
    name: String
}
impl VariableAccessExpressionNode {
    pub fn new(name: String) -> Self {
        Self {
            name
        }
    }
}

pub struct AssignmentStatementNode {
    cont: Expression,
    op: AssignmentOperator,
    value: Expression
}
pub struct FunctionCallStatementNode {
    name: String,
    args: Option<Vec<ASTNode>>
}
pub struct FunctionDeclarationStatementNode {
    name: String,
    args: Vec<Statement>,
    body: Statement
}
pub struct VariableCreationStatementNode {
    name: String,
    r#type: Type,
    value: Expression
}
impl VariableCreationStatementNode {
    pub fn new(name: String, r#type: Type, value: Expression) -> VariableCreationStatementNode {
        Self {
            name,
            r#type,
            value
        }
    }
}

pub struct ReturnStatementNode {
    value: ASTNode
}
impl ReturnStatementNode {
    pub fn new(value: ASTNode) -> ReturnStatementNode {
        Self {
            value
        }
    }
}

pub struct PlugStatementNode {
    lib: String
}
impl PlugStatementNode {
    pub fn new(lib: String) -> Self {
        Self {
            lib
        }
    }
}
pub struct IfStatementNode {
    cond: ConditionalOperation,
    then: Statement,
    r#else: Statement
}
pub struct BlockStatementNode {
    body: Vec<ASTNode>
}
impl BlockStatementNode {
    pub fn new() -> BlockStatementNode {
        Self {
            body: Vec::new()
        }
    }
    pub fn add_node(&mut self, node: ASTNode) {
        self.body.push(node)
    }
}




impl IntNode {
    pub fn new(value: i32) -> IntNode {
        Self {
            value: OmniaInt::get_from(value)
        }
    }
}
impl ByteNode {
    pub fn new(value: i8) -> Self {
        Self {
            value: OmniaByte::get_from(value)
        }
    }
}
impl LongNode {
    pub fn new(value: i64) -> Self {
        Self {
            value: OmniaLong::get_from(value)
        }
    }
}
impl DecimalNode {
    pub fn new(value: f64) -> Self {
        Self {
            value: OmniaDecimal::get_from(value)
        }
    }
}
impl UByteNode {
    pub fn new(value: u8) -> Self {
        Self {
            value: OmniaUByte::get_from(value)
        }
    }
}
impl UIntNode {
    pub fn new(value: u32) -> Self {
        Self {
            value: OmniaUInt::get_from(value)
        }
    }
}
impl ULongNode {
    pub fn new(value: u64) -> Self {
        Self {
            value: OmniaULong::get_from(value)
        }
    }
}
impl CharNode {
    pub fn new(value: char) -> Self {
        Self {
            value: OmniaChar::get_from(value)
        }
    }
}
impl <T: FromPrimitive + Clone> SpanNode<T> {
    pub fn new(value: Vec<T>) -> Self {
        Self {
            value: OmniaSpan::get_from(value)
        }
    }
}