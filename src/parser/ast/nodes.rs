use std::fmt::{write, Display, Formatter};
use std::io::stdin;
use num_traits::FromPrimitive;
use crate::core::omnia_types::{OmniaByte, OmniaChar, OmniaChararr, OmniaDecimal, OmniaInt, OmniaLong, OmniaOmni, OmniaSpan, OmniaUByte, OmniaUInt, OmniaULong, OmniaValue, Type};
use crate::core::utils::numeric_utils::omni::f128;
use crate::lexer::token::TokenType;
use crate::parser::ast::nodes::AssignmentOperator::{ANDA, ASSIGN, DIVA, MINUSA, MULA, ORA, PLUSA, REMA};
use crate::parser::ast::nodes::BinaryOperation::{Add, Div, Mul, Rem, Sub, Power};
use crate::parser::ast::nodes::BitwiseOperation::{BitwiseAnd, BitwiseOr, BitwiseXor, ShiftL, ShiftR};
use crate::parser::ast::nodes::ConditionalOperation::{Eq, Greater, GreaterEq, Less, LessEq, NotEq};
use crate::parser::ast::nodes::LogicalOperation::{And, Or};
use crate::parser::ast::nodes::UnaryOperation::{Dec, Inc, Neg, Not};
use crate::parser::CompilerError;
use crate::parser::CompilerError::TypeError;

pub enum ASTNode {
    Expression(Expression),
    Statement(Statement)
}

trait CanBeVisible {
    fn is_visible(&self) -> bool;
}

impl Display for ASTNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ASTNode::Expression(e) => { write!(f, "{}", e) }
            ASTNode::Statement(s) => { write!(f, "{}", s) }
        }
    }
}

pub enum BinaryOperation {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Power
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
pub enum LogicalOperation {
    And,
    Or
}
pub enum BitwiseOperation {
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    ShiftL,
    ShiftR
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
    Omni(OmniNode),
    Char(CharNode),
    Chararr(CharArrNode)
}
impl Display for LiteralExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            LiteralExpression::Byte(v) => { write!(f, "Byte literal: {}", v.value.get_as_int32()) }
            LiteralExpression::Int(v) => { write!(f, "Int literal: {}", v.value.get_as_int32())}
            LiteralExpression::Long(v) => { write!(f, "Long literal: {}", v.value.get_value_as::<i64>()) }
            LiteralExpression::UByte(v) => { write!(f, "Unsigned Byte literal: {}", v.value.get_value_as::<u8>()) }
            LiteralExpression::UInt(v) => { write!(f, "Unsigned Int literal: {}", v.value.get_value_as::<u32>()) }
            LiteralExpression::ULong(v) => { write!(f, "Unsigned Byte literal: {}", v.value.get_value_as::<u64>()) }
            LiteralExpression::Decimal(v) => { write!(f, "Decimal literal: {}", v.value.get_as_decimal()) }
            LiteralExpression::Omni(v) => { write!(f, "Omni literal: {}", v.value.get_value_as::<String>()) }
            LiteralExpression::Char(v) => { write!(f, "Char literal: {}", v.value.get_value_as::<char>()) }
            LiteralExpression::Chararr(v) => { write!(f, "Chararr literal: {}", v.value.get_as_string()) }
        }
    }
}
impl Display for LogicalOperation {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            LogicalOperation::And => { write!(f, "&&") }
            LogicalOperation::Or => { write!(f, "||") }
        }
    }
}
pub enum Expression {
    Literal(Box<LiteralExpression>),
    Binary(Box<BinaryExpressionNode>),
    Unary(Box<UnaryExpressionNode>),
    FunctionCall(Box<FunctionCallNode>),
    VariableAccess(Box<VariableAccessExpressionNode>),
    Argument(Box<ArgumentExpressionNode>),
    Comparative(Box<ComparativeExpressionNode>),
    Logical(Box<LogicalExpressionNode>),
    Bitwise(Box<BitwiseExpressionNode>)
}
impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Literal(v) => { write!(f, "{}", v) }
            Expression::Binary(v) => { write!(f, "{}", v) }
            Expression::Unary(v) => { write!(f, "{}", v) }
            Expression::FunctionCall(v) => { write!(f, "{}", v) }
            Expression::VariableAccess(v) => { write!(f, "{}", v) }
            Expression::Argument(v) => { write!(f, "{}", v) }
            Expression::Comparative(v) => { write!(f, "{}", v) }
            Expression::Logical(v) => { write!(f, "{}", v) }
            Expression::Bitwise(v) => { write!(f, "{}", v) }
        }
    }
}

pub enum Statement {
    If(Box<IfStatementNode>),
    Block(Box<BlockStatementNode>),
    Plug(Box<PlugStatementNode>),
    FunctionDeclaration(Box<FunctionDeclarationStatementNode>),
    FunctionCall(Box<FunctionCallNode>),
    Assignment(Box<AssignmentStatementNode>),
    VariableCreation(Box<VariableCreationStatementNode>),
    Return(Box<ReturnStatementNode>),
    LambdaDeclaration(Box<LambdaDeclarationStatementNode>),
    StructDeclaration(Box<StructDeclarationStatementNode>)
}
impl Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::If(v) => { write!(f, "{}", v) }
            Statement::Block(v) => { write!(f, "{}", v) }
            Statement::Plug(v) => { write!(f, "{}", v) }
            Statement::FunctionDeclaration(v) => { write!(f, "{}", v) }
            Statement::FunctionCall(v) => { write!(f, "{}", v) }
            Statement::Assignment(v) => { write!(f, "{}", v) }
            Statement::VariableCreation(v) => { write!(f, "{}", v) }
            Statement::Return(v) => { write!(f, "{}", v) }
            Statement::LambdaDeclaration(v) => { write!(f, "{}", v) }
            Statement::StructDeclaration(v) => { write!(f, "{}", v) }
        }
    }
}
impl Display for ConditionalOperation {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Greater => { write!(f, ">") }
            Less => { write!(f, "<") }
            GreaterEq => { write!(f, ">=") }
            LessEq => { write!(f, "<=") }
            Eq => { write!(f, "==") }
            NotEq => { write!(f, "!=") }
        }
    }
}
impl LogicalOperation {
    pub fn from_string(string: &str) -> Result<LogicalOperation, CompilerError> {
        match string {
            "&&" => {
                Ok(And)
            }
            "||" => {
                Ok(Or)
            }
            _ => {
                Err(TypeError(format!("Unexpected logical operator in string {}", string)))
            }
        }
    }
    pub fn from_token_type(t: &TokenType) -> Result<LogicalOperation, CompilerError> {
        if !TokenType::is_logical_operator(t) {
            return Err(TypeError(format!("Unexpected token type ({}) for logical operator", t)))
        }
        match t {
            TokenType::AND => {
                Ok(And)
            }
            TokenType::OR => {
                Ok(Or)
            }
            _ => {
                Err(TypeError(format!("Unexpected logical operator in token {}", t)))
            }
        }
    }
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
impl Display for UnaryOperation {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Inc => { write!(f, "++") }
            Dec => { write!(f, "--") }
            Not => { write!(f, "!") }
            Neg => { write!(f, "-") }
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
impl Display for BinaryOperation {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Add => { write!(f, "+") }
            Sub => { write!(f, "-") }
            Mul => { write!(f, "*") }
            Div => { write!(f, "/") }
            Rem => { write!(f, "%") }
            Power => { write!(f, "^") }
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

impl BitwiseOperation {
    pub fn from_token_type(t: &TokenType) -> Result<BitwiseOperation, CompilerError> {
        if !TokenType::is_bitwise_operator(t) {
            return Err(TypeError(format!("Unexpected token type ({}) for bitwise operator", t)))
        }
        match t {
            TokenType::AMPERSAND => {
                Ok(BitwiseAnd)
            }
            TokenType::PIPE => {
                Ok(BitwiseOr)
            }
            TokenType::XOR => {
                Ok(BitwiseXor)
            }
            TokenType::SHL => {
                Ok(ShiftL)
            }
            TokenType::SHR => {
                Ok(ShiftR)
            }
            _ => {
                Err(TypeError(String::new()))
            }
        }
    }
}
impl Display for BitwiseOperation {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            BitwiseAnd => { write!(f, "&") }
            BitwiseOr => { write!(f, "|") }
            BitwiseXor => { write!(f, "~") }
            ShiftL => { write!(f, "<|") }
            ShiftR => { write!(f, "|>") }
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
impl Display for AssignmentOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ASSIGN => { write!(f, "=") }
            PLUSA => { write!(f, "+=") }
            MINUSA => { write!(f, "-=") }
            MULA => { write!(f, "*=") }
            DIVA => { write!(f, "/=") }
            REMA => { write!(f, "%=") }
            ANDA => { write!(f, "&=") }
            ORA => { write!(f, "|=") }
        }
    }
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
pub struct OmniNode {
    value: OmniaOmni
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
pub struct CharArrNode {
    value: OmniaChararr
}
pub struct SpanNode<T: FromPrimitive + Clone> {
    value: OmniaSpan<T>
}

pub struct BinaryExpressionNode {
    left: Expression,
    op: BinaryOperation,
    right: Expression
}
impl BinaryExpressionNode {
    pub fn new(left: Expression, op: BinaryOperation, right: Expression) -> BinaryExpressionNode {
        Self {
            left,
            op,
            right
        }
    }
}
impl Display for BinaryExpressionNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "BinaryExpression({}, {}, {})", self.left, self.op, self.right)
    }
}
pub struct ComparativeExpressionNode {
    left: Expression,
    op: ConditionalOperation,
    right: Expression
}
impl ComparativeExpressionNode {
    pub fn new(left: Expression, op: ConditionalOperation, right: Expression) -> ComparativeExpressionNode {
        Self {
            left,
            op,
            right
        }
    }
}
impl Display for ComparativeExpressionNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "ComparativeExpression({}, {}, {})", self.left, self.op, self.right)
    }
}

pub struct LogicalExpressionNode {
    left: Expression,
    op: LogicalOperation,
    right: Expression
}
impl LogicalExpressionNode {
    pub fn new(left: Expression, op: LogicalOperation, right: Expression) -> Self {
        Self {
            left,
            op,
            right
        }
    }
}
impl Display for LogicalExpressionNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "LogicalExpression({}, {}, {})", self.left, self.op, self.right)
    }
}

pub struct BitwiseExpressionNode {
    left: Expression,
    op: BitwiseOperation,
    right: Expression
}
impl BitwiseExpressionNode {
    pub fn new(left: Expression, op: BitwiseOperation, right: Expression) -> Self {
        Self {
            left,
            op,
            right
        }
    }
}
impl Display for BitwiseExpressionNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "BitwiseExpression({}, {}, {})", self.left, self.op, self.right)
    }
}

pub struct UnaryExpressionNode {
    value: Expression,
    op: UnaryOperation
}
impl UnaryExpressionNode {
    pub fn new(value: Expression, op: UnaryOperation) -> Self {
        Self {
            value,
            op
        }
    }
}
impl Display for UnaryExpressionNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "UnaryExpression({}, {})", self.value, self.op)
    }
}

pub struct VariableAccessExpressionNode {
    name: String
}
impl Display for VariableAccessExpressionNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "VariableAccess({})", self.name)
    }
}
impl VariableAccessExpressionNode {
    pub fn new(name: String) -> Self {
        Self {
            name
        }
    }
}
pub struct ArgumentExpressionNode {
    name: String,
    r#type: Type
}

impl Display for ArgumentExpressionNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Argument({}, {})", self.name, self.r#type)
    }
}
impl ArgumentExpressionNode {
    pub fn new(name: String, r#type: Type) -> Self {
        Self {
            name,
            r#type
        }
    }
}

pub struct FieldExpressionNode {
    name: String,
    r#type: Type
}

impl FieldExpressionNode {
    pub fn new(name: String, r#type: Type) -> Self {
        Self {
            name,
            r#type
        }
    }
}
impl Display for FieldExpressionNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Field({}: {})", self.name, self.r#type)
    }
}

pub struct StructDeclarationStatementNode {
    name: String,
    fields: Vec<FieldExpressionNode>,
    is_visible: bool
}

impl StructDeclarationStatementNode {
    pub fn new(name: String, fields: Vec<FieldExpressionNode>, is_visible: bool) -> StructDeclarationStatementNode {
        Self {
            name,
            fields,
            is_visible
        }
    }
}
impl Display for StructDeclarationStatementNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "StructDeclaration({}, [", self.name)?;
        for (i, val) in self.fields.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", val)?;
        }
        write!(f, "], visible: {})", self.is_visible)
    }
}
impl CanBeVisible for StructDeclarationStatementNode {
    fn is_visible(&self) -> bool {
        self.is_visible
    }
}

pub struct LambdaDeclarationStatementNode {
    name: String,
    args: Vec<Expression>,
    returns: Type,
    body: Statement

}
impl LambdaDeclarationStatementNode {
    pub fn new(name: String, args: Vec<Expression>, returns: Type, body: Statement) -> Self {
        Self {
            name,
            args,
            returns,
            body
        }
    }
}
impl Display for LambdaDeclarationStatementNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "LambdaDeclaration({}, [", self.name)?;
        for (i, val) in self.args.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", val)?;
        }
        write!(f, "], {}, {}", self.returns, self.body)
    }
}

pub struct AssignmentStatementNode {
    cont: String,
    op: AssignmentOperator,
    value: Expression
}
impl AssignmentStatementNode {
    pub fn new(cont: String, op: AssignmentOperator, value: Expression) -> Self {
        Self {
            cont,
            op,
            value
        }
    }
}
impl Display for AssignmentStatementNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Assignment({} {} {})", self.cont, self.op, self.value)
    }
}
pub struct FunctionCallNode {
    name: String,
    args: Option<Vec<ASTNode>>
}
impl FunctionCallNode {
    pub fn new(name: String, args: Option<Vec<ASTNode>>) -> Self {
        Self {
            name,
            args
        }
    }
}
impl Display for FunctionCallNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if let Some(v) = &self.args {
            write!(f, "FunctionCall({}, [", self.name)?;
            for (i, val) in v.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{}", val)?;
            }
            write!(f, "]")
        }
        else {
            write!(f, "FunctionCall({})", self.name)
        }
    }
}
pub struct FunctionDeclarationStatementNode {
    name: String,
    args: Vec<Expression>,
    returns: Type,
    body: Statement,
    is_visible: bool
}

impl FunctionDeclarationStatementNode {
    pub fn new(name: String, args: Vec<Expression>, returns: Type, body: Statement, is_visible: bool) -> Self {
        Self {
            name,
            args,
            returns,
            body,
            is_visible
        }
    }
    pub fn get_name(&self) -> &String {
        &self.name
    }
    pub fn get_args(&self) -> &Vec<Expression> {
        &self.args
    }
    pub fn get_return_type(&self) -> &Type {
        &self.returns
    }
    pub fn get_body(&self) -> &Statement {
        &self.body
    }
    pub fn is_visible(&self) -> &bool {
        &self.is_visible
    }
}
impl Display for FunctionDeclarationStatementNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "FunctionDeclaration({}, [", self.name)?;
        for (i, val) in self.args.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", val)?;
        }
        write!(f, "], {}, {}, visible: {})", self.returns, self.body, self.is_visible)
    }
}
impl CanBeVisible for FunctionDeclarationStatementNode {
    fn is_visible(&self) -> bool {
        self.is_visible
    }
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
impl Display for VariableCreationStatementNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "VariableCreation({}, {}, {})", self.name, self.r#type, self.value)
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
impl Display for ReturnStatementNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Return({})", self.value)
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
impl Display for PlugStatementNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Plug({})", self.lib)
    }
}
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
}

impl Display for IfStatementNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if let Some(e) = &self.r#else {
            write!(f, "If({}, {}, else {})", self.cond, self.then, e)
        } else {
            write!(f, "If({}, {})", self.cond, self.then)
        }
    }
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
    pub fn get_body(&self) -> &Vec<ASTNode> {
        &self.body
    }
}
impl Display for BlockStatementNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "BlockStatement[\n")?;
        for (i, val) in self.body.iter().enumerate() {
            if i > 0 {
                write!(f, "\n")?;
            }
            write!(f, "{}", val)?;
        }
        write!(f, "\n]")
    }
}




impl IntNode {
    pub fn new(value: i32) -> IntNode {
        Self {
            value: OmniaInt::new(value)
        }
    }
}
impl ByteNode {
    pub fn new(value: i8) -> Self {
        Self {
            value: OmniaByte::new(value)
        }
    }
}
impl LongNode {
    pub fn new(value: i64) -> Self {
        Self {
            value: OmniaLong::new(value)
        }
    }
}
impl DecimalNode {
    pub fn new(value: f64) -> Self {
        Self {
            value: OmniaDecimal::new(value)
        }
    }
}
impl OmniNode {
    pub fn new(value: f128) -> Self {
        Self {
            value: OmniaOmni::new(value)
        }
    }
}

impl UByteNode {
    pub fn new(value: u8) -> Self {
        Self {
            value: OmniaUByte::new(value)
        }
    }
}
impl UIntNode {
    pub fn new(value: u32) -> Self {
        Self {
            value: OmniaUInt::new(value)
        }
    }
}
impl ULongNode {
    pub fn new(value: u64) -> Self {
        Self {
            value: OmniaULong::new(value)
        }
    }
}
impl CharNode {
    pub fn new(value: char) -> Self {
        Self {
            value: OmniaChar::new(value)
        }
    }
}
impl CharArrNode {
    pub fn new(value: String) -> Self {
        Self {
            value: OmniaChararr::get_from(value)
        }
    }
}
impl <T: FromPrimitive + Clone> SpanNode<T> {
    pub fn new(value: Vec<T>) -> Self {
        Self {
            value: OmniaSpan::new(value)
        }
    }
}
