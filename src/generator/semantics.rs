use std;
use std::collections::{HashMap, VecDeque};
use crate::generator::semantics::utils::SemanticsError;
use crate::generator::semantics::utils::SemanticsError::NotYetImplemented;
use crate::parser::ast::nodes::{ASTNode, BlockStatementNode, Expression, Statement};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum DataType {
    Byte,
    UByte,
    Int,
    UInt,
    Long,
    ULong,
    Decimal,
    Omni,
    Char,
    Chararr,
    Null,
    Bool
}

#[derive(Debug, Clone)]
pub struct VariableInfo {
    pub name: String,
    pub r#type: DataType,
    pub mutable: bool
}

#[derive(Debug, Clone)]
pub struct FunctionInfo {
    pub name: String,
    pub returns: DataType,
    pub parameters: Vec<(String, DataType)>
}

#[derive(Debug, Clone)]
pub enum Symbol {
    Variable(VariableInfo),
    Function(FunctionInfo),
    Custom(String)
}
pub struct Scope {
    symbols: HashMap<String, Symbol>
}
impl Scope {
    pub fn new() -> Self {
        Self {
            symbols: HashMap::new()
        }
    }
    pub fn insert(&mut self, name: String, symbol: Symbol) -> Result<(), String> {
        if self.symbols.contains_key(&name) {
            Err(format!("Symbol '{}' already defined in this scope", name))
        } else {
            self.symbols.insert(name, symbol);
            Ok(())
        }
    }
    pub fn get(&self, name: &str) -> Option<&Symbol> {
        self.symbols.get(name)
    }
}
pub struct SymbolTable {
    scopes: VecDeque<Scope>
}
impl SymbolTable {
    pub fn new() -> Self {
        Self {
            scopes: VecDeque::new()
        }
    }
    pub fn push(&mut self) {
        self.scopes.push_front(Scope::new());
    }
    pub fn pop(&mut self) {
        self.scopes.pop_front();
    }
    pub fn insert_symbol(&mut self, name: String, symbol: Symbol) -> Result<(), String> {
        if let Some(scope) = self.scopes.front_mut() {
            scope.insert(name, symbol)
        } else {
            Err(String::from("No available scopes for inserting"))
        }
    }
    pub fn get_symbol(&self, name: &str) -> Option<&Symbol> {
        for scope in &self.scopes {
            if let Some(symbol) = scope.get(name) {
                return Some(symbol)
            }
        }
        None
    }
}

pub struct SemanticsAnalyzer {
    table: SymbolTable,
    input: BlockStatementNode,

}
impl SemanticsAnalyzer {
    pub fn new(input: BlockStatementNode) -> Self {
        Self {
            table: SymbolTable::new(),
            input
        }
    }
    pub fn start(&mut self) -> Result<(), SemanticsError> {
        self.analyze();


        Ok(())
    }
    fn analyze(&mut self) -> Result<(), SemanticsError> {
        for x in self.input.get_body() {
            return match x {
                ASTNode::Statement(st) => { self.analyze_statement(st) }
                ASTNode::Expression(ex) => { self.analyze_expression(ex) }
            }
        }
        Ok(())
    }
    fn analyze_statement(&mut self, statement: &Statement) -> Result<(), SemanticsError> {
        match statement {
            Statement::FunctionDeclaration(f) => {
                self.table.push();
                self.table.insert_symbol(f.get_name().clone(), Symbol::Function(FunctionInfo { name: f.get_name().clone(), returns: f.get_return_type(), parameters: f.get_args() }))
            }
            _ => { Err(NotYetImplemented(format!("Not yet implemented for statement {}", statement))) }
        }
    }
    fn analyze_expression(&mut self, expression: &Expression) -> Result<(), SemanticsError> {

    }
}

pub mod utils {
    pub enum SemanticsError {
        NotYetImplemented(String)
    }
}