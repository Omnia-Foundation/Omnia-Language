use std::collections::HashMap;
use crate::core::omnia_types::omnia_types::{OmniaValue, Type};
use std::sync::LazyLock;
use std::vec;

pub struct Scope {
    variables: HashMap<String, (Box<dyn OmniaValue>, Type)>
    //TODO! Functions
}
impl Scope {
    pub fn new() -> Scope {
        Self {
            variables: HashMap::new()
        }
    }
    pub fn set_var(&mut self, name: String, value: (Box<dyn OmniaValue>, Type)) {
        self.variables.insert(name, value);
    }
    pub fn get_var(&self, name: String) -> Option<&(Box<dyn OmniaValue>, Type)> {
        self.variables.get(&name)
    }
}
pub struct Runtime {
    scopes: Vec<Scope>
}
impl Runtime {
    pub fn new() -> Runtime {
        let mut runtime = Self { scopes: Vec::new() };
        runtime.scopes.push(Scope::new());
        runtime
    }
    pub fn current_scope(&mut self) -> &mut Scope {
        self.scopes.last_mut().expect("No scope found")
    }
    pub fn push_scope(&mut self) {
        self.scopes.push(Scope::new())
    }
    pub fn pop_scope(&mut self) {
        self.scopes.pop().expect("Cannot pop the global scope!");
    }
    pub fn find_in_upper_scopes(&mut self, name: String) -> Option<&(Box<dyn OmniaValue>, Type)> {
        let mut ret_scopes: Vec<Scope> = Vec::new();
        for i in 0..self.scopes.len()-1 {
            let scope = self.scopes.pop().unwrap();
            if let Some(variable) = scope.get_var(name.clone()) {
                ret_scopes.push(scope);
                self.scopes.append(&mut ret_scopes);
                return Some(variable)
            }
            ret_scopes.push(scope);

        };
        self.scopes.append(&mut ret_scopes);
        None
    }
}
