use std::collections::HashMap;
use crate::core::omnia_types::omnia_types::OmniaValue;

pub struct RuntimeVariables {
    pub set: HashMap<String, Box<dyn OmniaValue>>
}