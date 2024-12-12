use std::collections::HashMap;
use crate::core::omnia_types::omnia_types::OmniaValue;

static mut VARIABLES: HashMap<String, Box<dyn OmniaValue>> = HashMap::new();
pub struct RuntimeVariables {

}
impl RuntimeVariables {
    pub unsafe fn add_variable(name: String, val: Box<dyn OmniaValue>) -> Result<(), ()> {
        if !Self::is_exists(&name) {
            VARIABLES.insert(name, val);
            Ok(())
        } else {
            Err(())
        }
    }
    pub unsafe fn is_exists(name: &String) -> bool {
        VARIABLES.contains_key(name)
    }
    pub unsafe fn get_variable(name: &String) -> Result<&Box<dyn OmniaValue>, String> {
        if let Some(var) = VARIABLES.get(name) {
            Ok(var)
        }else {
            Err(format!("variable {} does not exist", name))
        }
    }
}