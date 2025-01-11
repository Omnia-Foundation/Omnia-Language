use std::fmt::Debug;
use std::fs::{read_to_string, File};
use std::io::Read;
use std::process::exit;
use crate::core::omnia_types::{OmniaByte, OmniaValue, Type};
use crate::core::omnia_types::Type::{ANYNUM};
use crate::lexer::Lexer;
use crate::parser::Parser;

mod lexer;
mod parser;
mod core;

fn main() -> std::io::Result<()> {
    let mut input = read_to_string("src/test.oa")?;

    let mut lexer = Lexer::new(input);
    let tokens = lexer.tokenize();
    tokens.clone().iter().for_each(|x| {
        println!("{}", x)
    });
    let mut parser = Parser::new(tokens);
    println!("\n\n\n\n--------------------------------\n{}", parser.parse());
    Ok(())
}
fn cast<T, U>(value: U) -> T where T: From<U>, U: Into<T> {
    T::from(value)
}
// tt(&OmniaByte::get_from(9), &OmniaByte::get_from(2)).get_as_int32(), OmniaByte::get_from(8).get_value_as::<i128>()
// fn tt(a: &dyn OmniaValue, b: &dyn OmniaValue) -> Box<dyn OmniaValue> {
//     let binding = a.get_type();
//     let a_type = binding.get_right();
//     match a_type {
//         Type::INT8 => {
//             Box::new(OmniaByte::get_from(a.downcast_ref::<OmniaByte>().unwrap().get_value_as::<i8>() + b.downcast_ref::<OmniaByte>().unwrap().get_value_as::<i8>()))
//         }
//         Type::INT16 => {
//             Box::new(OmniaInt16::get_from(a.downcast_ref::<OmniaInt16>().unwrap().get_value_as::<i16>() + b.downcast_ref::<OmniaInt16>().unwrap().get_value_as::<i16>()))
//         }
//         _ => {
//             panic!("")
//         }
//     }
// }
