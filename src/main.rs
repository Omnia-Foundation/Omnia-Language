use std::fmt::Debug;
use std::fs::File;
use std::io::Read;
use std::process::exit;
use crate::core::omnia_types::omnia_types::{OmniaByte, OmniaValue, Type};
use crate::core::omnia_types::omnia_types::Type::{ANYNUM};
use crate::lexer::lexer::Lexer;

mod lexer;
mod parser;
mod core;
fn main() {
    // let mut buf = Vec::new();
    // File::open("src/test.oa").unwrap().read_to_end(&mut buf).expect("Error yomayo");
    // let mut input = String::new();
    // buf.iter().for_each(|x| {
    //     input.push(*x as char)
    // });
    //
    // let mut lexer = Lexer::new(input);
    // lexer.tokenize().iter().for_each(|x| {
    //      println!("{}", x.clone().to_string())
    // })
    println!("{}", cast::<i16, i8>(2i8) )
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
