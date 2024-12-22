//! # Omnia Abstract Syntax Tree
//! ***
//! In this file you can find definitions and implementations of **AST** nodes.
//!
//! Main trait - [Node]. He doesn't do anything
//!
//! Two general traits - [Expression] and [Statement].
//! A statement can be executed, it is a final command, that is, for example,
//! ```rust
//! if
//! else
//! while
//! ```
//! etc. are a Statement.
//! Expression is an "expression" that can be calculated, the end result is [OmniaValue]. Example - `(2 + 1)`, `(551 * var+(42-1))` etc.
//! Unlike Expression, a "command line" can begin with a Statement
//!
//! Also in this file you can find the internal enums used to define the operation
//!
//!
//!
use std::cmp::PartialEq;
use std::env::var;
use std::ops::{Add, Deref, Sub};
use num_traits::{Float};
use num_traits::float::FloatCore;
use crate::core::omnia_types::omnia_types::{OmniaByte, OmniaChar, OmniaDecimal, OmniaInt, OmniaLong, OmniaString, OmniaUByte, OmniaUInt, OmniaULong, OmniaValue};
use crate::core::omnia_types::omnia_types::Type;
use crate::core::runtime::{Scope};

trait Node {

}

trait Expression : Node {
    fn calc(&mut self) -> Box<dyn OmniaValue>;
}
trait Statement : Node {
    fn execute(&mut self);
}
#[derive(PartialEq, Debug)]
pub enum Operator{
    PLUS,
    DIVIDE,
    MULTIPLY,
    MINUS,
    REM,

    INC,
    DEC
}
#[derive(PartialEq, Debug)]
pub enum AssignmentOperator {
    ASSIGN,
    PLUSASSIGN,
    SUBASSIGN,
    MULASSIGN,
    DIVASSIGN,
    REMASSIGN
}
pub struct BinaryExpression {
    left: Box<dyn Expression>,
    right: Box<dyn Expression>,
    operator: Operator
}

impl Node for BinaryExpression {}



impl BinaryExpression {
    fn new(mut left: Box<dyn Expression>, mut right: Box<dyn Expression>, operator: Operator) -> BinaryExpression {
        Self {
            left,
            right,
            operator
        }
    }
    //SECTION:: Ints start

    //SECTION:: Int preparing start

    fn prepare_calc_l_int(&mut self, l: &OmniaInt) -> Box<dyn OmniaValue> {
        let mut r = self.right.calc();
        let binding = r.get_type();
        let r_type = binding.get_right();
        match r_type {
            Type::BYTE => {
                self.prepare_calc_int(l, &OmniaInt::get_from(r.downcast_ref::<OmniaByte>().unwrap().get_as_int32()))
            }
            Type::INT => {
                self.prepare_calc_int(l, r.downcast_ref::<OmniaInt>().unwrap())
            }
            Type::LONG => {
                self.prepare_calc_int(l, &OmniaInt::get_from(r.downcast_ref::<OmniaLong>().unwrap().get_value_as::<i64>()))
            }
            Type::DECIMAL => {
                self.prepare_calc_int(l, &OmniaInt::get_from(r.downcast_ref::<OmniaDecimal>().unwrap().get_value_as::<f64>() as i32))
            }
            Type::STRING => {
                self.append_int_to_string(l, r.downcast_ref::<OmniaString>().unwrap())
            }
            _ => {
                panic!("Unexpected or currently unsupported type {:?}", r_type)
            }
        }
    }
    fn prepare_calc_int(&self, l: &OmniaInt, r: &OmniaInt) -> Box<dyn OmniaValue> {
        match self.operator {
            Operator::PLUS => {
                Box::new(self.add_int(l, r))
            }
            Operator::MINUS => {
                Box::new(self.sub_int(l, r))
            }
            Operator::MULTIPLY => {
                Box::new(self.mul_int(l, r))
            }
            Operator::DIVIDE => {
                Box::new(self.div_int(l, r))
            }
            Operator::REM => {
                Box::new(self.rem_int(l, r))
            }
            _ => {
                panic!("Unexpected operator")
            }
        }
    }
    //SECTION:: Int preparing end

    //SECTION:: Int arithmetics start

    fn add_int(&self, l: &OmniaInt, r: &OmniaInt) -> OmniaInt {
        if let Some(calculated) = l.get_value_as::<i32>().checked_add(r.get_value_as::<i32>()) {
            OmniaInt::get_from(calculated)
        }else {
            panic!("Got value less or more than int bounds while adding")
        }
    }
    fn sub_int(&self, l: &OmniaInt, r: &OmniaInt) -> OmniaInt {
        if let Some(calculated) = l.get_value_as::<i32>().checked_sub(r.get_value_as::<i32>()) {
            OmniaInt::get_from(calculated)
        }else {
            panic!("Got value less or more than int bounds while subtracting")
        }
    }
    fn mul_int(&self, l: & OmniaInt, r: & OmniaInt) -> OmniaInt {
        if let Some(calculated) = l.get_value_as::<i32>().checked_mul(r.get_value_as::<i32>()) {
            OmniaInt::get_from(calculated)
        }else {
            panic!("Got value less or more than int bounds while multiplying")
        }
    }
    fn div_int(&self, l: & OmniaInt, r: & OmniaInt) -> OmniaInt {
        if let Some(calculated) = l.get_value_as::<i32>().checked_div(r.get_value_as::<i32>()) {
            OmniaInt::get_from(calculated)
        }else {
            panic!("Got value less or more than int bounds while dividing (or attempted to divide by zero)")
        }
    }
    fn rem_int(&self, l: & OmniaInt, r: & OmniaInt) -> OmniaInt {
        if let Some(calculated) = l.get_value_as::<i32>().checked_rem(r.get_value_as::<i32>()) {
            OmniaInt::get_from(calculated)
        }else {
            panic!("Got value less or more than int bounds while remaindering")
        }
    }
    //SECTION:: Int arithmetics end
    fn append_int_to_string(&self, l: & OmniaInt, r: &OmniaString) -> Box<OmniaString> {
        if self.operator != Operator::PLUS {
            panic!("Only can append(+) value to string")
        }
        let mut val = r.get_as_string();
        val.push_str(&l.get_as_string());
        Box::new(OmniaString::get_from(val))
    }
    //SECTION:: Ints end

    //SECTION:: Bytes start

    //SECTION:: Byte preparing start

    fn prepare_calc_l_byte(&mut self, l: &OmniaByte) -> Box<dyn OmniaValue> {
        let mut r = self.right.calc();
        let binding = r.get_type();
        let r_type = binding.get_right();
        match r_type {
            Type::BYTE => {
                self.prepare_calc_byte(l, r.downcast_ref::<OmniaByte>().unwrap())
            }
            Type::INT => {
                self.prepare_calc_byte(l, &OmniaByte::get_from::<i8>(r.downcast_ref::<OmniaInt>().unwrap().get_value_as::<i32>().try_into().unwrap()))
            }
            Type::LONG => {
                self.prepare_calc_byte(l, &OmniaByte::get_from::<i8>(r.downcast_ref::<OmniaLong>().unwrap().get_value_as::<i64>().try_into().unwrap()))
            }
            Type::DECIMAL => {
                self.prepare_calc_byte(l, &OmniaByte::get_from(r.downcast_ref::<OmniaDecimal>().unwrap().get_value_as::<f64>() as i8))
            }
            Type::STRING => {
                self.append_byte_to_string(l, r.downcast_ref::<OmniaString>().unwrap())
            }

            _ => {
                panic!("Unexpected or currently unsupported type {:?}", r_type)
            }
        }
    }
    fn prepare_calc_byte(&self, l: &OmniaByte, r: &OmniaByte) -> Box<OmniaByte> {
        match self.operator {
            Operator::PLUS => {
                Box::new(self.add_byte(l, r))
            }
            Operator::MINUS => {
                Box::new(self.sub_byte(l, r))
            }
            Operator::MULTIPLY => {
                Box::new(self.mul_byte(l, r))
            }
            Operator::DIVIDE => {
                Box::new(self.div_byte(l, r))
            }
            Operator::REM => {
                Box::new(self.rem_byte(l, r))
            }
            _ => {
                panic!("Unexpected operator")
            }
        }
    }
    //SECTION:: Byte preparing end

    //SECTION:: Byte arithmetics start

    fn add_byte(&self, l: &OmniaByte, r: &OmniaByte) -> OmniaByte {
        if let Some(calculated) = l.get_value_as::<i8>().checked_add(r.get_value_as::<i8>()) {
            OmniaByte::get_from(calculated)
        }else {
            panic!("Got value less or more than byte bounds while adding")
        }
    }
    fn sub_byte(&self, l: &OmniaByte, r: &OmniaByte) -> OmniaByte {
        if let Some(calculated) = l.get_value_as::<i8>().checked_sub(r.get_value_as::<i8>()) {
            OmniaByte::get_from(calculated)
        }else {
            panic!("Got value less or more than byte bounds while subtracting")
        }
    }
    fn mul_byte(&self, l: & OmniaByte, r: & OmniaByte) -> OmniaByte {
        if let Some(calculated) = l.get_value_as::<i8>().checked_mul(r.get_value_as::<i8>()) {
            OmniaByte::get_from(calculated)
        }else {
            panic!("Got value less or more than byte bounds while multiplying")
        }
    }
    fn div_byte(&self, l: & OmniaByte, r: & OmniaByte) -> OmniaByte {
        if let Some(calculated) = l.get_value_as::<i8>().checked_div(r.get_value_as::<i8>()) {
            OmniaByte::get_from(calculated)
        }else {
            panic!("Got value less or more than byte bounds while dividing (or attempted to divide by zero)")
        }
    }
    fn rem_byte(&self, l: & OmniaByte, r: & OmniaByte) -> OmniaByte {
        if let Some(calculated) = l.get_value_as::<i8>().checked_rem(r.get_value_as::<i8>()) {
            OmniaByte::get_from(calculated)
        }else {
            panic!("Got value less or more than byte bounds while remaindering")
        }
    }
    //SECTION:: Byte arithmetics end
    fn append_byte_to_string(&self, l: &OmniaByte, r: &OmniaString) -> Box<OmniaString> {
        if self.operator != Operator::PLUS {
            panic!("Only can append(+) value to string")
        }
        let mut val = r.get_as_string();
        val.push_str(&l.get_as_string());
        Box::new(OmniaString::get_from(val))
    }
    //SECTION:: Bytes end

    //SECTION:: Longs start

    fn prepare_calc_l_long(&mut self, l: &OmniaLong) -> Box<dyn OmniaValue> {
        let mut r = self.right.calc();
        let binding = r.get_type();
        let r_type = binding.get_right();
        match r_type {
            Type::BYTE => {
                self.prepare_calc_long(l, &OmniaLong::get_from(r.downcast_ref::<OmniaByte>().unwrap().get_value_as::<i8>() as i64))
            }
            Type::INT => {
                self.prepare_calc_long(l, &OmniaLong::get_from(r.downcast_ref::<OmniaInt>().unwrap().get_value_as::<i32>() as i64))
            }
            Type::DECIMAL => {
                self.prepare_calc_long(l, &OmniaLong::get_from(r.downcast_ref::<OmniaDecimal>().unwrap().get_value_as::<f64>() as i64))
            }
            Type::LONG => {
                self.prepare_calc_long(l, r.downcast_ref::<OmniaLong>().unwrap())
            }
            Type::STRING => {
                self.append_long_to_string(l, r.downcast_ref::<OmniaString>().unwrap())
            }
            _ => {
                panic!("Unexpected type {:?}", r_type)
            }
        }
    }
    fn prepare_calc_long(&self, l: &OmniaLong, r: &OmniaLong) -> Box<OmniaLong> {
        match self.operator {
            Operator::PLUS => {
                Box::new(self.add_long(l, r))
            }
            Operator::MINUS => {
                Box::new(self.sub_long(l, r))
            }
            Operator::MULTIPLY => {
                Box::new(self.mul_long(l, r))
            }
            Operator::DIVIDE => {
                Box::new(self.div_long(l, r))
            }
            Operator::REM => {
                Box::new(self.rem_long(l, r))
            }
            _ => {
                panic!("Unexpected operator")
            }
        }
    }
    fn add_long(&self, l: &OmniaLong, r: &OmniaLong) -> OmniaLong {
        if let Some(calculated) = l.get_value_as::<i64>().checked_add(r.get_value_as::<i64>()) {
            OmniaLong::get_from(calculated)
        } else {
            panic!("Got value less or more than long bounds while adding")
        }
    }
    fn sub_long(&self, l: &OmniaLong, r: &OmniaLong) -> OmniaLong {
        if let Some(calculated) = l.get_value_as::<i64>().checked_sub(r.get_value_as::<i64>()) {
            OmniaLong::get_from(calculated)
        } else {
            panic!("Got value less or more than long bounds while subtracting")
        }
    }
    fn mul_long(&self, l: &OmniaLong, r: &OmniaLong) -> OmniaLong {
        if let Some(calculated) = l.get_value_as::<i64>().checked_mul(r.get_value_as::<i64>()) {
            OmniaLong::get_from(calculated)
        } else {
            panic!("Got value less or more than long bounds while multiplying")
        }
    }
    fn div_long(&self, l: &OmniaLong, r: &OmniaLong) -> OmniaLong {
        if let Some(calculated) = l.get_value_as::<i64>().checked_div(r.get_value_as::<i64>()) {
            OmniaLong::get_from(calculated)
        } else {
            panic!("Got value less or more than long bounds while dividing(Or attempted to divide by zero)")
        }
    }
    fn rem_long(&self, l: &OmniaLong, r: &OmniaLong) -> OmniaLong {
        if let Some(calculated) = l.get_value_as::<i64>().checked_rem(r.get_value_as::<i64>()) {
            OmniaLong::get_from(calculated)
        } else {
            panic!("Got value less or more than long bounds while remaindering")
        }
    }
    fn append_long_to_string(&self, l: &OmniaLong, r: &OmniaString) -> Box<OmniaString> {
        if self.operator != Operator::PLUS {
            panic!("Can only append(+) value to string")
        }
        let mut val = r.get_as_string();
        val.push_str(&l.get_as_string());
        Box::new(OmniaString::get_from(val))
    }
    //SECTION:: Longs end



    //SECTION:: UInts start

    //SECTION:: UInt preparing start

    fn prepare_calc_l_uint(&mut self, l: &OmniaUInt) -> Box<dyn OmniaValue> {
        let mut r = self.right.calc();
        let binding = r.get_type();
        let r_type = binding.get_right();
        match r_type {
            Type::UBYTE => {
                self.prepare_calc_uint(l, &OmniaUInt::get_from(r.downcast_ref::<OmniaUByte>().unwrap().get_value_as::<u32>()))
            }
            Type::UINT => {
                self.prepare_calc_uint(l, r.downcast_ref::<OmniaUInt>().unwrap())
            }
            Type::ULONG => {
                self.prepare_calc_uint(l, &OmniaUInt::get_from(r.downcast_ref::<OmniaULong>().unwrap().get_value_as::<u64>()))
            }
            Type::STRING => {
                self.append_uint_to_string(l, r.downcast_ref::<OmniaString>().unwrap())
            }
            Type::DECIMAL => {
                self.prepare_calc_uint(l, &OmniaUInt::get_from(r.downcast_ref::<OmniaDecimal>().unwrap().get_value_as::<f64>() as u32))
            }
            _ => {
                panic!("Unexpected or currently unsupported type {:?}", r_type)
            }
        }
    }
    fn prepare_calc_uint(&self, l: &OmniaUInt, r: &OmniaUInt) -> Box<dyn OmniaValue> {
        match self.operator {
            Operator::PLUS => {
                Box::new(self.add_uint(l, r))
            }
            Operator::MINUS => {
                Box::new(self.sub_uint(l, r))
            }
            Operator::MULTIPLY => {
                Box::new(self.mul_uint(l, r))
            }
            Operator::DIVIDE => {
                Box::new(self.div_uint(l, r))
            }
            Operator::REM => {
                Box::new(self.rem_uint(l, r))
            }
            _ => {
                panic!("Unexpected operator")
            }
        }
    }
    //SECTION:: UInt preparing end

    //SECTION:: UInt arithmetics start

    fn add_uint(&self, l: &OmniaUInt, r: &OmniaUInt) -> OmniaUInt {
        if let Some(calculated) = l.get_value_as::<u32>().checked_add(r.get_value_as::<u32>()) {
            OmniaUInt::get_from(calculated)
        }else {
            panic!("Got value less or more than int bounds while adding")
        }
    }
    fn sub_uint(&self, l: &OmniaUInt, r: &OmniaUInt) -> OmniaUInt {
        if let Some(calculated) = l.get_value_as::<u32>().checked_sub(r.get_value_as::<u32>()) {
            OmniaUInt::get_from(calculated)
        }else {
            panic!("Got value less or more than int bounds while subtracting")
        }
    }
    fn mul_uint(&self, l: & OmniaUInt, r: & OmniaUInt) -> OmniaUInt {
        if let Some(calculated) = l.get_value_as::<u32>().checked_mul(r.get_value_as::<u32>()) {
            OmniaUInt::get_from(calculated)
        }else {
            panic!("Got value less or more than int bounds while multiplying")
        }
    }
    fn div_uint(&self, l: & OmniaUInt, r: & OmniaUInt) -> OmniaUInt {
        if let Some(calculated) = l.get_value_as::<u32>().checked_div(r.get_value_as::<u32>()) {
            OmniaUInt::get_from(calculated)
        }else {
            panic!("Got value less or more than int bounds while dividing (or attempted to divide by zero)")
        }
    }
    fn rem_uint(&self, l: & OmniaUInt, r: & OmniaUInt) -> OmniaUInt {
        if let Some(calculated) = l.get_value_as::<u32>().checked_rem(r.get_value_as::<u32>()) {
            OmniaUInt::get_from(calculated)
        }else {
            panic!("Got value less or more than int bounds while remaindering")
        }
    }
    //SECTION:: UInt arithmetics end
    fn append_uint_to_string(&self, l: & OmniaUInt, r: &OmniaString) -> Box<OmniaString> {
        if self.operator != Operator::PLUS {
            panic!("Only can append(+) value to string")
        }
        let mut val = r.get_as_string();
        val.push_str(&l.get_as_string());
        Box::new(OmniaString::get_from(val))
    }
    //SECTION:: UInts end

    //SECTION:: UBytes start

    //SECTION:: UByte preparing start

    fn prepare_calc_l_ubyte(&mut self, l: &OmniaUByte) -> Box<dyn OmniaValue> {
        let mut r = self.right.calc();
        let binding = r.get_type();
        let r_type = binding.get_right();
        match r_type {
            Type::UBYTE => {
                self.prepare_calc_ubyte(l, r.downcast_ref::<OmniaUByte>().unwrap())
            }
            Type::UINT => {
                self.prepare_calc_ubyte(l, &OmniaUByte::get_from(r.downcast_ref::<OmniaUInt>().unwrap().get_as_int32()))
            }
            Type::ULONG => {
                self.prepare_calc_ubyte(l, &OmniaUByte::get_from(r.downcast_ref::<OmniaULong>().unwrap().get_value_as::<u64>()))
            }
            Type::STRING => {
                self.append_ubyte_to_string(l, r.downcast_ref::<OmniaString>().unwrap())
            }
            Type::DECIMAL => {
                self.prepare_calc_ubyte(l, &OmniaUByte::get_from(r.downcast_ref::<OmniaDecimal>().unwrap().get_value_as::<f64>() as u8))
            }
            _ => {
                panic!("Unexpected or currently unsupported type {:?}", r_type)
            }
        }
    }
    fn prepare_calc_ubyte(&self, l: &OmniaUByte, r: &OmniaUByte) -> Box<OmniaUByte> {
        match self.operator {
            Operator::PLUS => {
                Box::new(self.add_ubyte(l, r))
            }
            Operator::MINUS => {
                Box::new(self.sub_ubyte(l, r))
            }
            Operator::MULTIPLY => {
                Box::new(self.mul_ubyte(l, r))
            }
            Operator::DIVIDE => {
                Box::new(self.div_ubyte(l, r))
            }
            Operator::REM => {
                Box::new(self.rem_ubyte(l, r))
            }
            _ => {
                panic!("Unexpected operator")
            }
        }
    }
    //SECTION:: UByte preparing end

    //SECTION:: UByte arithmetics start

    fn add_ubyte(&self, l: &OmniaUByte, r: &OmniaUByte) -> OmniaUByte {
        if let Some(calculated) = l.get_value_as::<u8>().checked_add(r.get_value_as::<u8>()) {
            OmniaUByte::get_from(calculated)
        }else {
            panic!("Got value less or more than byte bounds while adding")
        }
    }
    fn sub_ubyte(&self, l: &OmniaUByte, r: &OmniaUByte) -> OmniaUByte {
        if let Some(calculated) = l.get_value_as::<u8>().checked_sub(r.get_value_as::<u8>()) {
            OmniaUByte::get_from(calculated)
        }else {
            panic!("Got value less or more than byte bounds while subtracting")
        }
    }
    fn mul_ubyte(&self, l: & OmniaUByte, r: & OmniaUByte) -> OmniaUByte {
        if let Some(calculated) = l.get_value_as::<u8>().checked_mul(r.get_value_as::<u8>()) {
            OmniaUByte::get_from(calculated)
        }else {
            panic!("Got value less or more than byte bounds while multiplying")
        }
    }
    fn div_ubyte(&self, l: & OmniaUByte, r: & OmniaUByte) -> OmniaUByte {
        if let Some(calculated) = l.get_value_as::<u8>().checked_div(r.get_value_as::<u8>()) {
            OmniaUByte::get_from(calculated)
        }else {
            panic!("Got value less or more than byte bounds while dividing (or attempted to divide by zero)")
        }
    }
    fn rem_ubyte(&self, l: & OmniaUByte, r: & OmniaUByte) -> OmniaUByte {
        if let Some(calculated) = l.get_value_as::<u8>().checked_rem(r.get_value_as::<u8>()) {
            OmniaUByte::get_from(calculated)
        }else {
            panic!("Got value less or more than byte bounds while remaindering")
        }
    }
    //SECTION:: UByte arithmetics end
    fn append_ubyte_to_string(&self, l: &OmniaUByte, r: &OmniaString) -> Box<OmniaString> {
        if self.operator != Operator::PLUS {
            panic!("Only can append(+) value to string")
        }
        let mut val = r.get_as_string();
        val.push_str(&l.get_as_string());
        Box::new(OmniaString::get_from(val))
    }
    //SECTION:: UBytes end

    //SECTION:: ULongs start

    fn prepare_calc_l_ulong(&mut self, l: &OmniaULong) -> Box<dyn OmniaValue> {
        let mut r = self.right.calc();
        let binding = r.get_type();
        let r_type = binding.get_right();
        match r_type {
            Type::UBYTE => {
                self.prepare_calc_ulong(l, &OmniaULong::get_from(r.downcast_ref::<OmniaUByte>().unwrap().get_value_as::<u8>() as u64))
            }
            Type::UINT => {
                self.prepare_calc_ulong(l, &OmniaULong::get_from(r.downcast_ref::<OmniaUInt>().unwrap().get_value_as::<u32>() as u64))
            }
            Type::ULONG => {
                self.prepare_calc_ulong(l, r.downcast_ref::<OmniaULong>().unwrap())
            }
            Type::STRING => {
                self.append_ulong_to_string(l, r.downcast_ref::<OmniaString>().unwrap())
            }
            _ => {
                panic!("Unexpected type {:?}", r_type)
            }
        }
    }
    fn prepare_calc_ulong(&self, l: &OmniaULong, r: &OmniaULong) -> Box<OmniaULong> {
        match self.operator {
            Operator::PLUS => {
                Box::new(self.add_ulong(l, r))
            }
            Operator::MINUS => {
                Box::new(self.sub_ulong(l, r))
            }
            Operator::MULTIPLY => {
                Box::new(self.mul_ulong(l, r))
            }
            Operator::DIVIDE => {
                Box::new(self.div_ulong(l, r))
            }
            Operator::REM => {
                Box::new(self.rem_ulong(l, r))
            }
            _ => {
                panic!("Unexpected operator")
            }
        }
    }
    fn add_ulong(&self, l: &OmniaULong, r: &OmniaULong) -> OmniaULong {
        if let Some(calculated) = l.get_value_as::<u64>().checked_add(r.get_value_as::<u64>()) {
            OmniaULong::get_from(calculated)
        } else {
            panic!("Got value less or more than ulong bounds while adding")
        }
    }
    fn sub_ulong(&self, l: &OmniaULong, r: &OmniaULong) -> OmniaULong {
        if let Some(calculated) = l.get_value_as::<u64>().checked_sub(r.get_value_as::<u64>()) {
            OmniaULong::get_from(calculated)
        } else {
            panic!("Got value less or more than long bounds while subtracting")
        }
    }
    fn mul_ulong(&self, l: &OmniaULong, r: &OmniaULong) -> OmniaULong {
        if let Some(calculated) = l.get_value_as::<u64>().checked_mul(r.get_value_as::<u64>()) {
            OmniaULong::get_from(calculated)
        } else {
            panic!("Got value less or more than long bounds while multiplying")
        }
    }
    fn div_ulong(&self, l: &OmniaULong, r: &OmniaULong) -> OmniaULong {
        if let Some(calculated) = l.get_value_as::<u64>().checked_div(r.get_value_as::<u64>()) {
            OmniaULong::get_from(calculated)
        } else {
            panic!("Got value less or more than long bounds while dividing(Or attempted to divide by zero)")
        }
    }
    fn rem_ulong(&self, l: &OmniaULong, r: &OmniaULong) -> OmniaULong {
        if let Some(calculated) = l.get_value_as::<u64>().checked_rem(r.get_value_as::<u64>()) {
            OmniaULong::get_from(calculated)
        } else {
            panic!("Got value less or more than long bounds while remaindering")
        }
    }
    fn append_ulong_to_string(&self, l: &OmniaULong, r: &OmniaString) -> Box<OmniaString> {
        if self.operator != Operator::PLUS {
            panic!("Can only append(+) value to string!")
        }
        let mut val = r.get_as_string();
        val.push_str(&l.get_as_string());
        Box::new(OmniaString::get_from(val))
    }
    //SECTION:: ULongs end


    //SECTION:: Strings start

    fn prepare_calc_l_string(&mut self, l: &OmniaString) -> Box<OmniaString> {
        let mut r = self.right.calc();
        let binding = r.get_type();
        let r_type = binding.get_right();
        match r_type {
            Type::BYTE => {
                self.append_byte_to_string(r.downcast_ref::<OmniaByte>().unwrap(), l)
            }
            Type::UBYTE => {
                self.append_ubyte_to_string(r.downcast_ref::<OmniaUByte>().unwrap(), l)
            }
            Type::INT => {
                self.append_int_to_string(r.downcast_ref::<OmniaInt>().unwrap(), l)
            }
            Type::UINT => {
                self.append_uint_to_string(r.downcast_ref::<OmniaUInt>().unwrap(), l)
            }
            Type::LONG => {
                self.append_long_to_string(r.downcast_ref::<OmniaLong>().unwrap(), l)
            }
            Type::ULONG => {
                self.append_ulong_to_string(r.downcast_ref::<OmniaULong>().unwrap(), l)
            }
            Type::STRING => {
                let temp = r.downcast_ref::<OmniaString>().unwrap();
                let mut val = l.get_as_string();
                val.push_str(&temp.get_as_string());
                Box::new(OmniaString::get_from(val))
            }
            Type::DECIMAL => {
                self.append_decimal_to_string(r.downcast_ref::<OmniaDecimal>().unwrap(), l)
            }
            Type::CHAR => {
                let temp = r.downcast_ref::<OmniaChar>().unwrap();
                let mut val = l.get_as_string();
                val.push(temp.get_value_as::<char>());
                Box::new(OmniaString::get_from(val))
            }
            _ => {
                panic!("Unexpected or unsupported type {:?}", r_type)
            }
        }
    }
    //SECTION:: Strings end

    //SECTION:: Decimals start

    fn prepare_calc_l_decimal(&mut self, l: &OmniaDecimal) -> Box<dyn OmniaValue> {
        let mut r = self.right.calc();
        let binding = r.get_type();
        let r_type = binding.get_right();
        match r_type {
            Type::BYTE => {
                self.prepare_calc_decimal(l, &OmniaDecimal::get_from(r.downcast_ref::<OmniaByte>().unwrap().get_as_float32() as f64))
            }
            Type::UBYTE => {
                self.prepare_calc_decimal(l, &OmniaDecimal::get_from(r.downcast_ref::<OmniaUByte>().unwrap().get_as_float32() as f64))
            }
            Type::INT => {
                self.prepare_calc_decimal(l, &OmniaDecimal::get_from(r.downcast_ref::<OmniaInt>().unwrap().get_as_float32() as f64))
            }
            Type::UINT => {
                self.prepare_calc_decimal(l, &OmniaDecimal::get_from(r.downcast_ref::<OmniaUInt>().unwrap().get_value_as::<u32>() as f64))
            }
            Type::LONG => {
                self.prepare_calc_decimal(l, &OmniaDecimal::get_from(r.downcast_ref::<OmniaLong>().unwrap().get_value_as::<i64>() as f64))
            }
            Type::DECIMAL => {
                self.prepare_calc_decimal(l, r.downcast_ref::<OmniaDecimal>().unwrap())
            }
            Type::STRING => {
                self.append_decimal_to_string(l, r.downcast_ref::<OmniaString>().unwrap())
            }


            _ => {
                panic!("Unexpected or unsupported type {:?}", r_type)
            }
        }
    }
    fn prepare_calc_decimal(&self, l: &OmniaDecimal, r: &OmniaDecimal) -> Box<OmniaDecimal> {
        match self.operator {
            Operator::PLUS => {
                Box::new(self.add_decimal(l, r))
            }
            Operator::MINUS => {
                Box::new(self.sub_decimal(l, r))
            }
            Operator::MULTIPLY => {
                Box::new(self.mul_decimal(l, r))
            }
            Operator::DIVIDE => {
                Box::new(self.div_decimal(l, r))
            }
            Operator::REM => {
                Box::new(self.rem_decimal(l, r))
            }


            _ => {
                panic!("Unexpected or unsupported operator {:?}", self.operator)
            }
        }
    }
    fn add_decimal(&self, l: &OmniaDecimal, r: &OmniaDecimal) -> OmniaDecimal {
        if let Some(calculated) = l.get_value_as::<f64>().checked_add(r.get_value_as::<f64>()) {
            OmniaDecimal::get_from(calculated)
        } else {
            panic!("Got value which is out of bounds of decimal while adding")
        }
    }
    fn sub_decimal(&self, l: &OmniaDecimal, r: &OmniaDecimal) -> OmniaDecimal {
        if let Some(calculated) = l.get_value_as::<f64>().checked_sub(r.get_value_as::<f64>()) {
            OmniaDecimal::get_from(calculated)
        } else {
            panic!("Got value which is out of bounds of decimal while subtracting")
        }
    }
    fn mul_decimal(&self, l: &OmniaDecimal, r: &OmniaDecimal) -> OmniaDecimal {
        if let Some(calculated) = l.get_value_as::<f64>().checked_mul(r.get_value_as::<f64>()) {
            OmniaDecimal::get_from(calculated)
        } else {
            panic!("Got value which is out of bounds of decimal while multiplying")
        }
    }
    fn div_decimal(&self, l: &OmniaDecimal, r: &OmniaDecimal) -> OmniaDecimal {
        if let Some(calculated) = l.get_value_as::<f64>().checked_div(r.get_value_as::<f64>()) {
            OmniaDecimal::get_from(calculated)
        } else {
            panic!("Got value which is out of bounds of decimal while dividing(or attempted to divide by zero)")
        }
    }
    fn rem_decimal(&self, l: &OmniaDecimal, r: &OmniaDecimal) -> OmniaDecimal {
        if let Some(calculated) = l.get_value_as::<f64>().checked_rem(r.get_value_as::<f64>()) {
            OmniaDecimal::get_from(calculated)
        } else {
            panic!("Got value which is out of bounds of decimal while remaindering")
        }
    }
    fn append_decimal_to_string(&self, l: &OmniaDecimal, r: &OmniaString) -> Box<OmniaString> {
        let mut val = r.get_as_string();
        val.push_str(&l.get_as_string());
        Box::new(OmniaString::get_from(val))
    }
    //SECTION:: Decimals end
}
pub trait CheckerF64 {
    fn checked_add(&self, b: f64) -> Option<f64>;
    fn checked_sub(&self, b: f64) -> Option<f64>;
    fn checked_mul(&self, b: f64) -> Option<f64>;
    fn checked_div(&self, b: f64) -> Option<f64>;
    fn checked_rem(&self, b: f64) -> Option<f64>;
}
pub trait CheckedIncDec {
    fn checked_inc(&self) -> Option<impl Add>;
    fn checked_dec(&self) -> Option<impl Sub>;
}

impl CheckedIncDec for i8 {
    fn checked_inc(&self) -> Option<i8> {
        if let Some(c) = self.checked_add(1i8) {
            Some(c)
        } else {
            None
        }
    }
    fn checked_dec(&self) -> Option<i8> {
        if let Some(c) = self.checked_sub(1i8) {
            Some(c)
        } else {
            None
        }
    }
}
impl CheckedIncDec for u8 {
    fn checked_inc(&self) -> Option<u8> {
        if let Some(c) = self.checked_add(1u8) {
            Some(c)
        } else {
            None
        }
    }
    fn checked_dec(&self) -> Option<u8> {
        if let Some(c) = self.checked_sub(1u8) {
            Some(c)
        } else {
            None
        }
    }
}
impl CheckedIncDec for i32 {
    fn checked_inc(&self) -> Option<i32> {
        if let Some(c) = self.checked_add(1i32) {
            Some(c)
        } else {
            None
        }
    }
    fn checked_dec(&self) -> Option<i32> {
        if let Some(c) = self.checked_sub(1i32) {
            Some(c)
        } else {
            None
        }
    }
}
impl CheckedIncDec for u32 {
    fn checked_inc(&self) -> Option<u32> {
        if let Some(c) = self.checked_add(1u32) {
            Some(c)
        } else {
            None
        }
    }
    fn checked_dec(&self) -> Option<u32> {
        if let Some(c) = self.checked_sub(1u32) {
            Some(c)
        } else {
            None
        }
    }
}
impl CheckedIncDec for i64 {
    fn checked_inc(&self) -> Option<i64> {
        if let Some(c) = self.checked_add(1i64) {
            Some(c)
        } else {
            None
        }
    }
    fn checked_dec(&self) -> Option<i64> {
        if let Some(c) = self.checked_sub(1i64) {
            Some(c)
        } else {
            None
        }
    }
}
impl CheckedIncDec for u64 {
    fn checked_inc(&self) -> Option<u64> {
        if let Some(c) = self.checked_add(1u64) {
            Some(c)
        } else {
            None
        }
    }
    fn checked_dec(&self) -> Option<u64> {
        if let Some(c) = self.checked_sub(1u64) {
            Some(c)
        } else {
            None
        }
    }
}
impl CheckedIncDec for f64 {
    fn checked_inc(&self) -> Option<f64> {
        if let Some(c) = self.checked_add(1f64) {
            Some(c)
        } else {
            None
        }
    }
    fn checked_dec(&self) -> Option<f64> {
        if let Some(c) = self.checked_sub(1f64) {
            Some(c)
        } else {
            None
        }
    }
}


impl CheckerF64 for f64 {
    fn checked_add(&self, b: f64) -> Option<f64> {
        let result = self + b;
        if result.is_finite() {
            Some(result)
        } else {
            None
        }
    }
    fn checked_sub(&self, b: f64) -> Option<f64> {
        let result = self - b;
        if result.is_finite() {
            Some(result)
        } else {
            None
        }
    }
    fn checked_mul(&self, b: f64) -> Option<f64> {
        let result = self * b;
        if result.is_finite() {
            Some(result)
        } else {
            None
        }
    }
    fn checked_div(&self, b: f64) -> Option<f64> {
        if b == 0f64 {
            return None
        }
        let result = self / b;
        if result.is_finite() {
            Some(result)
        } else {
            None
        }
    }
    fn checked_rem(&self, b: f64) -> Option<f64> {
        let result = self % b;
        if result.is_finite() {
            Some(result)
        } else {
            None
        }
    }
}

impl Expression for BinaryExpression {
    fn calc(&mut self) -> Box<dyn OmniaValue> {
        let mut left = self.left.calc();
        let binding = left.get_type();
        let value_type = binding.get_right();
        match value_type {
            Type::BYTE => {
                if let Some(dc) = left.downcast_ref::<OmniaByte>() {
                    self.prepare_calc_l_byte(dc)
                } else {
                    panic!("Unexpected error")
                }
            }

            Type::INT => {
                if let Some(dc) = left.downcast_ref::<OmniaInt>() {
                    self.prepare_calc_l_int(dc)
                } else {
                    panic!("Unexpected error")
                }
            }
            Type::LONG => {
                if let Some(dc) = left.downcast_ref::<OmniaLong>() {
                    self.prepare_calc_l_long(dc)
                } else {
                    panic!("Unexpected error")
                }
            }

            Type::UBYTE => {
                if let Some(dc) = left.downcast_ref::<OmniaUByte>() {
                    self.prepare_calc_l_ubyte(dc)
                } else {
                    panic!("Unexpected error")
                }
            }
            Type::UINT => {
                if let Some(dc) = left.downcast_ref::<OmniaUInt>() {
                    self.prepare_calc_l_uint(dc)
                } else {
                    panic!("Unexpected error")
                }
            }
            Type::ULONG => {
                if let Some(dc) = left.downcast_ref::<OmniaULong>() {
                    self.prepare_calc_l_ulong(dc)
                } else {
                    panic!("Unexpected error")
                }
            }
            Type::STRING => {
                if let Some(dc) = left.downcast_ref::<OmniaString>() {
                   self.prepare_calc_l_string(dc)
                } else {
                    panic!("Unexpected error")
                }
            }
            Type::DECIMAL => {
                if let Some(dc) = left.downcast_ref::<OmniaDecimal>() {
                    self.prepare_calc_l_decimal(dc)
                } else {
                    panic!("Unexpected error")
                }
            }

            _ => {
                panic!("Unexpected type {:?}", left.get_type().get_right())
            }
        }

    }
}
pub struct UnaryExpression {
    left: Box<dyn Expression>,
    operation: Operator
}
impl UnaryExpression {
    fn new(mut left: Box<dyn Expression>, operation: Operator) -> UnaryExpression {
        Self {
            left,
            operation
        }
    }
    fn prepare_calc_byte(&self, val: &OmniaByte) -> Box<dyn OmniaValue> {
        match self.operation {
            Operator::INC => {
                Box::new(self.inc_byte(val))
            }
            Operator::DEC => {
                Box::new(self.dec_byte(val))
            }
            _ => {
                panic!("Unexpected or unsupported operator {:?}", self.operation)
            }
        }
    }
    fn inc_byte(&self, val: &OmniaByte) -> OmniaByte {
        if let Some(calculated) = val.get_value_as::<i8>().checked_inc() {
            OmniaByte::get_from(calculated)
        } else {
            panic!("Got value which is more or less than byte bounds while incrementing!")
        }
    }
    fn dec_byte(&self, val: &OmniaByte) -> OmniaByte {
        if let Some(calculated) = val.get_value_as::<i8>().checked_dec() {
            OmniaByte::get_from(calculated)
        } else {
            panic!("Got value which is more or less than byte bounds while decrementing!")
        }
    }
    fn prepare_calc_ubyte(&self, val: &OmniaUByte) -> Box<OmniaUByte> {
        match self.operation {
            Operator::INC => {
                Box::new(self.inc_ubyte(val))
            }
            Operator::DEC => {
                Box::new(self.dec_ubyte(val))
            }
            _ => {
                panic!("Unexpected or unsupported operator {:?}", self.operation)
            }
        }
    }
    fn inc_ubyte(&self, val: &OmniaUByte) -> OmniaUByte {
        if let Some(calculated) = val.get_value_as::<u8>().checked_inc() {
            OmniaUByte::get_from(calculated)
        } else {
            panic!("Got value which is more or less than ubyte bounds while incrementing!")
        }
    }
    fn dec_ubyte(&self, val: &OmniaUByte) -> OmniaUByte {
        if let Some(calculated) = val.get_value_as::<u8>().checked_dec() {
            OmniaUByte::get_from(calculated)
        } else {
            panic!("Got value which is more or less than ubyte bounds while decrementing!")
        }
    }

    fn prepare_calc_int(&self, val: &OmniaInt) -> Box<dyn OmniaValue> {
        match self.operation {
            Operator::INC => {
                Box::new(self.inc_int(val))
            }
            Operator::DEC => {
                Box::new(self.dec_int(val))
            }
            _ => {
                panic!("Unexpected or unsupported operator {:?}", self.operation)
            }
        }
    }
    fn inc_int(&self, val: &OmniaInt) -> OmniaInt {
        if let Some(calculated) = val.get_value_as::<i32>().checked_inc() {
            OmniaInt::get_from(calculated)
        } else {
            panic!("Got value which is more or less than int bounds while incrementing!")
        }
    }
    fn dec_int(&self, val: &OmniaInt) -> OmniaInt {
        if let Some(calculated) = val.get_value_as::<i32>().checked_dec() {
            OmniaInt::get_from(calculated)
        } else {
            panic!("Got value which is more or less than int bounds while decrementing!")
        }
    }
    fn prepare_calc_uint(&self, val: &OmniaUInt) -> Box<OmniaUInt> {
        match self.operation {
            Operator::INC => {
                Box::new(self.inc_uint(val))
            }
            Operator::DEC => {
                Box::new(self.dec_uint(val))
            }
            _ => {
                panic!("Unexpected or unsupported operator {:?}", self.operation)
            }
        }
    }
    fn inc_uint(&self, val: &OmniaUInt) -> OmniaUInt {
        if let Some(calculated) = val.get_value_as::<u32>().checked_inc() {
            OmniaUInt::get_from(calculated)
        } else {
            panic!("Got value which is more or less than uint bounds while incrementing!")
        }
    }
    fn dec_uint(&self, val: &OmniaUInt) -> OmniaUInt {
        if let Some(calculated) = val.get_value_as::<u32>().checked_dec() {
            OmniaUInt::get_from(calculated)
        } else {
            panic!("Got value which is more or less than uint bounds while decrementing!")
        }
    }
    fn prepare_calc_long(&self, val: &OmniaLong) -> Box<dyn OmniaValue> {
        match self.operation {
            Operator::INC => {
                Box::new(self.inc_long(val))
            }
            Operator::DEC => {
                Box::new(self.dec_long(val))
            }
            _ => {
                panic!("Unexpected or unsupported operator {:?}", self.operation)
            }
        }
    }
    fn inc_long(&self, val: &OmniaLong) -> OmniaLong {
        if let Some(calculated) = val.get_value_as::<i64>().checked_inc() {
            OmniaLong::get_from(calculated)
        } else {
            panic!("Got value which is more or less than long bounds while incrementing!")
        }
    }
    fn dec_long(&self, val: &OmniaLong) -> OmniaLong {
        if let Some(calculated) = val.get_value_as::<i64>().checked_dec() {
            OmniaLong::get_from(calculated)
        } else {
            panic!("Got value which is more or less than long bounds while decrementing!")
        }
    }
    fn prepare_calc_ulong(&self, val: &OmniaULong) -> Box<OmniaULong> {
        match self.operation {
            Operator::INC => {
                Box::new(self.inc_ulong(val))
            }
            Operator::DEC => {
                Box::new(self.dec_ulong(val))
            }
            _ => {
                panic!("Unexpected or unsupported operator {:?}", self.operation)
            }
        }
    }
    fn inc_ulong(&self, val: &OmniaULong) -> OmniaULong {
        if let Some(calculated) = val.get_value_as::<u64>().checked_inc() {
            OmniaULong::get_from(calculated)
        } else {
            panic!("Got value which is more or less than ulong bounds while incrementing!")
        }
    }
    fn dec_ulong(&self, val: &OmniaULong) -> OmniaULong {
        if let Some(calculated) = val.get_value_as::<u64>().checked_dec() {
            OmniaULong::get_from(calculated)
        } else {
            panic!("Got value which is more or less than ulong bounds while decrementing!")
        }
    }
    fn prepare_calc_decimal(&self, val: &OmniaDecimal) -> Box<OmniaDecimal> {
        match self.operation {
            Operator::INC => {
                Box::new(self.inc_decimal(val))
            }
            Operator::DEC => {
                Box::new(self.dec_decimal(val))
            }
            _ => {
                panic!("Unexpected or unsupported operator {:?}", self.operation)
            }
        }
    }
    fn inc_decimal(&self, val: &OmniaDecimal) -> OmniaDecimal {
        if let Some(calculated) = val.get_value_as::<f64>().checked_inc() {
            OmniaDecimal::get_from(calculated)
        } else {
            panic!("Got value which is more or less than decimal bounds while incrementing!")
        }
    }
    fn dec_decimal(&self, val: &OmniaDecimal) -> OmniaDecimal {
        if let Some(calculated) = val.get_value_as::<f64>().checked_dec() {
            OmniaDecimal::get_from(calculated)
        } else {
            panic!("Got value which is more or less than decimal bounds while decrementing!")
        }
    }

}
impl Node for UnaryExpression {}
impl Expression for UnaryExpression {
    fn calc(&mut self) -> Box<dyn OmniaValue>{
        let mut value = self.left.calc();
        let binding = value.get_type();
        let v_type = binding.get_right();
        match v_type {
            Type::BYTE => {
                self.prepare_calc_byte(value.downcast_ref::<OmniaByte>().unwrap())
            }
            Type::UBYTE => {
                self.prepare_calc_ubyte(value.downcast_ref::<OmniaUByte>().unwrap())
            }
            Type::INT => {
                self.prepare_calc_int(value.downcast_ref::<OmniaInt>().unwrap())
            }
            Type::UINT => {
                self.prepare_calc_uint(value.downcast_ref::<OmniaUInt>().unwrap())
            }
            Type::LONG => {
                self.prepare_calc_long(value.downcast_ref::<OmniaLong>().unwrap())
            }
            Type::ULONG => {
                self.prepare_calc_ulong(value.downcast_ref::<OmniaULong>().unwrap())
            }
            Type::DECIMAL => {
                self.prepare_calc_decimal(value.downcast_ref::<OmniaDecimal>().unwrap())
            }

            _ => {
                panic!("Unexpected or unsupported type {:?}", v_type)
            }
        }
    }
}

struct AssignmentStatement {
    name: String,
    value: Box<dyn Expression>,
    operation: AssignmentOperator,
    scope: Scope
}
impl AssignmentStatement {
    fn new(name: String, value: Box<dyn Expression>, operation: AssignmentOperator, scope: Scope) -> AssignmentStatement {
        Self {
            name,
            value,
            operation,
            scope
        }
    }
    fn plus_assign_bytes(&self, var: &OmniaByte, val: &OmniaByte) -> OmniaByte {
        if let Some(calculated) = var.get_value_as::<i8>().checked_add(val.get_value_as::<i8>()) {
            OmniaByte::get_from(calculated)
        } else {
            panic!("Got value which is more or less than byte bounds while plusassigning!")
        }
    }
    fn plus_assign_ints(&self, var: &OmniaInt, val: &OmniaInt) -> OmniaInt {
        if let Some(calculated) = var.get_value_as::<i32>().checked_add(val.get_value_as::<i32>()) {
            OmniaInt::get_from(calculated)
        } else {
            panic!("Got value which is more or less than int bounds while plusassigning!")
        }
    }
    fn plus_assign_longs(&self, var: &OmniaLong, val: &OmniaLong) -> OmniaLong {
        if let Some(calculated) = var.get_value_as::<i64>().checked_add(val.get_value_as::<i64>()) {
            OmniaLong::get_from(calculated)
        } else {
            panic!("Got value which is more or less than long bounds while plusassigning!")
        }
    }
    fn plus_assign_decimals(&self, var: &OmniaDecimal, val: &OmniaDecimal) -> OmniaDecimal {
        if let Some(calculated) = var.get_value_as::<f64>().checked_add(val.get_value_as::<f64>()) {
            OmniaDecimal::get_from(calculated)
        } else {
            panic!("Got value which is more or less than decimal bounds while plusassigning!")
        }
    }
    fn plus_assign_ubytes(&self, var: &OmniaUByte, val: &OmniaUByte) -> OmniaUByte {
        if let Some(calculated) = var.get_value_as::<u8>().checked_add(val.get_value_as::<u8>()) {
            OmniaUByte::get_from(calculated)
        } else {
            panic!("Got value which is more or less than ubyte bounds while plusassigning!")
        }
    }
    fn plus_assign_uints(&self, var: &OmniaUInt, val: &OmniaUInt) -> OmniaUInt {
        if let Some(calculated) = var.get_value_as::<u32>().checked_add(val.get_value_as::<u32>()) {
            OmniaUInt::get_from(calculated)
        } else {
            panic!("Got value which is more or less than uint bounds while plusassigning!")
        }
    }
    fn plus_assign_ulongs(&self, var: &OmniaULong, val: &OmniaULong) -> OmniaULong {
        if let Some(calculated) = var.get_value_as::<u64>().checked_add(val.get_value_as::<u64>()) {
            OmniaULong::get_from(calculated)
        } else {
            panic!("Got value which is more or less than long bounds while plusassigning!")
        }
    }

    fn sub_assign_bytes(&self, var: &OmniaByte, val: &OmniaByte) -> OmniaByte {
        if let Some(calculated) = var.get_value_as::<i8>().checked_sub(val.get_value_as::<i8>()) {
            OmniaByte::get_from(calculated)
        } else {
            panic!("Got value which is more or less than byte bounds while subassigning!")
        }
    }
    fn sub_assign_ints(&self, var: &OmniaInt, val: &OmniaInt) -> OmniaInt {
        if let Some(calculated) = var.get_value_as::<i32>().checked_sub(val.get_value_as::<i32>()) {
            OmniaInt::get_from(calculated)
        } else {
            panic!("Got value which is more or less than int bounds while subassigning!")
        }
    }
    fn sub_assign_longs(&self, var: &OmniaLong, val: &OmniaLong) -> OmniaLong {
        if let Some(calculated) = var.get_value_as::<i64>().checked_sub(val.get_value_as::<i64>()) {
            OmniaLong::get_from(calculated)
        } else {
            panic!("Got value which is more or less than long bounds while subassigning!")
        }
    }
    fn sub_assign_decimals(&self, var: &OmniaDecimal, val: &OmniaDecimal) -> OmniaDecimal {
        if let Some(calculated) = var.get_value_as::<f64>().checked_sub(val.get_value_as::<f64>()) {
            OmniaDecimal::get_from(calculated)
        } else {
            panic!("Got value which is more or less than decimal bounds while subassigning!")
        }
    }
    fn sub_assign_ubytes(&self, var: &OmniaUByte, val: &OmniaUByte) -> OmniaUByte {
        if let Some(calculated) = var.get_value_as::<u8>().checked_sub(val.get_value_as::<u8>()) {
            OmniaUByte::get_from(calculated)
        } else {
            panic!("Got value which is more or less than ubyte bounds while subassigning!")
        }
    }
    fn sub_assign_uints(&self, var: &OmniaUInt, val: &OmniaUInt) -> OmniaUInt {
        if let Some(calculated) = var.get_value_as::<u32>().checked_sub(val.get_value_as::<u32>()) {
            OmniaUInt::get_from(calculated)
        } else {
            panic!("Got value which is more or less than uint bounds while subassigning!")
        }
    }
    fn sub_assign_ulongs(&self, var: &OmniaULong, val: &OmniaULong) -> OmniaULong {
        if let Some(calculated) = var.get_value_as::<u64>().checked_sub(val.get_value_as::<u64>()) {
            OmniaULong::get_from(calculated)
        } else {
            panic!("Got value which is more or less than long bounds while subassigning!")
        }
    }

    fn mul_assign_bytes(&self, var: &OmniaByte, val: &OmniaByte) -> OmniaByte {
        if let Some(calculated) = var.get_value_as::<i8>().checked_mul(val.get_value_as::<i8>()) {
            OmniaByte::get_from(calculated)
        } else {
            panic!("Got value which is more or less than byte bounds while mulassigning!")
        }
    }
    fn mul_assign_ints(&self, var: &OmniaInt, val: &OmniaInt) -> OmniaInt {
        if let Some(calculated) = var.get_value_as::<i32>().checked_mul(val.get_value_as::<i32>()) {
            OmniaInt::get_from(calculated)
        } else {
            panic!("Got value which is more or less than int bounds while mulassigning!")
        }
    }
    fn mul_assign_longs(&self, var: &OmniaLong, val: &OmniaLong) -> OmniaLong {
        if let Some(calculated) = var.get_value_as::<i64>().checked_mul(val.get_value_as::<i64>()) {
            OmniaLong::get_from(calculated)
        } else {
            panic!("Got value which is more or less than long bounds while mulassigning!")
        }
    }
    fn mul_assign_decimals(&self, var: &OmniaDecimal, val: &OmniaDecimal) -> OmniaDecimal {
        if let Some(calculated) = var.get_value_as::<f64>().checked_mul(val.get_value_as::<f64>()) {
            OmniaDecimal::get_from(calculated)
        } else {
            panic!("Got value which is more or less than decimal bounds while mulassigning!")
        }
    }
    fn mul_assign_ubytes(&self, var: &OmniaUByte, val: &OmniaUByte) -> OmniaUByte {
        if let Some(calculated) = var.get_value_as::<u8>().checked_mul(val.get_value_as::<u8>()) {
            OmniaUByte::get_from(calculated)
        } else {
            panic!("Got value which is more or less than ubyte bounds while mulassigning!")
        }
    }
    fn mul_assign_uints(&self, var: &OmniaUInt, val: &OmniaUInt) -> OmniaUInt {
        if let Some(calculated) = var.get_value_as::<u32>().checked_mul(val.get_value_as::<u32>()) {
            OmniaUInt::get_from(calculated)
        } else {
            panic!("Got value which is more or less than uint bounds while mulassigning!")
        }
    }
    fn mul_assign_ulongs(&self, var: &OmniaULong, val: &OmniaULong) -> OmniaULong {
        if let Some(calculated) = var.get_value_as::<u64>().checked_mul(val.get_value_as::<u64>()) {
            OmniaULong::get_from(calculated)
        } else {
            panic!("Got value which is more or less than long bounds while mulassigning!")
        }
    }
    fn div_assign_bytes(&self, var: &OmniaByte, val: &OmniaByte) -> OmniaByte {
        if let Some(calculated) = var.get_value_as::<i8>().checked_div(val.get_value_as::<i8>()) {
            OmniaByte::get_from(calculated)
        } else {
            panic!("Got value which is more or less than byte bounds while divassigning!")
        }
    }
    fn div_assign_ints(&self, var: &OmniaInt, val: &OmniaInt) -> OmniaInt {
        if let Some(calculated) = var.get_value_as::<i32>().checked_div(val.get_value_as::<i32>()) {
            OmniaInt::get_from(calculated)
        } else {
            panic!("Got value which is more or less than int bounds while divassigning!")
        }
    }
    fn div_assign_longs(&self, var: &OmniaLong, val: &OmniaLong) -> OmniaLong {
        if let Some(calculated) = var.get_value_as::<i64>().checked_div(val.get_value_as::<i64>()) {
            OmniaLong::get_from(calculated)
        } else {
            panic!("Got value which is more or less than long bounds while divassigning!")
        }
    }
    fn div_assign_decimals(&self, var: &OmniaDecimal, val: &OmniaDecimal) -> OmniaDecimal {
        if let Some(calculated) = var.get_value_as::<f64>().checked_div(val.get_value_as::<f64>()) {
            OmniaDecimal::get_from(calculated)
        } else {
            panic!("Got value which is more or less than decimal bounds while divassigning!")
        }
    }
    fn div_assign_ubytes(&self, var: &OmniaUByte, val: &OmniaUByte) -> OmniaUByte {
        if let Some(calculated) = var.get_value_as::<u8>().checked_div(val.get_value_as::<u8>()) {
            OmniaUByte::get_from(calculated)
        } else {
            panic!("Got value which is more or less than ubyte bounds while divassigning!")
        }
    }
    fn div_assign_uints(&self, var: &OmniaUInt, val: &OmniaUInt) -> OmniaUInt {
        if let Some(calculated) = var.get_value_as::<u32>().checked_div(val.get_value_as::<u32>()) {
            OmniaUInt::get_from(calculated)
        } else {
            panic!("Got value which is more or less than uint bounds while divassigning!")
        }
    }
    fn div_assign_ulongs(&self, var: &OmniaULong, val: &OmniaULong) -> OmniaULong {
        if let Some(calculated) = var.get_value_as::<u64>().checked_div(val.get_value_as::<u64>()) {
            OmniaULong::get_from(calculated)
        } else {
            panic!("Got value which is more or less than long bounds while divassigning!")
        }
    }
    fn rem_assign_bytes(&self, var: &OmniaByte, val: &OmniaByte) -> OmniaByte {
        if let Some(calculated) = var.get_value_as::<i8>().checked_rem(val.get_value_as::<i8>()) {
            OmniaByte::get_from(calculated)
        } else {
            panic!("Got value which is more or less than byte bounds while remassigning!")
        }
    }
    fn rem_assign_ints(&self, var: &OmniaInt, val: &OmniaInt) -> OmniaInt {
        if let Some(calculated) = var.get_value_as::<i32>().checked_rem(val.get_value_as::<i32>()) {
            OmniaInt::get_from(calculated)
        } else {
            panic!("Got value which is more or less than int bounds while remassigning!")
        }
    }
    fn rem_assign_longs(&self, var: &OmniaLong, val: &OmniaLong) -> OmniaLong {
        if let Some(calculated) = var.get_value_as::<i64>().checked_rem(val.get_value_as::<i64>()) {
            OmniaLong::get_from(calculated)
        } else {
            panic!("Got value which is more or less than long bounds while remassigning!")
        }
    }
    fn rem_assign_decimals(&self, var: &OmniaDecimal, val: &OmniaDecimal) -> OmniaDecimal {
        if let Some(calculated) = var.get_value_as::<f64>().checked_rem(val.get_value_as::<f64>()) {
            OmniaDecimal::get_from(calculated)
        } else {
            panic!("Got value which is more or less than decimal bounds while remassigning!")
        }
    }
    fn rem_assign_ubytes(&self, var: &OmniaUByte, val: &OmniaUByte) -> OmniaUByte {
        if let Some(calculated) = var.get_value_as::<u8>().checked_rem(val.get_value_as::<u8>()) {
            OmniaUByte::get_from(calculated)
        } else {
            panic!("Got value which is more or less than ubyte bounds while remassigning!")
        }
    }
    fn rem_assign_uints(&self, var: &OmniaUInt, val: &OmniaUInt) -> OmniaUInt {
        if let Some(calculated) = var.get_value_as::<u32>().checked_rem(val.get_value_as::<u32>()) {
            OmniaUInt::get_from(calculated)
        } else {
            panic!("Got value which is more or less than uint bounds while remassigning!")
        }
    }
    fn rem_assign_ulongs(&self, var: &OmniaULong, val: &OmniaULong) -> OmniaULong {
        if let Some(calculated) = var.get_value_as::<u64>().checked_rem(val.get_value_as::<u64>()) {
            OmniaULong::get_from(calculated)
        } else {
            panic!("Got value which is more or less than long bounds while remassigning!")
        }
    }


    fn concat_strings(&self, var: &OmniaString, val: &OmniaString) -> OmniaString {
        let mut temp = var.get_as_string();
        temp.push_str(&val.get_as_string());
        OmniaString::get_from(temp)
    }

    fn check_types(&self, l_type: &Type, r_type: &Type) -> bool {
        l_type == r_type
    }
}

impl Node for AssignmentStatement {}

impl Statement for AssignmentStatement {
    fn execute(&mut self) {
        match self.operation {
            AssignmentOperator::ASSIGN => {
                let value = self.value.calc();
                let binding = value.get_type();
                if let Some(var) = self.scope.get_var(self.name.clone()) {
                    if !self.check_types(&var.1, &binding.get_right()) {
                        panic!("Cannot set value of type {:?} to variable with type {:?}!", binding.get_right(), var.1)
                    }
                    self.scope.set_var(self.name.clone(), (value, *binding.get_right()))
                    // panic!("Variable with name {} already exists in this context! HINT: Try to rename to {}", self.name, self.name.clone()+"1")
                }else {
                    panic!("Variable with name {} not found in this context!", self.name)
                }

            }
            AssignmentOperator::PLUSASSIGN => {
                let value = self.value.calc();
                if let Ok(variable) = self.scope.get_var(self.name.clone()) {
                    let binding = value.get_type();
                    let value_type = binding.get_right();
                    match variable.1 {
                        Type::BYTE => {
                            if !self.check_types(&Type::BYTE, value_type) {
                                panic!("Cannot add value of type {:?} to byte value!", value_type)
                            }
                            if let Err(s) = self.scope.set_var(self.name.clone(), (Box::new(self.plus_assign_bytes(&variable.0, &value.downcast_ref::<OmniaByte>().unwrap())), Type::BYTE)) {
                                panic!("Occurred error while assigning new value to variable {}", self.name)
                            }

                        }
                        Type::INT => {
                            if !self.check_types(&Type::INT, value_type) {
                                panic!("Cannot add value of type {:?} to int value!", value_type)
                            }
                            if let Err(s) = self.scope.set_var(self.name.clone(), (Box::new(self.plus_assign_ints(&variable.0, &value.downcast_ref::<OmniaInt>().unwrap())), Type::INT)) {
                                panic!("Occurred error while assigning new value to variable {}", self.name)
                            }
                        }
                        Type::LONG => {
                            if !self.check_types(&Type::LONG, value_type) {
                                panic!("Cannot add value of type {:?} to long value!", value_type)
                            }
                            if let Err(s) = self.scope.set_var(self.name.clone(), (Box::new(self.plus_assign_longs(&variable.0, &value.downcast_ref::<OmniaLong>().unwrap())), Type::LONG)) {
                                panic!("Occurred error while assigning new value to variable {}", self.name)
                            }
                        }
                        Type::DECIMAL => {
                            if !self.check_types(&Type::DECIMAL, value_type) {
                                panic!("Cannot add value of type {:?} to decimal value!", value_type)
                            }
                            if let Err(s) = self.scope.set_var(self.name.clone(), (Box::new(self.plus_assign_decimals(&variable.0, &value.downcast_ref::<OmniaDecimal>().unwrap())), Type::DECIMAL)) {
                                panic!("Occurred error while assigning new value to variable {}", self.name)
                            }
                        }
                        Type::STRING => {
                            if !self.check_types(&Type::STRING, value_type) {
                                panic!("Cannot add value of type {:?} to string value!", value_type)
                            }
                            if let Err(s) = self.scope.set_var(self.name.clone(), (Box::new(self.concat_strings(&variable.0, &value.downcast_ref::<OmniaString>().unwrap())), Type::STRING)) {
                                panic!("Occurred error while assigning new value to variable {}", self.name)
                            }
                        }
                        Type::UBYTE => {
                            if !self.check_types(&Type::UBYTE, value_type) {
                                panic!("Cannot add value of type {:?} to ubyte value!", value_type)
                            }
                            if let Err(s) = self.scope.set_var(self.name.clone(), (Box::new(self.plus_assign_ubytes(&variable.0, &value.downcast_ref::<OmniaUByte>().unwrap())), Type::UBYTE)) {
                                panic!("Occurred error while assigning new value to variable {}", self.name)
                            }
                        }
                        Type::UINT => {
                            if !self.check_types(&Type::UINT, value_type) {
                                panic!("Cannot add value of type {:?} to uint value!", value_type)
                            }
                            if let Err(s) = self.scope.set_var(self.name.clone(), (Box::new(self.plus_assign_uints(&variable.0, &value.downcast_ref::<OmniaUInt>().unwrap())), Type::UINT)) {
                                panic!("Occurred error while assigning new value to variable {}", self.name)
                            }
                        }
                        Type::ULONG => {
                            if !self.check_types(&Type::ULONG, value_type) {
                                panic!("Cannot add value of type {:?} to ulong value!", value_type)
                            }
                            if let Err(s) = self.scope.set_var(self.name.clone(), (Box::new(self.plus_assign_ulongs(&variable.0, &value.downcast_ref::<OmniaULong>().unwrap())), Type::ULONG)) {
                                panic!("Occurred error while assigning new value to variable {}", self.name)
                            }
                        }
                        _ => {
                            panic!("Unexpected or unsupported type")
                        }
                    }
                } else {
                    panic!("Variable with name {} not found in this context!", self.name)
                }
            }
            AssignmentOperator::SUBASSIGN => {
                let value = self.value.calc();
                if let Ok(variable) = self.scope.get_var(self.name.clone()) {
                    let binding = value.get_type();
                    let value_type = binding.get_right();
                    match variable.1 {
                        Type::BYTE => {
                            if !self.check_types(&Type::BYTE, value_type) {
                                panic!("Cannot add value of type {:?} to byte value!", value_type)
                            }
                            if let Err(s) = self.scope.set_var(self.name.clone(), (Box::new(self.sub_assign_bytes(&variable.0, &value.downcast_ref::<OmniaByte>().unwrap())), Type::BYTE)) {
                                panic!("Occurred error while assigning new value to variable {}", self.name)
                            }

                        }
                        Type::INT => {
                            if !self.check_types(&Type::INT, value_type) {
                                panic!("Cannot add value of type {:?} to int value!", value_type)
                            }
                            if let Err(s) = self.scope.set_var(self.name.clone(), (Box::new(self.sub_assign_ints(&variable.0, &value.downcast_ref::<OmniaInt>().unwrap())), Type::INT)) {
                                panic!("Occurred error while assigning new value to variable {}", self.name)
                            }
                        }
                        Type::LONG => {
                            if !self.check_types(&Type::LONG, value_type) {
                                panic!("Cannot add value of type {:?} to long value!", value_type)
                            }
                            if let Err(s) = self.scope.set_var(self.name.clone(), (Box::new(self.sub_assign_longs(&variable.0, &value.downcast_ref::<OmniaLong>().unwrap())), Type::LONG)) {
                                panic!("Occurred error while assigning new value to variable {}", self.name)
                            }
                        }
                        Type::DECIMAL => {
                            if !self.check_types(&Type::DECIMAL, value_type) {
                                panic!("Cannot add value of type {:?} to decimal value!", value_type)
                            }
                            if let Err(s) = self.scope.set_var(self.name.clone(), (Box::new(self.sub_assign_decimals(&variable.0, &value.downcast_ref::<OmniaDecimal>().unwrap())), Type::DECIMAL)) {
                                panic!("Occurred error while assigning new value to variable {}", self.name)
                            }
                        }
                        Type::UBYTE => {
                            if !self.check_types(&Type::UBYTE, value_type) {
                                panic!("Cannot add value of type {:?} to ubyte value!", value_type)
                            }
                            if let Err(s) = self.scope.set_var(self.name.clone(), (Box::new(self.sub_assign_ubytes(&variable.0, &value.downcast_ref::<OmniaUByte>().unwrap())), Type::UBYTE)) {
                                panic!("Occurred error while assigning new value to variable {}", self.name)
                            }
                        }
                        Type::UINT => {
                            if !self.check_types(&Type::UINT, value_type) {
                                panic!("Cannot add value of type {:?} to uint value!", value_type)
                            }
                            if let Err(s) = self.scope.set_var(self.name.clone(), (Box::new(self.sub_assign_uints(&variable.0, &value.downcast_ref::<OmniaUInt>().unwrap())), Type::UINT)) {
                                panic!("Occurred error while assigning new value to variable {}", self.name)
                            }
                        }
                        Type::ULONG => {
                            if !self.check_types(&Type::ULONG, value_type) {
                                panic!("Cannot add value of type {:?} to ulong value!", value_type)
                            }
                            if let Err(s) = self.scope.set_var(self.name.clone(), (Box::new(self.sub_assign_ulongs(&variable.0, &value.downcast_ref::<OmniaULong>().unwrap())), Type::ULONG)) {
                                panic!("Occurred error while assigning new value to variable {}", self.name)
                            }
                        }
                        _ => {
                            panic!("Unexpected or unsupported type")
                        }
                    }
                } else {
                    panic!("Variable with name {} not found in this context!", self.name)
                }
            }
            AssignmentOperator::MULASSIGN => {
                let value = self.value.calc();
                if let Ok(variable) = self.scope.get_var(self.name.clone()) {
                    let binding = value.get_type();
                    let value_type = binding.get_right();
                    match variable.1 {
                        Type::BYTE => {
                            if !self.check_types(&Type::BYTE, value_type) {
                                panic!("Cannot add value of type {:?} to byte value!", value_type)
                            }
                            if let Err(s) = self.scope.set_var(self.name.clone(), (Box::new(self.mul_assign_bytes(&variable.0, &value.downcast_ref::<OmniaByte>().unwrap())), Type::BYTE)) {
                                panic!("Occurred error while assigning new value to variable {}", self.name)
                            }

                        }
                        Type::INT => {
                            if !self.check_types(&Type::INT, value_type) {
                                panic!("Cannot add value of type {:?} to int value!", value_type)
                            }
                            if let Err(s) = self.scope.set_var(self.name.clone(), (Box::new(self.mul_assign_ints(&variable.0, &value.downcast_ref::<OmniaInt>().unwrap())), Type::INT)) {
                                panic!("Occurred error while assigning new value to variable {}", self.name)
                            }
                        }
                        Type::LONG => {
                            if !self.check_types(&Type::LONG, value_type) {
                                panic!("Cannot add value of type {:?} to long value!", value_type)
                            }
                            if let Err(s) = self.scope.set_var(self.name.clone(), (Box::new(self.mul_assign_longs(&variable.0, &value.downcast_ref::<OmniaLong>().unwrap())), Type::LONG)) {
                                panic!("Occurred error while assigning new value to variable {}", self.name)
                            }
                        }
                        Type::DECIMAL => {
                            if !self.check_types(&Type::DECIMAL, value_type) {
                                panic!("Cannot add value of type {:?} to decimal value!", value_type)
                            }
                            if let Err(s) = self.scope.set_var(self.name.clone(), (Box::new(self.mul_assign_decimals(&variable.0, &value.downcast_ref::<OmniaDecimal>().unwrap())), Type::DECIMAL)) {
                                panic!("Occurred error while assigning new value to variable {}", self.name)
                            }
                        }
                        Type::UBYTE => {
                            if !self.check_types(&Type::UBYTE, value_type) {
                                panic!("Cannot add value of type {:?} to ubyte value!", value_type)
                            }
                            if let Err(s) = self.scope.set_var(self.name.clone(), (Box::new(self.mul_assign_ubytes(&variable.0, &value.downcast_ref::<OmniaUByte>().unwrap())), Type::UBYTE)) {
                                panic!("Occurred error while assigning new value to variable {}", self.name)
                            }
                        }
                        Type::UINT => {
                            if !self.check_types(&Type::UINT, value_type) {
                                panic!("Cannot add value of type {:?} to uint value!", value_type)
                            }
                            if let Err(s) = self.scope.set_var(self.name.clone(), (Box::new(self.mul_assign_uints(&variable.0, &value.downcast_ref::<OmniaUInt>().unwrap())), Type::UINT)) {
                                panic!("Occurred error while assigning new value to variable {}", self.name)
                            }
                        }
                        Type::ULONG => {
                            if !self.check_types(&Type::ULONG, value_type) {
                                panic!("Cannot add value of type {:?} to ulong value!", value_type)
                            }
                            if let Err(s) = self.scope.set_var(self.name.clone(), (Box::new(self.mul_assign_ulongs(&variable.0, &value.downcast_ref::<OmniaULong>().unwrap())), Type::ULONG)) {
                                panic!("Occurred error while assigning new value to variable {}", self.name)
                            }
                        }
                        _ => {
                            panic!("Unexpected or unsupported type")
                        }
                    }
                } else {
                    panic!("Variable with name {} not found in this context!", self.name)
                }
            }
            AssignmentOperator::DIVASSIGN => {
                let value = self.value.calc();
                if let Ok(variable) = self.scope.get_var(self.name.clone()) {
                    let binding = value.get_type();
                    let value_type = binding.get_right();
                    match variable.1 {
                        Type::BYTE => {
                            if !self.check_types(&Type::BYTE, value_type) {
                                panic!("Cannot add value of type {:?} to byte value!", value_type)
                            }
                            if let Err(s) = self.scope.set_var(self.name.clone(), (Box::new(self.div_assign_bytes(&variable.0, &value.downcast_ref::<OmniaByte>().unwrap())), Type::BYTE)) {
                                panic!("Occurred error while assigning new value to variable {}", self.name)
                            }

                        }
                        Type::INT => {
                            if !self.check_types(&Type::INT, value_type) {
                                panic!("Cannot add value of type {:?} to int value!", value_type)
                            }
                            if let Err(s) = self.scope.set_var(self.name.clone(), (Box::new(self.div_assign_ints(&variable.0, &value.downcast_ref::<OmniaInt>().unwrap())), Type::INT)) {
                                panic!("Occurred error while assigning new value to variable {}", self.name)
                            }
                        }
                        Type::LONG => {
                            if !self.check_types(&Type::LONG, value_type) {
                                panic!("Cannot add value of type {:?} to long value!", value_type)
                            }
                            if let Err(s) = self.scope.set_var(self.name.clone(), (Box::new(self.div_assign_longs(&variable.0, &value.downcast_ref::<OmniaLong>().unwrap())), Type::LONG)) {
                                panic!("Occurred error while assigning new value to variable {}", self.name)
                            }
                        }
                        Type::DECIMAL => {
                            if !self.check_types(&Type::DECIMAL, value_type) {
                                panic!("Cannot add value of type {:?} to decimal value!", value_type)
                            }
                            if let Err(s) = self.scope.set_var(self.name.clone(), (Box::new(self.div_assign_decimals(&variable.0, &value.downcast_ref::<OmniaDecimal>().unwrap())), Type::DECIMAL)) {
                                panic!("Occurred error while assigning new value to variable {}", self.name)
                            }
                        }
                        Type::UBYTE => {
                            if !self.check_types(&Type::UBYTE, value_type) {
                                panic!("Cannot add value of type {:?} to ubyte value!", value_type)
                            }
                            if let Err(s) = self.scope.set_var(self.name.clone(), (Box::new(self.div_assign_ubytes(&variable.0, &value.downcast_ref::<OmniaUByte>().unwrap())), Type::UBYTE)) {
                                panic!("Occurred error while assigning new value to variable {}", self.name)
                            }
                        }
                        Type::UINT => {
                            if !self.check_types(&Type::UINT, value_type) {
                                panic!("Cannot add value of type {:?} to uint value!", value_type)
                            }
                            if let Err(s) = self.scope.set_var(self.name.clone(), (Box::new(self.div_assign_uints(&variable.0, &value.downcast_ref::<OmniaUInt>().unwrap())), Type::UINT)) {
                                panic!("Occurred error while assigning new value to variable {}", self.name)
                            }
                        }
                        Type::ULONG => {
                            if !self.check_types(&Type::ULONG, value_type) {
                                panic!("Cannot add value of type {:?} to ulong value!", value_type)
                            }
                            if let Err(s) = self.scope.set_var(self.name.clone(), (Box::new(self.div_assign_ulongs(&variable.0, &value.downcast_ref::<OmniaULong>().unwrap())), Type::ULONG)) {
                                panic!("Occurred error while assigning new value to variable {}", self.name)
                            }
                        }
                        _ => {
                            panic!("Unexpected or unsupported type")
                        }
                    }
                } else {
                    panic!("Variable with name {} not found in this context!", self.name)
                }
            }
            AssignmentOperator::REMASSIGN => {
                let value = self.value.calc();
                if let Ok(variable) = self.scope.get_var(self.name.clone()) {
                    let binding = value.get_type();
                    let value_type = binding.get_right();
                    match variable.1 {
                        Type::BYTE => {
                            if !self.check_types(&Type::BYTE, value_type) {
                                panic!("Cannot add value of type {:?} to byte value!", value_type)
                            }
                            if let Err(s) = self.scope.set_var(self.name.clone(), (Box::new(self.rem_assign_bytes(&variable.0, &value.downcast_ref::<OmniaByte>().unwrap())), Type::BYTE)) {
                                panic!("Occurred error while assigning new value to variable {}", self.name)
                            }

                        }
                        Type::INT => {
                            if !self.check_types(&Type::INT, value_type) {
                                panic!("Cannot add value of type {:?} to int value!", value_type)
                            }
                            if let Err(s) = self.scope.set_var(self.name.clone(), (Box::new(self.rem_assign_ints(&variable.0, &value.downcast_ref::<OmniaInt>().unwrap())), Type::INT)) {
                                panic!("Occurred error while assigning new value to variable {}", self.name)
                            }
                        }
                        Type::LONG => {
                            if !self.check_types(&Type::LONG, value_type) {
                                panic!("Cannot add value of type {:?} to long value!", value_type)
                            }
                            if let Err(s) = self.scope.set_var(self.name.clone(), (Box::new(self.rem_assign_longs(&variable.0, &value.downcast_ref::<OmniaLong>().unwrap())), Type::LONG)) {
                                panic!("Occurred error while assigning new value to variable {}", self.name)
                            }
                        }
                        Type::DECIMAL => {
                            if !self.check_types(&Type::DECIMAL, value_type) {
                                panic!("Cannot add value of type {:?} to decimal value!", value_type)
                            }
                            if let Err(s) = self.scope.set_var(self.name.clone(), (Box::new(self.rem_assign_decimals(&variable.0, &value.downcast_ref::<OmniaDecimal>().unwrap())), Type::DECIMAL)) {
                                panic!("Occurred error while assigning new value to variable {}", self.name)
                            }
                        }
                        Type::UBYTE => {
                            if !self.check_types(&Type::UBYTE, value_type) {
                                panic!("Cannot add value of type {:?} to ubyte value!", value_type)
                            }
                            if let Err(s) = self.scope.set_var(self.name.clone(), (Box::new(self.rem_assign_ubytes(&variable.0, &value.downcast_ref::<OmniaUByte>().unwrap())), Type::UBYTE)) {
                                panic!("Occurred error while assigning new value to variable {}", self.name)
                            }
                        }
                        Type::UINT => {
                            if !self.check_types(&Type::UINT, value_type) {
                                panic!("Cannot add value of type {:?} to uint value!", value_type)
                            }
                            if let Err(s) = self.scope.set_var(self.name.clone(), (Box::new(self.rem_assign_uints(&variable.0, &value.downcast_ref::<OmniaUInt>().unwrap())), Type::UINT)) {
                                panic!("Occurred error while assigning new value to variable {}", self.name)
                            }
                        }
                        Type::ULONG => {
                            if !self.check_types(&Type::ULONG, value_type) {
                                panic!("Cannot add value of type {:?} to ulong value!", value_type)
                            }
                            if let Err(s) = self.scope.set_var(self.name.clone(), (Box::new(self.rem_assign_ulongs(&variable.0, &value.downcast_ref::<OmniaULong>().unwrap())), Type::ULONG)) {
                                panic!("Occurred error while assigning new value to variable {}", self.name)
                            }
                        }
                        _ => {
                            panic!("Unexpected or unsupported type")
                        }
                    }
                } else {
                    panic!("Variable with name {} not found in this context!", self.name)
                }
            }
        }
    }
}
