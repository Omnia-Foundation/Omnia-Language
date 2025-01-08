#![feature(f16)]
use std::alloc::{alloc, Layout};
use std::any::{type_name, Any, TypeId};
use std::fmt::Debug;
use std::hash::{DefaultHasher, Hash, Hasher};
use std::sync::Arc;
use downcast_rs::impl_downcast;
use num_traits::{AsPrimitive, FromPrimitive};
use crate::core::omnia_types::Type::{ANYNUM, BOOL, BYTE, CHAR, DECIMAL, INT, LONG, OTHER, STRING, STRLIKE, UBYTE, UINT, ULONG};


#[derive(Clone, Copy, PartialEq, Debug)]
pub enum Type {
    DECIMAL,
    CHAR,
    STRING,
    BOOL,

    BYTE,
    UBYTE,
    INT,
    UINT,
    LONG,
    ULONG,

    ANYNUM,
    STRLIKE,
    OTHER,

}

#[derive(PartialEq, Copy, Clone)]
pub struct Pair<T, S> {
    left: T,
    right: S
}
impl Pair<Type, Type> {
    pub fn new(left: Type, right: Type) -> Pair<Type, Type> {
        Self {
            left,
            right
        }
    }
    pub fn get_right(&self) -> &Type {
        &self.right
    }
    pub fn get_left(&self) -> &Type {
        &self.left
    }
}



pub trait OmniaValue: downcast_rs::Downcast {
    fn get_as_int32(&self) -> i32;
    fn get_as_string(&self) -> String;
    fn get_as_float32(&self) -> f32;
    fn get_type(&self) -> Pair<Type, Type>;

}
impl_downcast!(OmniaValue);


//SECTION::Floats start
#[derive(Clone)]
pub struct OmniaDecimal {
    value: f64
}

impl OmniaDecimal {
    fn new(value: f64) -> OmniaDecimal {
        Self {
            value
        }
    }
    pub fn get_value_as<T: From<f64>>(&self) -> T {
        // println!("{:?}", type_name::<T>());
        T::from(self.value)
    }

    pub(crate) fn get_from<T>(value: T) -> OmniaDecimal where T : TryInto<f64> + PartialOrd, <T as TryInto<f64>>::Error : Debug{
        if value < f64::MIN.into() {
            return OmniaDecimal::new(f64::MIN)
        }
        if value > f64::MAX.into() {
            return OmniaDecimal::new(f64::MAX)
        }
        OmniaDecimal::new(value.try_into().unwrap())
    }
}

impl OmniaValue for OmniaDecimal {

    fn get_as_int32(&self) -> i32 {
        self.value as i32
    }
    fn get_as_string(&self) -> String {
        self.value.to_string()
    }
    fn get_as_float32(&self) -> f32 {
        self.value as f32
    }
    fn get_type(&self) -> Pair<Type, Type> {
        Pair::new(ANYNUM, DECIMAL)

    }

}

//SECTION::Floats end

//SECTION::Char start

#[derive(Clone)]
pub struct OmniaChar {
    value: char
}
impl OmniaChar {
    fn new(value: char) -> OmniaChar {
        Self {
            value
        }
    }
    pub fn get_value_as<T: From<char>>(&self) -> T {
        // println!("{:?}", type_name::<T>());
        T::from(self.value)
    }

    pub(crate) fn get_from<T>(value: T) -> OmniaChar where T : TryInto<char> + PartialOrd, <T as TryInto<char>>::Error: Debug {
        if value > char::MAX.into() {
            return OmniaChar::new(char::MAX)
        }
        if value < char::from(0).into() {
            return OmniaChar::new(char::from(0));
        }
        OmniaChar::new(value.try_into().unwrap())
    }
}
impl OmniaValue for OmniaChar {

    fn get_as_int32(&self) -> i32 {
        self.value as i32
    }
    fn get_as_string(&self) -> String {
        self.value.to_string()
    }
    fn get_as_float32(&self) -> f32 {
        (self.value as u8) as f32
    }
    fn get_type(&self) -> Pair<Type, Type> {
        Pair::new(STRLIKE, CHAR)

    }

}
//SECTION::Char end

//SECTION::String start

#[derive(Clone)]
pub struct OmniaString {
    value: String
}
impl OmniaString {
    fn new(value: String) -> OmniaString {
        Self {
            value
        }
    }
    pub fn get_value_as<T: From<String>>(&self) -> T {
        // println!("{:?}", type_name::<T>());
        T::from(self.value.clone())
    }

    pub(crate) fn get_from<T>(value: T) -> OmniaString where T : TryInto<String> + PartialOrd, <T as TryInto<String>>::Error: Debug {
        OmniaString::new(value.try_into().expect("Given value is not a string"))
    }
}
impl OmniaValue for OmniaString {

    fn get_as_int32(&self) -> i32 {
        let mut hasher = DefaultHasher::new();
        self.value.hash(&mut hasher);
        hasher.finish() as i32
    }
    fn get_as_string(&self) -> String {
        self.value.clone()
    }
    fn get_as_float32(&self) -> f32 {
        let mut hasher = DefaultHasher::new();
        self.value.hash(&mut hasher);
        hasher.finish() as f32
    }
    fn get_type(&self) -> Pair<Type, Type> {
        Pair::new(STRLIKE, STRING)

    }

}
//SECTION::String end

//SECTION::Bool start

#[derive(Clone)]
pub struct OmniaBool {
    value: bool
}
impl OmniaBool {
    fn new(value: bool) -> OmniaBool {
        Self {
            value
        }
    }
    pub fn get_value_as<T: From<bool>>(&self) -> T {
        // println!("{:?}", type_name::<T>());
        T::from(self.value)
    }

    pub(crate) fn get_from<T>(value: T) -> OmniaBool where T : TryInto<bool> + PartialOrd, <T as TryInto<bool>>::Error: Debug, String: From<T>, i128: From<T> {
        match type_name::<T>() {
            "bool" => {
                return OmniaBool::new(value.try_into().unwrap())
            }
            "String" | "&str" => {
                let temp = String::from(value);
                if temp.clone().to_lowercase() == "true" {
                    return OmniaBool::new(true)
                } else if temp.clone().to_lowercase() == "false" {
                    return OmniaBool::new(false)
                } else {
                    panic!("Value is not [true] or [false]")
                }
            }
            "i8" | "i16" | "i32" | "i64" | "i128" | "u8" | "u16" | "u32" | "u64" | "u128" | "f32" | "f64" => {
                let temp = i128::from(value);
                if temp <= 0 {
                    OmniaBool::new(false)
                } else {
                    OmniaBool::new(true)
                }
            }
            _ => {
                panic!("Unexpected value!")
            }

        }

    }
}
impl OmniaValue for OmniaBool {

    fn get_as_int32(&self) -> i32 {
        self.value as i32
    }
    fn get_as_string(&self) -> String {
        self.value.to_string()
    }
    fn get_as_float32(&self) -> f32 {
        (self.value as u8) as f32
    }
    fn get_type(&self) -> Pair<Type, Type> {
        Pair::new(OTHER, BOOL)

    }

}
//SECTION::Bool end







//SECTION::Byte start

#[derive(Clone)]
pub struct OmniaByte {
    value: i8
}
impl OmniaByte {
    fn new(value: i8) -> OmniaByte {
        Self {
            value
        }
    }
    pub fn get_value_as<T: From<i8>>(&self) -> T {
        T::from(self.value)
    }
    pub(crate) fn get_from<T>(value: T) -> OmniaByte where T : TryInto<i8> + PartialOrd, <T as TryInto<i8>>::Error: Debug {
        if value < i8::MIN.into() {
            return OmniaByte::new(i8::MIN)
        }
        if value > i8::MAX.into() {
            return OmniaByte::new(i8::MAX)
        }
        OmniaByte::new(value.try_into().unwrap())
    }
}
impl OmniaValue for OmniaByte {
    fn get_as_int32(&self) -> i32 {
        self.value as i32
    }
    fn get_as_string(&self) -> String {
        self.value.to_string()
    }
    fn get_as_float32(&self) -> f32 {
        self.value as f32
    }
    fn get_type(&self) -> Pair<Type, Type> {
        Pair::new(ANYNUM, BYTE)
    }
}
#[derive(Clone)]
pub struct OmniaUByte {
    value: u8
}
impl OmniaUByte {
    fn new(value: u8) -> OmniaUByte {
        Self {
            value
        }
    }
    pub fn get_value_as<T: From<u8>>(&self) -> T {
        T::from(self.value)
    }
    pub(crate) fn get_from<T>(value: T) -> OmniaUByte where T : TryInto<u8> + PartialOrd, <T as TryInto<u8>>::Error: Debug {
        if value < 0u8.into() {
            return OmniaUByte::new(0)
        }
        if value > u8::MAX.into() {
            return OmniaUByte::new(u8::MAX)
        }
        OmniaUByte::new(value.try_into().unwrap())
    }
}

impl OmniaValue for OmniaUByte {
    fn get_as_int32(&self) -> i32 {
        self.value as i32
    }
    fn get_as_string(&self) -> String {
        self.value.to_string()
    }
    fn get_as_float32(&self) -> f32 {
        self.value as f32
    }
    fn get_type(&self) -> Pair<Type, Type> {
        Pair::new(ANYNUM, UBYTE)
    }
}
//SECTION::Byte end

//SECTION::Int start

#[derive(Clone)]
pub struct OmniaInt {
    value: i32
}
impl OmniaInt {
    fn new(value: i32) -> OmniaInt {
        Self {
            value
        }
    }
    pub fn get_value_as<T: From<i32>>(&self) -> T {
        T::from(self.value)
    }
    pub(crate) fn get_from<T>(value: T) -> OmniaInt where T : TryInto<i32> + PartialOrd, <T as TryInto<i32>>::Error: Debug {
        if value < i32::MIN.into() {
            return OmniaInt::new(i32::MIN)
        }
        if value > i32::MAX.into() {
            return OmniaInt::new(i32::MAX)
        }
        OmniaInt::new(value.try_into().unwrap())
    }
}
impl OmniaValue for OmniaInt {
    fn get_as_int32(&self) -> i32 {
        self.value
    }
    fn get_as_string(&self) -> String {
        self.value.to_string()
    }
    fn get_as_float32(&self) -> f32 {
        self.value as f32
    }
    fn get_type(&self) -> Pair<Type, Type> {
        Pair::new(ANYNUM, INT)
    }
}
#[derive(Clone)]
pub struct OmniaUInt {
    value: u32
}
impl OmniaUInt {
    fn new(value: u32) -> OmniaUInt {
        Self {
            value
        }
    }
    pub fn get_value_as<T: From<u32>>(&self) -> T {
        T::from(self.value)
    }
    pub(crate) fn get_from<T>(value: T) -> OmniaUInt where T : TryInto<u32> + PartialOrd, <T as TryInto<u32>>::Error: Debug {
        if value < 0u32.into() {
            return OmniaUInt::new(0)
        }
        if value > u32::MAX.into() {
            return OmniaUInt::new(u32::MAX)
        }
        OmniaUInt::new(value.try_into().unwrap())
    }
}
impl OmniaValue for OmniaUInt {
    fn get_as_int32(&self) -> i32 {
        self.value as i32
    }
    fn get_as_string(&self) -> String {
        self.value.to_string()
    }
    fn get_as_float32(&self) -> f32 {
        self.value as f32
    }
    fn get_type(&self) -> Pair<Type, Type> {
        Pair::new(ANYNUM, UINT)
    }
}
//SECTION::Int end

//SECTION::Long start

#[derive(Clone)]
pub struct OmniaLong {
    value: i64
}
#[derive(Clone)]
pub struct OmniaULong {
    value: u64
}
impl OmniaLong {
    fn new(value: i64) -> OmniaLong {
        Self {
            value
        }
    }
    pub fn get_value_as<T: From<i64>>(&self) -> T {
        T::from(self.value)
    }
    pub(crate) fn get_from<T>(value: T) -> OmniaLong where T : TryInto<i64> + PartialOrd, <T as TryInto<i64>>::Error: Debug {
        if value < i64::MIN.into() {
            return OmniaLong::new(i64::MIN)
        }
        if value > i64::MAX.into() {
            return OmniaLong::new(i64::MAX)
        }
        OmniaLong::new(value.try_into().unwrap())
    }
}
impl OmniaULong {
    fn new(value: u64) -> OmniaULong {
        Self {
            value
        }
    }
    pub fn get_value_as<T: From<u64>>(&self) -> T {
        T::from(self.value)
    }
    pub(crate) fn get_from<T>(value: T) -> OmniaULong where T : TryInto<u64> + PartialOrd, <T as TryInto<u64>>::Error: Debug {
        if value < 0u64.into() {
            return OmniaULong::new(0)
        }
        if value > u64::MAX.into() {
            return OmniaULong::new(u64::MAX)
        }
        OmniaULong::new(value.try_into().unwrap())
    }
}
impl OmniaValue for OmniaLong {
    fn get_as_int32(&self) -> i32 {
        self.value as i32
    }
    fn get_as_string(&self) -> String {
        self.value.to_string()
    }
    fn get_as_float32(&self) -> f32 {
        self.value as f32
    }
    fn get_type(&self) -> Pair<Type, Type> {
        Pair::new(ANYNUM, LONG)
    }
}
impl OmniaValue for OmniaULong {
    fn get_as_int32(&self) -> i32 {
        self.value as i32
    }
    fn get_as_string(&self) -> String {
        self.value.to_string()
    }
    fn get_as_float32(&self) -> f32 {
        self.value as f32
    }
    fn get_type(&self) -> Pair<Type, Type> {
        Pair::new(ANYNUM, ULONG)
    }
}
//SECTION::Long end


pub struct OmniaSpan<T: FromPrimitive + Clone> {
    value: Vec<T>
}
impl <T: Clone> OmniaSpan<T> {
    fn new(value: Vec<T>) -> OmniaSpan<T> {
        Self {
            value
        }
    }
    pub fn get_value_as<T: From<Vec<T>>>(&self) -> Vec<T> {
        T::from(self.value.clone())
    }
    pub(crate) fn get_from<F>(value: Vec<F>) -> OmniaSpan<T> where F : TryInto<Vec<T>> + PartialOrd, <F as TryInto<Vec<T>>>::Error: Debug {
        OmniaSpan::new(value)
    }
}