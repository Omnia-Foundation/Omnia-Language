

pub mod omni {
    use num_bigfloat::BigFloat;

    #[derive(Clone)]
    pub struct f128 {
        _val: BigFloat
    }
    impl f128 {


        pub fn new(value: BigFloat) -> Self {
            Self {
                _val: value
            }
        }
        pub fn get(&self) -> BigFloat {
            self._val.clone()
        }
    }

}

use std::str::FromStr;
use num_bigfloat::BigFloat;
use crate::core::utils::numeric_utils::omni::f128;

impl From<f128> for String {
    fn from(value: f128) -> Self {
        format!("{}", value.get())
    }
}
impl FromStr for f128 {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Self::new(BigFloat::parse(s).unwrap()))
    }
}
