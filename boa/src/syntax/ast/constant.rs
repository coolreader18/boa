use gc_derive::{Finalize, Trace};
use std::fmt::{Display, Formatter, Result};

#[cfg(feature = "serde-ast")]
use serde::{Deserialize, Serialize};
/// A Javascript Constant.
#[cfg_attr(feature = "serde-ast", derive(Serialize, Deserialize))]
#[derive(Clone, Debug, Trace, Finalize, PartialEq)]
pub enum Const {
    /// A UTF-8 string, such as `"Hello, world"`
    String(String),
    // A 64-bit floating-point number, such as `3.1415`
    Num(f64),
    // A 32-bit integer, such as `42`
    Int(i32),
    // A boolean, which is either `true` or `false` and is used to check if criteria are met
    Bool(bool),
    // The `null` value, which represents a non-existant value
    Null,
    // The `undefined` value, which represents a field or index that doesn't exist
    Undefined,
}

impl From<&str> for Const {
    fn from(s: &str) -> Self {
        Const::String(s.into())
    }
}

impl From<&String> for Const {
    fn from(s: &String) -> Self {
        Const::String(s.clone())
    }
}

impl From<String> for Const {
    fn from(s: String) -> Self {
        Const::String(s)
    }
}

impl From<f64> for Const {
    fn from(num: f64) -> Self {
        Self::Num(num)
    }
}

impl From<i32> for Const {
    fn from(i: i32) -> Self {
        Self::Int(i)
    }
}

impl From<bool> for Const {
    fn from(b: bool) -> Self {
        Self::Bool(b)
    }
}

impl Display for Const {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match *self {
            Self::String(ref st) => write!(f, "\"{}\"", st),
            Self::Num(num) => write!(f, "{}", num),
            Self::Int(num) => write!(f, "{}", num),
            Self::Bool(v) => write!(f, "{}", v),
            Self::Null => write!(f, "null"),
            Self::Undefined => write!(f, "undefined"),
        }
    }
}
