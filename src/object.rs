use crate::value::Value;
use std::collections::hash_map::DefaultHasher;
use std::fmt::{Display, Formatter};
use std::hash::{Hash, Hasher};

pub enum Obj {
    String(ObjString),
}

impl Clone for Obj {
    fn clone(&self) -> Self {
        match self {
            Obj::String(obj_string) => Obj::String(obj_string.clone()),
        }
    }
}

impl PartialEq for Obj {
    fn eq(&self, other: &Self) -> bool {
        use std::mem::discriminant;

        if discriminant(self) != discriminant(other) {
            false
        } else {
            match (self, other) {
                (Obj::String(lhs), Obj::String(rhs)) => lhs == rhs,
            }
        }
    }
}

impl Display for Obj {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        match self {
            Obj::String(obj_string) => write!(f, "{}", obj_string),
        }
    }
}

#[cfg_attr(feature = "rlox_debug", derive(Debug))]
pub struct ObjString {
    pub data: String,
    pub hash: u64,
}

impl Clone for ObjString {
    fn clone(&self) -> Self {
        Self {
            data: self.data.clone(),
            hash: self.hash,
        }
    }
}

impl PartialEq for ObjString {
    fn eq(&self, other: &Self) -> bool {
        self.data == other.data
    }
}

impl Eq for ObjString {}

impl Display for ObjString {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.data)
    }
}

impl Hash for ObjString {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.data.hash(state);
    }
}

impl ObjString {
    pub fn new(value: String) -> ObjString {
        let mut hasher = DefaultHasher::new();
        value.hash(&mut hasher);

        Self {
            data: value.clone(),
            hash: hasher.finish(),
        }
    }

    pub fn add(lhs: &Self, rhs: &Self) -> Self {
        let mut concatenated = String::with_capacity(lhs.data.len() + rhs.data.len());
        concatenated.push_str(&lhs.data);
        concatenated.push_str(&rhs.data);

        Self::take_string(concatenated)
    }

    pub fn copy_string(value: &str) -> Self {
        Self::new(String::from(value))
    }

    pub fn take_string(value: String) -> Self {
        Self::new(value)
    }
}

pub fn obj_as_rlox_string(obj: Obj) -> ObjString {
    match obj {
        Obj::String(obj_string) => obj_string,
    }
}

pub fn value_as_rlox_string(value: Value) -> ObjString {
    if let Value::Obj(obj) = value {
        return obj_as_rlox_string(obj);
    }

    panic!("Given Value is not an Obj")
}
