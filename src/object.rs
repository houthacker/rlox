use crate::table::Table;
use crate::value::Value;
use crate::Chunk;
use std::collections::hash_map::DefaultHasher;
use std::fmt::{Display, Formatter};
use std::hash::{Hash, Hasher};

pub enum Obj {
    String(*const ObjString),
    Function(*mut ObjFunction),
}

impl Clone for Obj {
    fn clone(&self) -> Self {
        match self {
            Obj::String(obj_string) => Obj::String(*obj_string),
            Obj::Function(_) => panic!("Cannot clone Function."),
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
                (Obj::Function(lhs), Obj::Function(rhs)) => *lhs == *rhs,
                _ => false,
            }
        }
    }
}

impl Display for Obj {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        match self {
            Obj::String(obj_string) => {
                write!(f, "{}", unsafe { (*obj_string).as_ref().unwrap() })
            }
            Obj::Function(func) => {
                write!(f, "{}", unsafe { func.as_ref().unwrap() })
            }
        }
    }
}

impl Obj {
    pub fn as_rlox_string_ref<'a, 'b: 'a>(&'a self) -> Option<&'b ObjString> {
        match self {
            Obj::String(obj_string) => unsafe { obj_string.as_ref() },
            _ => None,
        }
    }
}

pub trait Hashed {
    fn calculated_hash(&self) -> u64;
}

#[cfg_attr(feature = "rlox_debug", derive(Debug))]
pub struct ObjFunction {
    pub arity: u32,
    pub chunk: Chunk,
    pub name: Option<*const ObjString>,
}

impl PartialEq for ObjFunction {
    fn eq(&self, other: &Self) -> bool {
        // todo check parameters
        unsafe {
            self.arity == other.arity
                && match (self.name, other.name) {
                    (Some(lhs), Some(rhs)) => *lhs == *rhs,
                    _ => false,
                }
        }
    }
}

impl Display for ObjFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        match self.name {
            Some(nm) => unsafe { write!(f, "<fn {}>", &(*nm).data) },
            None => write!(f, "<script>"),
        }
    }
}

impl ObjFunction {
    pub fn new(name: &ObjString) -> Self {
        Self {
            arity: 0,
            chunk: Chunk::new(),
            name: Some(name),
        }
    }

    pub fn new_unnamed() -> Self {
        Self {
            arity: 0,
            chunk: Chunk::new(),
            name: None,
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

impl Hashed for ObjString {
    fn calculated_hash(&self) -> u64 {
        self.hash
    }
}

impl ObjString {
    pub fn new_interned(value: String, cache: &mut Table<ObjString, Value>) -> &Self {
        let mut hasher = DefaultHasher::new();
        value.hash(&mut hasher);

        let instance = Self {
            data: value.clone(),
            hash: hasher.finish(),
        };

        cache.get_or_insert(instance, Value::Nil())
    }

    pub fn add_interned<'a>(
        lhs: &Self,
        rhs: &Self,
        cache: &'a mut Table<ObjString, Value>,
    ) -> &'a Self {
        let mut concatenated = String::with_capacity(lhs.data.len() + rhs.data.len());
        concatenated.push_str(&lhs.data);
        concatenated.push_str(&rhs.data);

        Self::take_string_interned(concatenated, cache)
    }

    pub fn copy_string_interned<'a>(
        value: &str,
        cache: &'a mut Table<ObjString, Value>,
    ) -> &'a Self {
        Self::new_interned(String::from(value), cache)
    }

    pub fn take_string_interned(value: String, cache: &mut Table<ObjString, Value>) -> &Self {
        Self::new_interned(value, cache)
    }
}
