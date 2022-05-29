use crate::value::Value;
use std::any::Any;
use std::fmt::{Display, Formatter};
use std::ops;

#[derive(Copy, Clone, PartialEq)]
pub enum ObjType {
    String,
}

pub trait CloneObj {
    fn clone_box(&self) -> Box<dyn Obj>;
}

impl<T> CloneObj for T
where
    T: Clone + Obj + 'static,
{
    fn clone_box(&self) -> Box<dyn Obj> {
        Box::new(self.clone())
    }
}

pub trait Obj: CloneObj + ToString {
    fn kind(&self) -> ObjType;

    fn as_any(&self) -> &dyn Any;
}

pub struct ObjString {
    pub data: String,
}

impl Clone for ObjString {
    fn clone(&self) -> Self {
        Self {
            data: self.data.clone(),
        }
    }
}

impl Obj for ObjString {
    fn kind(&self) -> ObjType {
        ObjType::String
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Display for ObjString {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.data)
    }
}

impl ObjString {
    pub fn copy_string(value: &str) -> Self {
        Self {
            data: String::from(value),
        }
    }

    pub fn take_string(value: String) -> Self {
        Self { data: value }
    }
}

impl ops::Add<&ObjString> for &ObjString {
    type Output = ObjString;

    fn add(self, rhs: &ObjString) -> Self::Output {
        let mut s = String::with_capacity(self.data.len() + rhs.data.len());
        s.push_str(&self.data);
        s.push_str(&rhs.data);

        ObjString::take_string(s)
    }
}

impl PartialEq for ObjString {
    fn eq(&self, other: &Self) -> bool {
        self.data == other.data
    }
}

pub fn obj_equal(lhs: &Box<dyn Obj>, rhs: &Box<dyn Obj>) -> bool {
    let lhs_kind = (*lhs).kind();
    if lhs_kind != (*rhs).kind() {
        false
    } else {
        match lhs_kind {
            ObjType::String => obj_as_rlox_string_ref(lhs) == obj_as_rlox_string_ref(rhs),
        }
    }
}

fn obj_as_rlox_string_ref(b: &Box<dyn Obj>) -> &ObjString {
    b.as_any().downcast_ref::<ObjString>().unwrap()
}

pub fn value_as_rlox_string_ref(value: &Value) -> &ObjString {
    match value {
        Value::Obj(obj) => obj.as_any().downcast_ref::<ObjString>().unwrap(),
        _ => panic!("Value discriminant is not a Value::Obj"),
    }
}

#[inline(always)]
pub fn obj_type(value: &Value) -> ObjType {
    match value {
        Value::Obj(obj) => obj.kind(),
        _ => panic!("Value is not an Obj"),
    }
}
