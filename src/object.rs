use crate::value::{as_obj_ref, Value};
use std::any::Any;
use std::ops;

use crate::obj_type;

#[derive(Copy, Clone, PartialEq)]
pub enum ObjType {
    String,
}

pub trait CloneObj {
    fn clone_obj<'a>(&self) -> Box<dyn Obj>;
}

impl<T> CloneObj for T
where
    T: Clone + Obj + 'static,
{
    fn clone_obj(&self) -> Box<dyn Obj> {
        Box::new(self.clone())
    }
}

pub trait Obj: CloneObj {
    fn kind(&self) -> ObjType;

    fn as_any(&self) -> &dyn Any;
}

impl Clone for Box<dyn Obj> {
    fn clone(&self) -> Self {
        self.clone_obj()
    }
}

pub unsafe fn obj_from_ptr<'a, 'b>(ptr: *const (dyn Obj + 'a)) -> &'a dyn Obj {
    ptr.as_ref().unwrap_unchecked()
}

#[derive(Clone)]
pub struct ObjString {
    pub data: String,
}

impl Obj for ObjString {
    fn kind(&self) -> ObjType {
        ObjType::String
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl ObjString {
    pub fn from_slice(value: &str) -> Self {
        Self {
            data: String::from(value),
        }
    }

    pub fn to_obj_ptr(&self) -> *const dyn Obj {
        self as *const dyn Obj
    }

    pub fn to_obj_ref(&self) -> &dyn Obj {
        self as &dyn Obj
    }
}

impl ops::Add<&ObjString> for &ObjString {
    type Output = ObjString;

    fn add(self, rhs: &ObjString) -> Self::Output {
        let mut s = String::with_capacity(self.data.len() + rhs.data.len());
        s.push_str(&self.data);
        s.push_str(&rhs.data);

        ObjString { data: s }
    }
}

pub unsafe fn as_string_ref(value: &Value) -> &ObjString {
    as_obj_ref(value)
        .as_any()
        .downcast_ref::<ObjString>()
        .unwrap_unchecked()
}

pub unsafe fn as_rstring_ref(value: &Value) -> &str {
    as_string_ref(value).data.as_ref()
}

pub fn print_object(value: &Value) {
    match obj_type!(value) {
        ObjType::String => {
            let s = unsafe { as_string_ref(value) };
            let data = &s.data;
            let len = data.len();
            print!("{}", data)
        }
    }
}

#[macro_export]
macro_rules! obj_type {
    ($arg:expr) => {{
        {
            let value: &Value = $arg;
            unsafe { crate::value::as_obj_ref(value).kind() }
        }
    }};
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn x() {}
}
