use crate::as_obj_ref;
use crate::obj_type;
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

pub trait Obj: CloneObj {
    fn kind(&self) -> ObjType;

    fn as_any(&self) -> &dyn Any;
}

impl Clone for Box<dyn Obj> {
    fn clone(&self) -> Box<dyn Obj> {
        self.clone_box()
    }
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

impl Display for ObjString {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.data)
    }
}

impl ObjString {
    pub fn boxed_from_slice(value: &str) -> Box<Self> {
        Box::new(Self {
            data: String::from(value),
        })
    }
}

impl ops::Add<&ObjString> for &ObjString {
    type Output = Box<ObjString>;

    fn add(self, rhs: &ObjString) -> Self::Output {
        let mut s = String::with_capacity(self.data.len() + rhs.data.len());
        s.push_str(&self.data);
        s.push_str(&rhs.data);

        Box::new(ObjString { data: s })
    }
}

impl PartialEq for ObjString {
    fn eq(&self, other: &Self) -> bool {
        self.data == other.data
    }
}

impl Drop for ObjString {
    fn drop(&mut self) {
        println!("Dropping string {}", self.data)
    }
}

pub fn as_string_ref(value: &Value) -> &ObjString {
    as_obj_ref!(value)
        .as_any()
        .downcast_ref::<ObjString>()
        .unwrap()
}

pub unsafe fn as_rstring_ref(value: &Value) -> &str {
    as_string_ref(value).data.as_ref()
}

pub fn print_object(value: &Value) {
    match obj_type!(value) {
        ObjType::String => {
            print!("{}", as_string_ref(value));
        }
    }
}

#[macro_export]
macro_rules! obj_type {
    ($arg:expr) => {{
        {
            let value: &Value = $arg;
            as_obj_ref!(value).kind()
        }
    }};
}
