use crate::object::{as_rstring_ref, as_string_ref, print_object, Obj, ObjString, ObjType};
use crate::{bool_val, nil_val, number_val, obj_val};
use std::fmt::Formatter;
use std::mem::ManuallyDrop;

#[cfg_attr(feature = "rlox_debug", derive(Debug))]
#[derive(Clone, Copy, PartialEq)]
pub enum ValueType {
    Nil,
    Bool,
    Number,

    /// Heap-allocated value types
    Obj,
}

pub union U {
    pub boolean: bool,
    pub number: f64,
    pub obj_ptr: ManuallyDrop<Box<dyn Obj>>, // *const dyn Obj<_>
}

pub struct Value {
    pub kind: ValueType,
    pub to: U,
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut debug_struct = f.debug_struct("Value");
        debug_struct.field("kind", &self.kind);

        match self.kind {
            ValueType::Nil => {
                debug_struct.field("value", &"nil");
            }
            ValueType::Bool => {
                debug_struct.field("value", &unsafe { self.to.boolean });
            }
            ValueType::Number => {
                debug_struct.field("value", &unsafe { self.to.number });
            }
            ValueType::Obj => {
                debug_struct.field("value", unsafe { &as_rstring_ref(self) });
            }
        }

        debug_struct.finish()
    }
}

impl Clone for Value {
    fn clone(&self) -> Self {
        match self.kind {
            ValueType::Nil => nil_val!(),
            ValueType::Bool => bool_val!(unsafe { self.to.boolean }),
            ValueType::Number => number_val!(unsafe { self.to.number }),
            ValueType::Obj => {
                obj_val!(
                    unsafe { ManuallyDrop::into_inner(self.to.obj_ptr.clone()) } /* unsafe { ManuallyDrop::into_inner(self.to.obj_ptr) } */
                )
            }
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        if self.kind != other.kind {
            return false;
        }

        match self.kind {
            ValueType::Nil => true,
            ValueType::Bool => unsafe { self.to.boolean == other.to.boolean },
            ValueType::Number => unsafe { self.to.number == other.to.number },
            ValueType::Obj => unsafe {
                let a = as_string_ref(self);
                let b = as_string_ref(other);

                a.data.eq(&b.data)
            },
        }
    }
}

impl Value {
    pub fn is_bool(value: &Value) -> bool {
        value.kind == ValueType::Bool
    }

    pub fn is_nil(value: &Value) -> bool {
        value.kind == ValueType::Nil
    }

    pub fn is_number(value: &Value) -> bool {
        value.kind == ValueType::Number
    }

    pub fn is_obj(value: &Value) -> bool {
        value.kind == ValueType::Obj
    }

    pub fn is_string(value: &Value) -> bool {
        Value::is_obj(value) && unsafe { as_obj_ref(value).kind() == ObjType::String }
    }
}

#[macro_export]
macro_rules! as_bool {
    ($arg:expr) => {{
        {
            let value: &Value = $arg;
            unsafe { value.to.boolean }
        }
    }};
}

#[macro_export]
macro_rules! as_number {
    ($arg:expr) => {{
        {
            let value: &Value = $arg;
            unsafe { value.to.number }
        }
    }};
}

/*
#[macro_export]
macro_rules! as_obj {
    ($arg:expr) => {{
        {
            let value: &Value = $arg;
            unsafe {
                obj_from_ptr(std::mem::ManuallyDrop::<&dyn Obj>::into_inner(
                    value.to.obj_ptr,
                ))
            }
        }
    }};
}
 */

pub unsafe fn as_obj_ref(value: &Value) -> &Box<dyn Obj> {
    &value.to.obj_ptr
}

pub unsafe fn as_obj(value: Value) -> Box<dyn Obj> {
    ManuallyDrop::into_inner(value.to.obj_ptr)
}

#[macro_export]
macro_rules! bool_val {
    ($arg:expr) => {{
        {
            let value: bool = $arg;
            Value {
                kind: ValueType::Bool,
                to: U { boolean: value },
            }
        }
    }};
}

#[macro_export]
macro_rules! nil_val {
    () => {{
        {
            Value {
                kind: ValueType::Nil,
                to: U { number: 0.0 },
            }
        }
    }};
}

#[macro_export]
macro_rules! number_val {
    ($arg:expr) => {{
        {
            let value: f64 = $arg;
            Value {
                kind: ValueType::Number,
                to: U { number: value },
            }
        }
    }};
}

#[macro_export]
macro_rules! obj_val {
    ($arg:expr) => {{
        {
            let value: Box<dyn Obj> = $arg;
            Value {
                kind: ValueType::Obj,
                to: U {
                    obj_ptr: std::mem::ManuallyDrop::new(value),
                },
            }
        }
    }};
}

pub fn obj_val(value: Box<dyn Obj>) -> Value {
    Value {
        kind: ValueType::Obj,
        to: U {
            obj_ptr: ManuallyDrop::new(value),
        },
    }
}

pub fn print_value(value: &Value) {
    match value.kind {
        ValueType::Nil => print!("nil"),
        ValueType::Bool => print!("{}", unsafe { value.to.boolean }),
        ValueType::Number => print!("{}", unsafe { value.to.number }),
        ValueType::Obj => print_object(value),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::object::ObjString;

    #[test]
    fn test_string_conversion() {
        let s = String::from("test");
        let b = Box::new(ObjString::from_slice(&s));
        let val1 = obj_val!(b);

        println!("{}", Value::is_string(&val1));

        println!("{}", unsafe { as_rstring_ref(&val1) });

        let val1_addr = unsafe { as_string_ref(&val1).data.as_ptr() as usize };
    }
}
