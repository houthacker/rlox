use crate::object::Obj;
use std::fmt::Formatter;

pub enum Value {
    Nil(),
    Boolean(bool),
    Number(f64),
    Obj(Obj),
}

impl Default for Value {
    fn default() -> Self {
        Value::Nil()
    }
}

impl Clone for Value {
    fn clone(&self) -> Self {
        match self {
            Value::Nil() => Value::Nil(),
            Value::Boolean(b) => Value::from_bool(*b),
            Value::Number(n) => Value::from_number(*n),
            Value::Obj(obj) => Value::Obj(obj.clone()),
        }
    }
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut debug_struct = f.debug_struct("Value");

        match self {
            Value::Nil() => {
                debug_struct.field("value", &"nil");
            }
            Value::Number(n) => {
                debug_struct.field("value", n);
            }
            Value::Boolean(b) => {
                debug_struct.field("value", b);
            }
            Value::Obj(obj) => {
                debug_struct.field("value", &obj.to_string());
            }
        }

        debug_struct.finish()
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        use std::mem::discriminant;
        if discriminant(self) != discriminant(other) {
            return false;
        }

        match (self, other) {
            (Value::Nil(), Value::Nil()) => true,
            (Value::Boolean(lhs), Value::Boolean(rhs)) => *lhs == *rhs,
            (Value::Number(lhs), Value::Number(rhs)) => *lhs == *rhs,
            (Value::Obj(lhs), Value::Obj(rhs)) => lhs == rhs,
            _ => false,
        }
    }
}

impl Value {
    #[inline(always)]
    pub fn from_bool(value: bool) -> Value {
        Value::Boolean(value)
    }

    #[inline(always)]
    pub fn from_nil() -> Value {
        Value::Nil()
    }

    #[inline(always)]
    pub fn from_number(value: f64) -> Value {
        Value::Number(value)
    }

    #[inline(always)]
    pub fn from_obj(value: Obj) -> Value {
        Value::Obj(value)
    }

    pub fn is_bool(value: &Value) -> bool {
        matches!(value, &Value::Boolean(_))
    }

    pub fn is_nil(value: &Value) -> bool {
        matches!(value, &Value::Nil())
    }

    pub fn is_number(value: &Value) -> bool {
        matches!(value, &Value::Number(_))
    }

    pub fn is_string(value: &Value) -> bool {
        match value {
            Value::Obj(obj) => match obj {
                Obj::String(_not_used) => true,
            },
            _ => false,
        }
    }
}

#[inline(always)]
pub fn as_bool(value: &Value) -> bool {
    match value {
        &Value::Boolean(v) => v,
        _ => panic!("Value discriminant is not a Value::Boolean"),
    }
}

#[inline(always)]
pub fn as_number(value: &Value) -> f64 {
    match value {
        &Value::Number(v) => v,
        _ => panic!("Value discriminant is not a Value::Number"),
    }
}

pub fn print_value(value: &Value) {
    match value {
        Value::Nil() => print!("nil"),
        Value::Boolean(b) => print!("{}", b),
        Value::Number(n) => print!("{}", n),
        Value::Obj(obj) => print!("{}", obj),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::object::{value_as_rlox_string_ref, ObjString};
    use crate::table::Table;

    #[test]
    fn test_string_conversion() {
        let mut cache = Table::new();
        let s = String::from("test");
        let b = ObjString::take_string(s, &mut cache);
        let val1 = Value::from_obj(Obj::String(b));

        println!("{}", Value::is_string(&val1));
        println!("{}", value_as_rlox_string_ref(val1));
    }
}
