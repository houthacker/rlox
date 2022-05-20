use std::fmt::Formatter;

#[cfg_attr(feature = "rlox_debug", derive(Debug))]
#[derive(Copy, Clone, PartialEq)]
pub enum ValueType {
    Nil,
    Bool,
    Number,
}

#[repr(C)]
#[derive(Copy, Clone)]
pub union U {
    pub boolean: bool,
    pub number: f64,
}

#[repr(C)]
#[derive(Copy)]
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
        }

        debug_struct.finish()
    }
}

impl Clone for Value {
    fn clone(&self) -> Self {
        match self.kind {
            ValueType::Nil => Value {
                kind: ValueType::Nil,
                to: U { number: 0.0 },
            },
            ValueType::Bool => Value {
                kind: ValueType::Bool,
                to: U {
                    boolean: unsafe { self.to.boolean },
                },
            },
            ValueType::Number => Value {
                kind: ValueType::Number,
                to: U {
                    number: unsafe { self.to.number },
                },
            },
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
}

#[macro_export]
macro_rules! as_bool {
    ($arg:expr) => {{
        {
            let value: Value = $arg;
            unsafe { value.to.boolean }
        }
    }};
}

#[macro_export]
macro_rules! as_number {
    ($arg:expr) => {{
        {
            let value: Value = $arg;
            unsafe { value.to.number }
        }
    }};
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

pub fn print_value(value: &Value) {
    match value.kind {
        ValueType::Nil => print!("nil"),
        ValueType::Bool => print!("{}", unsafe { value.to.boolean }),
        ValueType::Number => print!("{}", unsafe { value.to.number }),
    }
}
