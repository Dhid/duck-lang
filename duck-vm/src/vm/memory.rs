use super::{
    gc::{Gc, Trace},
    native::Native,
};
use duck_bytecode::bytecode::Function;
use std::collections::HashMap;
use std::{cell::Cell, fmt::Display};

#[derive(Debug, Clone, Copy)]
pub enum Upvalue {
    Open(usize),   // stores the index of the value to be closened
    Closed(Value), // stores the value which is closed over
}

impl Trace for Upvalue {
    #[inline]
    fn trace(&self) {
        match self {
            Upvalue::Open(_) => {}
            Upvalue::Closed(value) => value.trace(),
        }
    }
}

impl Upvalue {
    pub fn is_open_with_index(&self, index: usize) -> bool {
        match self {
            Self::Open(i) => {
                if *i == index {
                    true
                } else {
                    false
                }
            }
            Self::Closed(_) => false,
        }
    }

    pub fn is_open(&self) -> bool {
        match self {
            Self::Open(_) => true,
            Self::Closed(_) => false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Closure {
    pub function: Function,
    pub upvalues: Vec<Gc<Cell<Upvalue>>>,
}

#[derive(Debug, Clone, Default)]
pub struct ObjInstance {
    // TODO this must be a Map of Values
    pub properties: HashMap<String, Cell<Value>>,
}

impl ObjInstance {
    pub fn add_property(&mut self, name: String, value: Cell<Value>) {
        self.properties.insert(name, value);
    }

    pub fn get_property(&self, name: &String) -> Option<Value> {
        let v = self.properties.get(name);
        match v {
            Some(value) => Some(value.get()),
            None => None,
        }
    }

    pub fn set_property(&self, name: &String, value: Value) {
        let v = self.properties.get(name);
        match v {
            Some(found) => {
                found.replace(value);
            }
            None => {}
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Value {
    Number(f64),
    String(Gc<String>),
    Boolean(bool),
    Func(DuckFn),
    Instance(Gc<ObjInstance>),
    Nil,
}

#[derive(Debug, Clone, Copy)]
pub enum DuckFn {
    Script { func: Gc<Closure> },
    Native { func: Gc<Native> },
}

impl Into<f64> for Value {
    fn into(self) -> f64 {
        match self {
            Value::Number(v) => v,
            Value::Boolean(b) => match b {
                true => 1.0,
                false => 0.0,
            },
            Value::Nil => 0.0,
            _ => 1.0,
        }
    }
}

impl Into<bool> for Value {
    fn into(self) -> bool {
        match self {
            Value::Boolean(v) => v,
            _ => panic!("Value is not a Boolean: {:?}", self),
        }
    }
}

impl Into<String> for Value {
    fn into(self) -> String {
        match self {
            Value::String(v) => (*v).clone(),
            _ => panic!("Value is not a String: {:?}", self),
        }
    }
}

impl Into<DuckFn> for Value {
    fn into(self) -> DuckFn {
        match self {
            Value::Func(v) => v.clone(),
            _ => panic!("Value is not a DuckFn: {:?}", self),
        }
    }
}

impl Trace for Value {
    #[inline]
    fn trace(&self) {
        match self {
            Value::String(string) => string.trace(),
            Value::Func(f) => match f {
                DuckFn::Script { func } => func.trace(),
                DuckFn::Native { func } => func.trace(),
            },
            Value::Instance(instance) => instance.trace(),
            Value::Number(_) => (),
            Value::Nil => (),
            Value::Boolean(_) => (),
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        match self {
            Value::Number(n) => {
                write!(f, "{}", n)
            }
            Value::String(s) => {
                write!(f, "{}", s.as_str())
            }
            Value::Boolean(b) => {
                write!(f, "{}", b)
            }
            Value::Nil => {
                write!(f, "Nil")
            }
            Value::Func(func) => {
                write!(f, "Func: {:?}", func)
            }
            Value::Instance(i) => {
                write!(f, "Instance {:?}", i)
            }
        }
    }
}
