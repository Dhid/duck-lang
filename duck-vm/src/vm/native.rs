use super::{errors::VMError, gc::Trace, memory::Value};

#[derive(Debug, Clone)]
pub enum NativeValue {
    Number(f64),
    String(String),
}

pub type NativeFunction = fn(NativeArgs) -> NativeResult;
pub type NativeResult = Result<Option<NativeValue>, VMError>;

pub type NativeArgs = Vec<Value>;
pub trait NativeCall {
    fn call(&self) -> NativeResult;
}
#[derive(Debug, Clone)]
pub struct Native {
    pub arity: usize,
    args: NativeArgs,
    callback: NativeFunction,
}

impl Native {
    pub fn new(arity: usize, callback: NativeFunction) -> Self {
        Native {
            arity,
            callback,
            args: Vec::with_capacity(arity),
        }
    }

    pub fn add_arg(&mut self, value: Value) {
        self.args.push(value);
    }
}

impl NativeCall for Native {
    fn call(&self) -> NativeResult {
        (self.callback)(self.args.clone())
    }
}

impl Trace for Native {
    fn trace(&self) {}
}
