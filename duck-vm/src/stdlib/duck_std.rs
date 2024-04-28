use crate::vm::{
    errors::VMError,
    memory::Value,
    native::{NativeArgs, NativeResult, NativeValue},
};

/// Identity native function that returns zero
pub fn quack(_: NativeArgs) -> NativeResult {
    println!("Quack");
    Ok(None)
}

pub fn time(_: NativeArgs) -> NativeResult {
    let now = std::time::SystemTime::now();
    Ok(Some(NativeValue::Number(
        now.elapsed().unwrap().as_millis() as f64,
    )))
}

pub fn append_string(args: NativeArgs) -> NativeResult {
    if args.len() != 2 {
        return Err(VMError::IncorrectArity {
            expected_arity: 2,
            actual_arity: args.len(),
        });
    }
    let from = args.get(0).unwrap();
    let append = args.get(1).unwrap();
    match from {
        Value::String(s1) => match append {
            Value::String(s2) => {
                let mut result = s1.to_string();
                result.push_str(s2);
                Ok(Some(NativeValue::String(result)))
            }
            _ => panic!("Invalid String"),
        },
        _ => panic!("Invalid String"),
    }
}
