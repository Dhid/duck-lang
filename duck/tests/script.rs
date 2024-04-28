#[cfg(test)]
mod tests {
    use duck_compiler::compile;
    use duck_vm::vm::{memory::Value, vm::VM};

    fn run_script(code: &str) -> Result<Option<Value>, ()> {
        let instance = compile(code).unwrap();
        let mut vm = VM::new(instance);
        let res = vm.run();
        match res {
            Ok(val) => match val {
                duck_vm::vm::vm::InterpreterResult::Ok(res) => Ok(res),
                _ => Err(()),
            },
            Err(_) => Err(()),
        }
    }

    #[test]
    fn sum_two_numbers() {
        let res = run_script(" 1 + 2;").unwrap().unwrap();
        let value: f64 = res.into();
        assert_eq!(value, 3.0);
    }

    #[test]
    fn mul_two_numbers() {
        let res = run_script(" 3 * 2;").unwrap().unwrap();
        let value: f64 = res.into();
        assert_eq!(value, 6.0);
    }

    #[test]
    fn div_two_numbers() {
        let res = run_script(" 3 / 2;").unwrap().unwrap();
        let value: f64 = res.into();
        assert_eq!(value, 3.0 / 2.0);
    }

    #[test]
    fn div_by_zero() {
        let res = run_script(" 3 / 0;");
        assert_eq!(res.is_err(), true);
    }

    #[test]
    fn define_fn() {
        let res = run_script("fn foo() {return 2;}").unwrap();
        assert_eq!(res.is_none(), true);
    }

    #[test]
    fn define_and_callfn() {
        let res = run_script("fn foo() {return 2;} foo();").unwrap().unwrap();
        let value: f64 = res.into();
        assert_eq!(value, 2.0);
    }

    #[test]
    fn if_branch() {
        let res = run_script("if(5 > 0) {5;}").unwrap().unwrap();
        let value: f64 = res.into();
        assert_eq!(value, 5.0);
    }

    #[test]
    fn else_branch() {
        let res = run_script("if(5 < 0) {false;} else {true;}")
            .unwrap()
            .unwrap();
        let value: bool = res.into();
        assert_eq!(value, true);
    }

    #[test]
    fn create_obj_instance() {
        let res = run_script("#{nice: \"thanks\"};").unwrap().unwrap();
        assert!(matches!(res, Value::Instance { .. }));
    }

    #[test]
    fn get_prop_from_obj() {
        let res = run_script("const x = #{nice: \"thanks\"}; x.nice;")
            .unwrap()
            .unwrap();
        let value: String = res.into();
        assert_eq!(value, "thanks".to_string());
    }
}
