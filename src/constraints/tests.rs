use constraints::*;

#[test]
fn test_solve_sum_constraint_forward() {
    let constraint: Constraint = Constraint::Numerical(NumericalConstraint::Sum {
        first: "?x".to_string(),
        second: "?y".to_string(),
        third: "?z".to_string(),
    });
    let bindings: Bindings<Number> = vec![("?x".to_string(), Number::new(10.0)), ("?y".to_string(), Number::new(5.0))].into_iter().collect();
    let expected_bindings: Bindings<Number> =
        vec![("?x".to_string(), Number::new(10.0)), ("?y".to_string(), Number::new(5.0)), ("?z".to_string(), Number::new(15.0))]
            .into_iter()
            .collect();
    assert_eq!(constraint.solve(&bindings),
               SolveResult::Success(expected_bindings));
}

#[test]
fn test_solve_sum_constraint_backward() {
    let constraint: Constraint = Constraint::Numerical(NumericalConstraint::Sum {
        first: "?x".to_string(),
        second: "?y".to_string(),
        third: "?z".to_string(),
    });
    let bindings: Bindings<Number> = vec![("?x".to_string(), Number::new(10.0)), ("?z".to_string(), Number::new(15.0))].into_iter().collect();
    let expected_bindings: Bindings<Number> =
        vec![("?x".to_string(), Number::new(10.0)), ("?y".to_string(), Number::new(5.0)), ("?z".to_string(), Number::new(15.0))]
            .into_iter()
            .collect();
    assert_eq!(constraint.solve(&bindings),
               SolveResult::Success(expected_bindings));
}

#[test]
fn test_solve_mul_constraint_forward() {
    let constraint: Constraint = Constraint::Numerical(NumericalConstraint::Mul {
        first: "?x".to_string(),
        second: "?y".to_string(),
        third: "?z".to_string(),
    });
    let bindings: Bindings<Number> = vec![("?x".to_string(), Number::new(3.0)), ("?y".to_string(), Number::new(5.0))].into_iter().collect();
    let expected_bindings: Bindings<Number> =
        vec![("?x".to_string(), Number::new(3.0)), ("?y".to_string(), Number::new(5.0)), ("?z".to_string(), Number::new(15.0))]
            .into_iter()
            .collect();
    assert_eq!(constraint.solve(&bindings),
               SolveResult::Success(expected_bindings));
}

#[test]
fn test_solve_mul_constraint_backward() {
    let constraint: Constraint = Constraint::Numerical(NumericalConstraint::Mul {
        first: "?x".to_string(),
        second: "?y".to_string(),
        third: "?z".to_string(),
    });
    let bindings: Bindings<Number> = vec![("?x".to_string(), Number::new(3.0)), ("?z".to_string(), Number::new(15.0))].into_iter().collect();
    let expected_bindings: Bindings<Number> =
        vec![("?x".to_string(), Number::new(3.0)), ("?y".to_string(), Number::new(5.0)), ("?z".to_string(), Number::new(15.0))]
            .into_iter()
            .collect();
    assert_eq!(constraint.solve(&bindings),
               SolveResult::Success(expected_bindings));
}

#[test]
fn test_solve_greater_than_constraint_succeeds() {
    let constraint: Constraint = Constraint::Numerical(NumericalConstraint::GreaterThan {
        left: "?x".to_string(),
        right: "?y".to_string(),
    });
    let bindings: Bindings<Number> = vec![("?x".to_string(), Number::new(15.0)), ("?y".to_string(), Number::new(5.0))].into_iter().collect();
    assert_eq!(constraint.solve(&bindings), SolveResult::Success(bindings));
}

#[test]
fn test_solve_greater_than_constraint_fails() {
    let constraint: Constraint = Constraint::Numerical(NumericalConstraint::GreaterThan {
        left: "?x".to_string(),
        right: "?y".to_string(),
    });
    let bindings: Bindings<Number> = vec![("?x".to_string(), Number::new(5.0)), ("?y".to_string(), Number::new(15.0))].into_iter().collect();
    assert_eq!(constraint.solve(&bindings), SolveResult::Conflict);
}

#[test]
fn test_solve_multi_constraint() {
    let constraints = from_json!(Vec<Constraint>, [
    {
        "numerical": {
            "set": {
                "variable": "?diff",
                "constant": 5.0,
            },
        },
    },
    {
        "numerical": {
            "sum": {
                "first": "?x",
                "second": "?y",
                "third": "?diff",
            },
        },
    },
    {
        "numerical": {
            "sum": {
                "first": "?w",
                "second": "?x",
                "third": "?diff",
            },
        }
    }]);
    let bindings: Bindings<Number> = Bindings::new().set_binding(&"?w".to_string(), Number::new(5.0));
    let expected_bindings: Bindings<Number> = Bindings::new()
        .set_binding(&"?diff".to_string(), Number::new(5.0))
        .set_binding(&"?w".to_string(), Number::new(5.0))
        .set_binding(&"?x".to_string(), Number::new(0.0))
        .set_binding(&"?y".to_string(), Number::new(5.0));

    println!("\n");
    assert_eq!(Constraint::solve_many(constraints.iter().collect(), &bindings),
               SolveResult::Success(expected_bindings));
}

#[test]
fn test_solve_multi_constraint_terminates_when_unsolvable() {
    let constraints = from_json!(Vec<Constraint>, [
    {
        "numerical": {
            "set": {
                "variable": "?diff",
                "constant": 5.0,
            },
        },
    },
    {
        "numerical": {
            "sum": {
                "first": "?z",
                "second": "?y",
                "third": "?diff",
            },
        },
    },
    {
        "numerical": {
            "sum": {
                "first": "?w",
                "second": "?x",
                "third": "?diff",
            },
        }
    }]);
    let bindings: Bindings<Number> = Bindings::new().set_binding(&"?w".to_string(), Number::new(5.0));
    let expected_bindings: Bindings<Number> = Bindings::new()
        .set_binding(&"?diff".to_string(), Number::new(5.0))
        .set_binding(&"?w".to_string(), Number::new(5.0))
        .set_binding(&"?x".to_string(), Number::new(0.0));

    assert_eq!(Constraint::solve_many(constraints.iter().collect(), &bindings),
               SolveResult::Partial(expected_bindings));
}
