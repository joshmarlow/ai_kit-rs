use constraints::{Constraint, SolveResult};

#[test]
fn test_constraint_sum_to_sexp() {
    let constraint: Constraint<Datum> = Constraint::Sum {
        first: "?x".to_string(),
        second: "?y".to_string(),
        third: "?z".to_string(),
        _marker: PhantomData,
    };
    let expected_sexp = sexp::parse(&"(constraint-sum (?x ?y ?z))").unwrap();
    assert_eq!(constraint.to_sexp(), expected_sexp);
}

#[test]
fn test_constraint_mul_to_sexp() {
    let constraint: Constraint<Datum> = Constraint::Mul {
        first: "?x".to_string(),
        second: "?y".to_string(),
        third: "?z".to_string(),
        _marker: PhantomData,
    };
    let expected_sexp = sexp::parse(&"(constraint-mul (?x ?y ?z))").unwrap();
    assert_eq!(constraint.to_sexp(), expected_sexp);
}


#[test]
fn test_constraint_greater_than_to_sexp() {
    let constraint: Constraint<Datum> = Constraint::GreaterThan {
        left: "?x".to_string(),
        right: "?y".to_string(),
        _marker: PhantomData,
    };
    let expected_sexp = sexp::parse(&"(constraint-greater-than (?x ?y))").unwrap();
    assert_eq!(constraint.to_sexp(), expected_sexp);
}

#[test]
fn test_constraint_greater_than_from_sexp() {
    let expected_constraint: Constraint<Datum> = Constraint::GreaterThan {
        left: "?x".to_string(),
        right: "?y".to_string(),
        _marker: PhantomData,
    };
    let constraint = Constraint::from_sexp_str(&"(constraint-greater-than (?x ?y))").unwrap();
    assert_eq!(constraint, expected_constraint);
}

#[test]
fn test_solve_sum_constraint_forward() {
    let constraint: Constraint<Datum> = Constraint::Sum {
        first: "?x".to_string(),
        second: "?y".to_string(),
        third: "?z".to_string(),
        _marker: PhantomData,
    };
    let bindings: Bindings<Datum> =
        vec![("?x".to_string(), Datum::from_float(10.0)), ("?y".to_string(), Datum::from_float(5.0))].into_iter().collect();
    let expected_bindings: Bindings<Datum> =
        vec![("?x".to_string(), Datum::from_float(10.0)), ("?y".to_string(), Datum::from_float(5.0)), ("?z".to_string(), Datum::from_float(15.0))]
            .into_iter()
            .collect();
    assert_eq!(constraint.solve(&bindings),
               SolveResult::Success(expected_bindings));
}

#[test]
fn test_solve_sum_constraint_backward() {
    let constraint: Constraint<Datum> = Constraint::Sum {
        first: "?x".to_string(),
        second: "?y".to_string(),
        third: "?z".to_string(),
        _marker: PhantomData,
    };
    let bindings: Bindings<Datum> =
        vec![("?x".to_string(), Datum::from_float(10.0)), ("?z".to_string(), Datum::from_float(15.0))].into_iter().collect();
    let expected_bindings: Bindings<Datum> =
        vec![("?x".to_string(), Datum::from_float(10.0)), ("?y".to_string(), Datum::from_float(5.0)), ("?z".to_string(), Datum::from_float(15.0))]
            .into_iter()
            .collect();
    assert_eq!(constraint.solve(&bindings),
               SolveResult::Success(expected_bindings));
}

#[test]
fn test_solve_mul_constraint_forward() {
    let constraint: Constraint<Datum> = Constraint::Mul {
        first: "?x".to_string(),
        second: "?y".to_string(),
        third: "?z".to_string(),
        _marker: PhantomData,
    };
    let bindings: Bindings<Datum> =
        vec![("?x".to_string(), Datum::from_float(3.0)), ("?y".to_string(), Datum::from_float(5.0))].into_iter().collect();
    let expected_bindings: Bindings<Datum> =
        vec![("?x".to_string(), Datum::from_float(3.0)), ("?y".to_string(), Datum::from_float(5.0)), ("?z".to_string(), Datum::from_float(15.0))]
            .into_iter()
            .collect();
    assert_eq!(constraint.solve(&bindings),
               SolveResult::Success(expected_bindings));
}

#[test]
fn test_solve_mul_constraint_backward() {
    let constraint: Constraint<Datum> = Constraint::Mul {
        first: "?x".to_string(),
        second: "?y".to_string(),
        third: "?z".to_string(),
        _marker: PhantomData,
    };
    let bindings: Bindings<Datum> =
        vec![("?x".to_string(), Datum::from_float(3.0)), ("?z".to_string(), Datum::from_float(15.0))].into_iter().collect();
    let expected_bindings: Bindings<Datum> =
        vec![("?x".to_string(), Datum::from_float(3.0)), ("?y".to_string(), Datum::from_float(5.0)), ("?z".to_string(), Datum::from_float(15.0))]
            .into_iter()
            .collect();
    assert_eq!(constraint.solve(&bindings),
               SolveResult::Success(expected_bindings));
}

#[test]
fn test_solve_less_than_constraint_succeeds() {
    let constraint: Constraint<Datum> = Constraint::LessThan {
        left: "?x".to_string(),
        right: "?y".to_string(),
        _marker: PhantomData,
    };
    let bindings: Bindings<Datum> =
        vec![("?x".to_string(), Datum::from_float(10.0)), ("?y".to_string(), Datum::from_float(15.0))].into_iter().collect();
    assert_eq!(constraint.solve(&bindings), SolveResult::Success(bindings));
}

#[test]
fn test_solve_less_than_constraint_fails() {
    let constraint: Constraint<Datum> = Constraint::LessThan {
        left: "?x".to_string(),
        right: "?y".to_string(),
        _marker: PhantomData,
    };
    let bindings: Bindings<Datum> =
        vec![("?x".to_string(), Datum::from_float(10.0)), ("?y".to_string(), Datum::from_float(5.0))].into_iter().collect();
    assert_eq!(constraint.solve(&bindings), SolveResult::Conflict);
}

#[test]
fn test_solve_greater_than_constraint_succeeds() {
    let constraint: Constraint<Datum> = Constraint::GreaterThan {
        left: "?x".to_string(),
        right: "?y".to_string(),
        _marker: PhantomData,
    };
    let bindings: Bindings<Datum> =
        vec![("?x".to_string(), Datum::from_float(15.0)), ("?y".to_string(), Datum::from_float(5.0))].into_iter().collect();
    assert_eq!(constraint.solve(&bindings), SolveResult::Success(bindings));
}

#[test]
fn test_solve_greater_than_constraint_fails() {
    let constraint: Constraint<Datum> = Constraint::GreaterThan {
        left: "?x".to_string(),
        right: "?y".to_string(),
        _marker: PhantomData,
    };
    let bindings: Bindings<Datum> =
        vec![("?x".to_string(), Datum::from_float(5.0)), ("?y".to_string(), Datum::from_float(15.0))].into_iter().collect();
    assert_eq!(constraint.solve(&bindings), SolveResult::Conflict);
}

#[test]
fn test_solve_multi_constraint() {
    let constraints = vec![Constraint::from_sexp_str("(constraint-set (?diff 5.0))").unwrap(),
                           Constraint::from_sexp_str("(constraint-sum (?x ?y ?diff))").unwrap(),
                           Constraint::from_sexp_str("(constraint-sum (?w ?x ?diff))").unwrap()];
    let bindings: Bindings<Datum> = vec![("?w".to_string(), Datum::from_float(5.0))].into_iter().collect();
    let expected_bindings: Bindings<Datum> = vec![("?diff".to_string(), Datum::from_float(5.0)),
                                                  ("?w".to_string(), Datum::from_float(5.0)),
                                                  ("?x".to_string(), Datum::from_float(0.0)),
                                                  ("?y".to_string(), Datum::from_float(5.0))]
        .into_iter()
        .collect();

    assert_eq!(Constraint::solve_many(constraints.iter().collect(), &bindings),
               SolveResult::Success(expected_bindings));
}

#[test]
fn test_solve_multi_constraint_terminates_when_unsolvable() {
    let constraints = vec![Constraint::from_sexp_str("(constraint-set (?diff 5.0))").unwrap(),
                           Constraint::from_sexp_str("(constraint-sum (?z ?y ?diff))").unwrap(),
                           Constraint::from_sexp_str("(constraint-sum (?w ?x ?diff))").unwrap()];
    let bindings: Bindings<Datum> = vec![("?w".to_string(), Datum::from_float(5.0))].into_iter().collect();
    let expected_bindings: Bindings<Datum> =
        vec![("?diff".to_string(), Datum::from_float(5.0)), ("?w".to_string(), Datum::from_float(5.0)), ("?x".to_string(), Datum::from_float(0.0))]
            .into_iter()
            .collect();

    assert_eq!(Constraint::solve_many(constraints.iter().collect(), &bindings),
               SolveResult::Partial(expected_bindings));
}
