use datum::Datum;
use rule::*;

fn setup() -> Rule<Datum, Datum> {
    from_json!(Rule<Datum, Datum>, {
      "lhs": [{"vec": [{"str": "x"}, {"var": "?x"}]}],
      "rhs": {"vec": [{"str": "y"}, {"var": "?y"}]},
      "constraints": [
        {"numerical": {"set": {"variable": "?diff", "constant": 25}}},
        {"numerical": {"sum": {"first": "?x", "second": "?y", "third": "?diff"}}}
      ],
    })
}

#[test]
fn test_snowflake() {
    let rule: Rule<Datum, Datum> = setup();

    let expected_snowflake = from_json!(Rule<Datum, Datum>, {
      "lhs": [{"vec": [{"str": "x"}, {"var": "?x::test"}]}],
      "rhs": {"vec": [{"str": "y"}, {"var": "?y::test"}]},
      "constraints": [
        {"numerical": {"set": {"variable": "?diff::test", "constant": 25}}},
        {"numerical": {"sum": {"first": "?x::test", "second": "?y::test", "third": "?diff::test"}}}
      ],
    });

    assert_eq!(rule.snowflake("test".to_string()), expected_snowflake);
}

#[test]
fn test_rule_application() {
    let rule: Rule<Datum, Datum> = setup();
    let initial_datum = from_json!(Datum, {"vec": [{"str": "x"}, {"float": 10}]});
    let expected_datum = from_json!(Datum, {"vec": [{"str": "y"}, {"float": 15}]});
    let expected_bindings: Bindings<Datum> = Bindings::new()
        .set_binding(&"?diff".to_string(), Datum::Float(25.0))
        .set_binding(&"?x".to_string(), Datum::Float(10.0))
        .set_binding(&"?y".to_string(), Datum::Float(15.0));
    assert_eq!(rule.apply(&vec![&initial_datum], &Bindings::new()),
               Some((expected_datum, expected_bindings)));
}

#[test]
fn test_rule_application_with_no_antecedents() {
    let rule = from_json!(Rule<Datum, Datum>, {
      "rhs": {"vec": [{"str": "y"}, {"float": 1}]},
    });
    let expected_datum = from_json!(Datum, {"vec": [{"str": "y"}, {"float": 1}]});
    let expected_bindings: Bindings<Datum> = Bindings::new();
    assert_eq!(rule.apply(&Vec::new(), &Bindings::new()),
               Some((expected_datum, expected_bindings)));
}

#[test]
fn test_rule_reverse_application() {
    let rule: Rule<Datum, Datum> = setup();
    let expected_datum = from_json!(Datum, {"vec": [{"str": "x"}, {"float": 10}]});
    let initial_datum = from_json!(Datum, {"vec": [{"str": "y"}, {"float": 15}]});
    let expected_bindings = Bindings::new()
        .set_binding(&"?diff".to_string(), Datum::Float(25.0))
        .set_binding(&"?x".to_string(), Datum::Float(10.0))
        .set_binding(&"?y".to_string(), Datum::Float(15.0));
    assert_eq!(rule.r_apply(&initial_datum, &Bindings::new()),
               Some((vec![expected_datum], expected_bindings)));
}
