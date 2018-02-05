use super::super::core::{Bindings, Unify};
use datum::*;

macro_rules! assert_some_value {
($x:expr, $y:expr) => (match $x {
    Some(val) => assert_eq!(val, $y),
    None => panic!("Expected value but received 'None'"),
    })
}

macro_rules! assert_none {
($x:expr) => (match $x {
    None => (),
    Some(val) => panic!("Expected 'None' received {}", val),
    })
}

macro_rules! datum_json {
    ($json: tt) => ({
        use serde_json;
        let d: Datum = serde_json::from_value(json!($json)).expect("Expected json decoding");
        d
    })
}

#[test]
fn test_unify_passes_when_variables_match() {
    let d = from_json!(Datum, {
        "vec": [{"str": "action"}, {"int": 1}, {"var": "?::t1"}]
    });
    let bindings = Bindings::new().set_binding(&"?::t1".to_string(), Datum::Float(2.0));
    assert_eq!(d.unify(&d, &bindings), Some(bindings));
}

#[test]
fn test_unify_passes_when_match_with_new_variable_in_self() {
    let d = from_json!(Datum, {
        "vec": [{"str": "isa"}, {"str": "socrates"}, {"var": "?x"}]
    });
    let d2 = from_json!(Datum, {
        "vec": [{"str": "isa"}, {"str": "socrates"}, {"str": "man"}]
    });
    let expected_bindings = Bindings::new().set_binding(&"?x".to_string(), Datum::String("man".to_string()));
    let actual_bindings = d.unify(&d2, &Bindings::new());
    assert_some_value!(actual_bindings, expected_bindings);
}

#[test]
fn test_unify_passes_when_match_with_new_variable_in_other() {
    let d = from_json!(Datum, {
        "vec": [{"str": "isa"}, {"str": "socrates"}, {"var": "?x"}]
    });
    let d2 = from_json!(Datum, {
        "vec": [{"str": "isa"}, {"str": "socrates"}, {"str": "man"}]
    });
    let expected_bindings = Bindings::new().set_binding(&"?x".to_string(), Datum::String("man".to_string()));
    let actual_bindings = d2.unify(&d, &Bindings::new());
    assert_some_value!(actual_bindings, expected_bindings);
}

#[test]
fn test_unify_passes_with_matching_vectors() {
    let d = datum_json!(
        {"vec": [{"vec": [{"str": "current-state"}, {"var": "?s1"}]},
                 {"vec": [{"str": "time"}, {"var": "?t1"}]}]});
    let d2 = datum_json!({"vec": [
        {"vec":[{"str": "current-state"}, {"float": 0}]},
        {"vec":[{"str": "time"}, {"float": 0}]},
    ]});
    let expected_bindings = Bindings::new()
        .set_binding(&"?s1".to_string(), Datum::Float(0.0))
        .set_binding(&"?t1".to_string(), Datum::Float(0.0));
    let actual_bindings = d2.unify(&d, &Bindings::new());
    assert_some_value!(actual_bindings, expected_bindings);
}

#[test]
fn test_unify_passes_when_bindings_in_self_match() {
    let d = from_json!(Datum, {
        "vec": [{"str": "isa"}, {"str": "socrates"}, {"var": "?x"}]
    });
    let d2 = from_json!(Datum, {
        "vec": [{"str": "isa"}, {"str": "socrates"}, {"str": "man"}]
    });
    let bindings = Bindings::new().set_binding(&"?x".to_string(), Datum::String("man".to_string()));
    let actual_bindings = d.unify(&d2, &bindings);
    assert_some_value!(actual_bindings, bindings);
}

#[test]
fn test_unify_passes_when_bindings_in_other_match() {
    let d = from_json!(Datum, {
        "vec": [{"str": "isa"}, {"str": "socrates"}, {"var": "?x"}]
    });
    let d2 = from_json!(Datum, {
        "vec": [{"str": "isa"}, {"str": "socrates"}, {"str": "man"}]
    });
    let bindings = Bindings::new().set_binding(&"?x".to_string(), Datum::String("man".to_string()));
    let actual_bindings = d2.unify(&d, &bindings);
    assert_some_value!(actual_bindings, bindings);
}

#[test]
fn test_unify_fails_when_bindings_conflict() {
    let d = from_json!(Datum, {
        "vec": [{"str": "isa"}, {"str": "socrates"}, {"var": "?x"}]
    });
    let d2 = from_json!(Datum, {
        "vec": [{"str": "isa"}, {"str": "socrates"}, {"str": "man"}]
    });
    let bindings = Bindings::new().set_binding(&"?x".to_string(), Datum::String("mortal".to_string()));
    let actual_bindings = d.unify(&d2, &bindings);
    assert_none!(actual_bindings);
}

#[test]
fn test_unify_with_nesting() {
    let d = from_json!(Datum, {
        "vec": [
            {"str": "reward"},
            {"vec": [{"str": "value"}, {"int": 5}]},
            {"vec": [{"str": "time"}, {"int": 608356800}]},
            {"vec": [{"str": "type"}, {"str": "observation"}]}
        ]
    });
    let d2 = from_json!(Datum, {
        "vec": [
            {"str": "reward"},
            {"vec": [{"str": "value"}, {"var": "?rv"}]},
            {"vec": [{"str": "time"}, {"var": "?t"}]},
            {"vec": [{"str": "type"}, {"str": "observation"}]}
        ]
    });
    let expected_bindings = Bindings::new()
        .set_binding(&"?t".to_string(), Datum::Int(608356800))
        .set_binding(&"?rv".to_string(), Datum::Int(5));
    assert_eq!(d.unify(&d2, &Bindings::new()), Some(expected_bindings));
}

#[test]
fn test_unify_fails_when_no_match() {
    let d = from_json!(Datum, {
        "vec": [{"str": "isa"}, {"str": "socrates"}, {"str": "mortal"}]
    });
    let d2 = from_json!(Datum, {
        "vec": [{"str": "isa"}, {"str": "socrates"}, {"str": "man"}]
    });
    let actual_bindings = d.unify(&d2, &Bindings::new());
    assert_none!(actual_bindings);
}
