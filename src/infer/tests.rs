use std::collections::BTreeMap;

use core::{Apply, Bindings};
use datum::Datum;
use infer::{InferenceEngine, Rule};
use pedigree::{InferenceChain, Origin};

fn setup() -> Rule<Datum, Datum> {
    from_json!(Rule<Datum, Datum>, {
      "lhs": [{"vec": [{"str": "x"}, {"var": "?x"}]}],
      "rhs": {"vec": [{"str": "y"}, {"var": "?y"}]},
      "constraints": [{"set": {"variable": "?diff", "constant": 25}},{"sum": {"first": "?x", "second": "?y", "third": "?diff"}}],
    })
}

#[test]
fn test_snowflake() {
    let rule: Rule<Datum, Datum> = setup();

    let expected_snowflake = from_json!(Rule<Datum, Datum>, {
      "lhs": [{"vec": [{"str": "x"}, {"var": "?x::test"}]}],
      "rhs": {"vec": [{"str": "y"}, {"var": "?y::test"}]},
      "constraints": [
        {"set": {"variable": "?diff::test", "constant": 25}},
        {"sum": {"first": "?x::test", "second": "?y::test", "third": "?diff::test"}}
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

#[test]
fn test_forward_chain() {
    let r_id = "rule-0".to_string();
    let r = from_json!(Rule<Datum, Datum>, {
      "lhs": [{"vec": [{"str": "has-features"}, {"var": "?x"}]}],
      "rhs": {"vec": [{"str": "bird"}, {"var": "?x"}]},
    });
    let rules: BTreeMap<&String, &Rule<Datum, Datum>> = vec![(&r_id, &r)]
        .into_iter()
        .collect();

    let f_id = "fact-0".to_string();
    let f = from_json!(Datum, {"vec": [{"str": "has-features"}, {"str": "bonnie"}]});
    let facts: BTreeMap<&String, &Datum> = vec![(&f_id, &f)]
        .into_iter()
        .collect();

    let mut engine = InferenceEngine::new("test".to_string(), rules, facts);
    let new_facts = engine.chain_forward();

    let expected_new_fact = from_json!(Datum, {"vec": [{"str": "bird"}, {"str": "bonnie"}]});
    let expected_bindings = Bindings::new().set_binding(&"?x".to_string(), Datum::String("bonnie".to_string()));

    assert_eq!(new_facts.len(), 1);
    assert_eq!(new_facts,
               vec![(expected_new_fact,
                     expected_bindings,
                     Origin {
                         source_id: "rule-0".to_string(),
                         args: vec!["fact-0".to_string()],
                     })]);
}

#[test]
fn test_chain_until_match() {
    let rules = from_json!(Vec<Rule<Datum, Datum>>, [
      {
        "lhs": [{"vec": [{"str": "current-value"}, {"var": "?x"}]}],
        "rhs": {"vec": [{"str": "current-value"}, {"var": "?y"}]},
        "constraints": [
          {"set": {"variable": "?diff", "constant": 1}},
          {"sum": {"first": "?x", "second": "?diff", "third": "?y"}}
        ]
      },
      {
        "lhs": [{"vec": [{"str": "current-value"}, {"var": "?x"}]}],
        "rhs": {"vec": [{"str": "current-value"}, {"var": "?y"}]},
        "constraints": [
          {"set": {"variable": "?diff", "constant": -1}},
          {"sum": {"first": "?x", "second": "?diff", "third": "?y"}}
        ]
      },
      {
        "lhs": [{"vec": [{"str": "current-value"}, {"var": "?x"}]}],
        "rhs": {"vec": [{"str": "current-value"}, {"var": "?y"}]},
        "constraints": [
          {"set": {"variable": "?factor", "constant": 2}},
          {"mul": {"first": "?x", "second": "?factor", "third": "?y"}}
        ]
      },
      {
        "lhs": [{"vec": [{"str": "current-value"}, {"var": "?x"}]}],
        "rhs": {"vec": [{"str": "current-value"}, {"var": "?y"}]},
        "constraints": [
          {"set": {"variable": "?factor", "constant": 0.5}},
          {"mul": {"first": "?x", "second": "?factor", "third": "?y"}}
        ]
      }
    ]);

    let rule_ids = vec!["add_one".to_string(), "subtract_one".to_string(), "double".to_string(), "halve".to_string()];

    let rules: BTreeMap<&String, &Rule<Datum, Datum>> = rule_ids.iter()
        .zip(rules.iter())
        .into_iter()
        .collect();

    let f_id = "fact-0".to_string();
    let f = from_json!(Datum, {"vec": [{"str": "current-value"}, {"float": 0}]});
    let facts: BTreeMap<&String, &Datum> = vec![(&f_id, &f)]
        .into_iter()
        .collect();

    let goal = from_json!(Datum, {"vec": [{"str": "current-value"}, {"float": 4}]});
    let engine = InferenceEngine::new("test".to_string(), rules, facts);
    let (result, _engine) = engine.chain_until_match(4, &goal);
    assert_eq!(result.is_some(), true);
    let (target_fact, _target_fact_id) = result.unwrap();
    assert_eq!(target_fact, goal);
}

#[test]
fn test_chain_until_match_updates_pedigree() {
    let rules = from_json!(Vec<Rule<Datum, Datum>>, [
      {
        "lhs": [{"vec": [{"str": "current-value"}, {"var": "?x"}]}],
        "rhs": {"vec": [{"str": "current-value"}, {"var": "?y"}]},
        "constraints": [
            {"set": {"variable": "?diff", "constant": 1}},
            {"sum": {"first": "?x", "second": "?diff", "third": "?y"}}
        ]
      },
      {
        "lhs": [{"vec": [{"str": "current-value"}, {"var": "?x"}]}],
        "rhs": {"vec": [{"str": "current-value"}, {"var": "?y"}]},
        "constraints": [
            {"set": {"variable": "?factor", "constant": 2}},
            {"mul": {"first": "?x", "second": "?factor", "third": "?y"}}
        ]
      }
    ]);

    let add_one_id = "add_one".to_string();
    let double_id = "double".to_string();
    let rule_ids = vec![add_one_id.clone(), double_id.clone()];

    let rules: BTreeMap<&String, &Rule<Datum, Datum>> = rule_ids.iter()
        .zip(rules.iter())
        .into_iter()
        .collect();

    let f_id = "fact-0".to_string();
    let f = from_json!(Datum, {"vec": [{"str": "current-value"}, {"float": 0}]});
    let facts: BTreeMap<&String, &Datum> = vec![(&f_id, &f)]
        .into_iter()
        .collect();

    let goal = from_json!(Datum, {"vec": [{"str": "current-value"}, {"float": 4}]});
    let engine = InferenceEngine::new("test".to_string(), rules, facts);
    let (result, engine) = engine.chain_until_match(4, &goal);
    assert_eq!(result.is_some(), true);
    let (target_fact, target_fact_id) = result.unwrap();
    assert_eq!(target_fact, goal);
    println!("\n");
    println!("Target fact: {}", target_fact);
    println!("Goal       : {}", goal);

    let test_id_0 = "test-0".to_string();
    let test_0_origin = Origin {
        source_id: "add_one".to_string(),
        args: vec!["fact-0".to_string()],
    };

    let test_id_2 = "test-2".to_string();
    let test_2_origin = Origin {
        source_id: "double".to_string(),
        args: vec!["test-0".to_string()],
    };

    let test_id_6 = "test-6".to_string();
    let test_6_origin = Origin {
        source_id: "double".to_string(),
        args: vec!["test-2".to_string()],
    };

    let inference_chain = engine.pedigree.extract_inference_chain(&target_fact_id);
    let expected_inference_chain = InferenceChain {
        elements: vec![(vec![(test_id_6.clone(), Some(test_6_origin.clone()))]),
                       (vec![(double_id.clone(), None), (test_id_2.clone(), Some(test_2_origin.clone()))]),
                       (vec![(double_id.clone(), None), (test_id_0.clone(), Some(test_0_origin.clone()))]),
                       (vec![(add_one_id.clone(), None), (f_id.clone(), None)])],
    };
    println!("Expected: {}", expected_inference_chain);
    println!("Actual:   {}", inference_chain);
    assert_eq!(inference_chain, expected_inference_chain);
}
