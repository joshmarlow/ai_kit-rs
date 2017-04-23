use std::collections::BTreeMap;
use sexp;

use core::{Apply, Bindings, ToSexp};
use datum::Datum;
use infer::{InferenceEngine, Rule};
use pedigree::{InferenceChain, Origin};

#[test]
fn test_rule_to_sexp() {
    let r = Rule {
        lhs: vec![Datum::from_sexp_str("(has-feathers ?x)").unwrap()],
        rhs: Datum::from_sexp_str("(bird ?x)").unwrap(),
        constraints: Vec::new(),
    };
    let expected_sexp = sexp::parse("(defrule (((has-feathers ?x)) (bird ?x) ()))").ok().unwrap();
    assert_eq!(r.to_sexp(), expected_sexp);
}

#[test]
fn test_rule_from_sexp() {
    let sexp = sexp::parse("(defrule (((has-feathers ?x)) (bird ?x) ()))").ok().unwrap();
    let expected_r = Rule {
        lhs: vec![Datum::from_sexp_str("(has-feathers ?x)").unwrap()],
        rhs: Datum::from_sexp_str("(bird ?x)").unwrap(),
        constraints: Vec::new(),
    };
    assert_eq!(Rule::from_sexp(&sexp).unwrap(), expected_r);
}

#[test]
fn test_snowflake() {
    let rule: Rule<Datum, Datum> = Rule::from_sexp_str("(defrule (((x ?x)) (y ?y) ((constraint-set (?diff \
                                                                      25.0)) (constraint-sum (?x ?y ?diff)))))")
        .unwrap();
    let expected_snowflake: Rule<Datum, Datum> = Rule::from_sexp_str("(defrule (((x ?x::test)) (y ?y::test) ((constraint-set \
                                                                      (?diff::test 25.0)) (constraint-sum (?x::test ?y::test \
                                                                      ?diff::test)))))")
        .unwrap();

    assert_eq!(rule.snowflake("test".to_string()), expected_snowflake);
}

#[test]
fn test_rule_application() {
    let rule = Rule::from_sexp_str("(defrule (((x ?x)) (y ?y) ((constraint-set (?diff 25.0)) (constraint-sum (?x ?y \
                                    ?diff)))))")
        .unwrap();
    let initial_datum = Datum::from_sexp_str("(x 10.0)").unwrap();
    let expected_datum = Datum::from_sexp_str("(y 15.0)").unwrap();
    let expected_bindings: Bindings<Datum> = vec![("?diff".to_string(), Datum::from_float(25.0)),
                                                  ("?x".to_string(), Datum::from_float(10.0)),
                                                  ("?y".to_string(), Datum::from_float(15.0))]
        .into_iter()
        .collect();
    assert_eq!(rule.apply(&vec![&initial_datum], &Bindings::new()),
               Some((expected_datum, expected_bindings)));
}

#[test]
fn test_rule_application_with_no_antecedents() {
    let rule: Rule<Datum, Datum> = Rule::from_sexp_str("(defrule (() (y 1) ()))").unwrap();
    let expected_datum = Datum::from_sexp_str("(y 1)").unwrap();
    let expected_bindings: Bindings<Datum> = Bindings::new();
    assert_eq!(rule.apply(&vec![], &Bindings::new()),
               Some((expected_datum, expected_bindings)));
}

#[test]
fn test_rule_reverse_application() {
    let rule = Rule::from_sexp_str("(defrule (((x ?x)) (y ?y) ((constraint-set (?diff 25.0)) (constraint-sum (?x ?y \
                                    ?diff)))))")
        .unwrap();
    let expected_datum = Datum::from_sexp_str("(x 10.0)").unwrap();
    let initial_datum = Datum::from_sexp_str("(y 15.0)").unwrap();
    let expected_bindings: Bindings<Datum> = vec![("?diff".to_string(), Datum::from_float(25.0)),
                                                  ("?x".to_string(), Datum::from_float(10.0)),
                                                  ("?y".to_string(), Datum::from_float(15.0))]
        .into_iter()
        .collect();
    assert_eq!(rule.r_apply(&initial_datum, &Bindings::new()),
               Some((vec![expected_datum], expected_bindings)));
}

#[test]
fn test_forward_chain() {
    let r_id = "rule-0".to_string();
    let r = Rule::from_sexp_str("(defrule (((has-feathers ?x)) (bird ?x) ()))").unwrap();
    let rules: BTreeMap<&String, &Rule<Datum, Datum>> = vec![(&r_id, &r)]
        .into_iter()
        .collect();

    let f_id = "fact-0".to_string();
    let f = Datum::from_sexp_str("(has-feathers bonnie)").unwrap();
    let facts: BTreeMap<&String, &Datum> = vec![(&f_id, &f)]
        .into_iter()
        .collect();

    let mut engine = InferenceEngine::new("test".to_string(), rules, facts);
    let new_facts = engine.chain_forward();

    let expected_new_fact = Datum::from_sexp_str("(bird bonnie)").unwrap();
    let expected_bindings = Bindings::new().set_binding(&"?x".to_string(), Datum::from_string(&"bonnie".to_string()));

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
    let rules = vec![Rule::from_sexp_str("(defrule (((current-value ?x)) (current-value ?y) ((constraint-set (?diff 1)) \
                                          (constraint-sum (?x ?diff ?y)))))")
                         .unwrap(),
                     Rule::from_sexp_str("(defrule (((current-value ?x)) (current-value ?y) ((constraint-set (?diff -1)) \
                                          (constraint-sum (?x ?diff ?y)))))")
                         .unwrap(),
                     Rule::from_sexp_str("(defrule (((current-value ?x)) (current-value ?y) ((constraint-set (?factor 2)) \
                                          (constraint-mul (?x ?factor ?y)))))")
                         .unwrap(),
                     Rule::from_sexp_str("(defrule (((current-value ?x)) (current-value ?y) ((constraint-set (?factor 0.5)) \
                                          (constraint-mul (?x ?factor ?y)))))")
                         .unwrap()];

    let rule_ids = vec!["add_one".to_string(), "subtract_one".to_string(), "double".to_string(), "halve".to_string()];

    let rules: BTreeMap<&String, &Rule<Datum, Datum>> = rule_ids.iter()
        .zip(rules.iter())
        .into_iter()
        .collect();

    let f_id = "fact-0".to_string();
    let f = Datum::from_sexp_str("(current-value 0)").unwrap();
    let facts: BTreeMap<&String, &Datum> = vec![(&f_id, &f)]
        .into_iter()
        .collect();

    let goal = Datum::from_sexp_str("(current-value 4)").unwrap();
    let engine = InferenceEngine::new("test".to_string(), rules, facts);
    let (result, _engine) = engine.chain_until_match(4, &goal);
    assert_eq!(result.is_some(), true);
    let (target_fact, _target_fact_id) = result.unwrap();
    assert_eq!(target_fact, goal);
}

#[test]
fn test_chain_until_match_updates_pedigree() {
    let rules = vec![Rule::from_sexp_str("(defrule (((current-value ?x)) (current-value ?y) ((constraint-set (?diff 1)) \
                                          (constraint-sum (?x ?diff ?y)))))")
                         .unwrap(),
                     Rule::from_sexp_str("(defrule (((current-value ?x)) (current-value ?y) ((constraint-set (?factor 2)) \
                                          (constraint-mul (?x ?factor ?y)))))")
                         .unwrap()];

    let add_one_id = "add_one".to_string();
    let double_id = "double".to_string();
    let rule_ids = vec![add_one_id.clone(), double_id.clone()];

    let rules: BTreeMap<&String, &Rule<Datum, Datum>> = rule_ids.iter()
        .zip(rules.iter())
        .into_iter()
        .collect();

    let f_id = "fact-0".to_string();
    let f = Datum::from_sexp_str("(current-value 0)").unwrap();
    let facts: BTreeMap<&String, &Datum> = vec![(&f_id, &f)]
        .into_iter()
        .collect();

    let goal = Datum::from_sexp_str("(current-value 4)").unwrap();
    let engine = InferenceEngine::new("test".to_string(), rules, facts);
    let (result, engine) = engine.chain_until_match(4, &goal);
    assert_eq!(result.is_some(), true);
    let (target_fact, target_fact_id) = result.unwrap();
    assert_eq!(target_fact, goal);

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
    assert_eq!(inference_chain, expected_inference_chain);
}
