

use core::Bindings;
use datum::Datum;
use infer::{chain_forward_with_negative_goals, InferenceEngine, Negatable, OriginCache};
use pedigree::{InferenceChain, Origin};
use rule::Rule;
use std::collections::BTreeMap;

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
          {"numerical": {"set": {"variable": "?diff", "constant": 1}}},
          {"numerical": {"sum": {"first": "?x", "second": "?diff", "third": "?y"}}}
        ]
      },
      {
        "lhs": [{"vec": [{"str": "current-value"}, {"var": "?x"}]}],
        "rhs": {"vec": [{"str": "current-value"}, {"var": "?y"}]},
        "constraints": [
          {"numerical": {"set": {"variable": "?diff", "constant": -1}}},
          {"numerical": {"sum": {"first": "?x", "second": "?diff", "third": "?y"}}}
        ]
      },
      {
        "lhs": [{"vec": [{"str": "current-value"}, {"var": "?x"}]}],
        "rhs": {"vec": [{"str": "current-value"}, {"var": "?y"}]},
        "constraints": [
          {"numerical": {"set": {"variable": "?factor", "constant": 2}}},
          {"numerical": {"mul": {"first": "?x", "second": "?factor", "third": "?y"}}}
        ]
      },
      {
        "lhs": [{"vec": [{"str": "current-value"}, {"var": "?x"}]}],
        "rhs": {"vec": [{"str": "current-value"}, {"var": "?y"}]},
        "constraints": [
          {"numerical": {"set": {"variable": "?factor", "constant": 0.5}}},
          {"numerical": {"mul": {"first": "?x", "second": "?factor", "third": "?y"}}}
        ]
      }
    ]);

    let rule_ids = vec!["add_one".to_string(), "subtract_one".to_string(), "double".to_string(), "halve".to_string()];

    let rules: BTreeMap<&String, &Rule<Datum, Datum>> = rule_ids.iter()
        .zip(rules.iter())
        .into_iter()
        .collect();

    let f_id = "fact-0".to_string();
    let f = from_json!(Datum, {"vec": [{"str": "current-value"}, {"float": 0.0}]});
    let facts: BTreeMap<&String, &Datum> = vec![(&f_id, &f)]
        .into_iter()
        .collect();

    let goal = from_json!(Datum, {"vec": [{"str": "current-value"}, {"float": 4.0}]});
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
            {"numerical": {"set": {"variable": "?diff", "constant": 1}}},
            {"numerical": {"sum": {"first": "?x", "second": "?diff", "third": "?y"}}}
        ]
      },
      {
        "lhs": [{"vec": [{"str": "current-value"}, {"var": "?x"}]}],
        "rhs": {"vec": [{"str": "current-value"}, {"var": "?y"}]},
        "constraints": [
            {"numerical": {"set": {"variable": "?factor", "constant": 2}}},
            {"numerical": {"mul": {"first": "?x", "second": "?factor", "third": "?y"}}}
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

#[test]
fn test_chain_forward_with_negative_goals() {
    let rules = from_json!(Vec<Rule<Datum, Negatable<Datum, Datum>>>, [
      {
        "lhs": [
          {"content": {"vec": [{"str": "current-value"}, {"var": "?x"}]}},
          {"content": {"vec": [{"str": "even-value"}, {"var": "?x"}]}, "is_negative": true},
        ],
        "rhs": {"content": {"vec": [{"str": "odd-value"}, {"var": "?x"}]}},
      },
    ]);
    let data = from_json!(Vec<Negatable<Datum, Datum>>, [
          {"content": {"vec": [{"str": "current-value"}, {"int": 0}]}},
          {"content": {"vec": [{"str": "even-value"}, {"int": 0}]}},
          {"content": {"vec": [{"str": "current-value"}, {"int": 1}]}},
          {"content": {"vec": [{"str": "current-value"}, {"int": 2}]}},
          {"content": {"vec": [{"str": "even-value"}, {"int": 2}]}},
          {"content": {"vec": [{"str": "current-value"}, {"int": 3}]}},
      ]);
    let expected_derived_facts = from_json!(Vec<Negatable<Datum, Datum>>, [
          {"content": {"vec": [{"str": "odd-value"}, {"int": 1}]}},
          {"content": {"vec": [{"str": "odd-value"}, {"int": 3}]}},
    ]);

    let data_ids = vec!["d0".to_string(), "d1".to_string(), "d2".to_string(), "d3".to_string(), "d4".to_string(), "d5".to_string()];
    let data_with_ids: Vec<(&String, &Negatable<Datum, Datum>)> = data_ids.iter()
        .zip(data.iter())
        .collect();
    let rule_ids = vec!["r0".to_string()];
    let rules_with_ids: Vec<(&String, &Rule<Datum, Negatable<Datum, Datum>>)> = rule_ids.iter().zip(rules.iter()).collect();
    let results = chain_forward_with_negative_goals(data_with_ids, rules_with_ids, &mut OriginCache::new());
    let derived_facts: Vec<Negatable<Datum, Datum>> = results.into_iter().map(|(f, _, _)| f).collect();
    assert_eq!(derived_facts, expected_derived_facts);
}
