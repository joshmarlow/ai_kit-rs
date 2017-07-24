[![Build Status](https://travis-ci.org/joshmarlow/ai_kit-rs.svg?branch=master)](https://travis-ci.org/joshmarlow/ai_kit-rs)

AI_Kit
======

AI_Kit aims to be a single dependency for various clssic AI algorithms.

Core project goals are:
    - use a few core traits that all algorithms operate on
    - convenient and ergonomic interfaces to various algorithms
    - easy to understand implementations
    - only build what you need through the use of feature flags

All of the algorithms (documented below) operate on several core traits, `BindingsValue`, `Unify`, `Operation`.
`ai_kit` provides [optional data structures](#default-trait-implementations) that implement these traits, allowing all algorithms to be usable out of the box - see [Datum](#datum) and [Rule](#rule).
Quick examples are provided before, followed by more in-depth documentation.

Quick examples
==============

Sudoku Solver
=============

NLP Parser
==========

This example takes a bunch of words, and rules for aggregating words into phrases and sentences, and constructs a valid parse of the words.

```rust
extern crate ai_kit;
use ai_kit::core::Bindings;
use ai_kit::datum::Datum;
use ai_kit::planner::*;
use ai_kit::rule::Rule;

macro_rules! from_json {
    ($type: ty, $json: tt) => ({
        use serde_json;
        let x: $type = serde_json::from_value(json!($json)).expect("Expected json decoding");
        x
    })
}

fn main() {
    /*
     * Inference rules that encode the parts of speech for each word and how to
     * compose parts of speech into sentences.
     */
    let rules: Vec<Rule<Datum, Datum>> = from_json!(Vec<Rule<Datum, Datum>>, [
          // Parts of speech for each word
          {"lhs": [{"str": "a"}], "rhs": {"str": "det"}},
          {"lhs": [{"str": "the"}], "rhs": {"str": "det"}},
          {"lhs": [{"str": "chased"}], "rhs": {"str": "verb"}},
          {"lhs": [{"str": "chased"}], "rhs": {"str": "verb"}},
          {"lhs": [{"str": "dog"}], "rhs": {"str": "noun"}},
          {"lhs": [{"str": "cat"}], "rhs": {"str": "noun"}},
          // Building phrases into sentences
          {"lhs": [{"str": "det"}, {"str": "noun"}], "rhs": {"str": "np"}},
          {"lhs": [{"str": "verb"}, {"str": "np"}], "rhs": {"str": "vp"}},
          {"lhs": [{"str": "np"}, {"str": "vp"}], "rhs": {"str": "sen"}}
        ]);

    // Our input data - a series of words
    let data: Vec<Datum> = from_json!(Vec<Datum>, [
          {"str": "a"},
          {"str": "the"},
          {"str": "dog"},
          {"str": "cat"},
          {"str": "chased"}
        ]);

    // Specify that our goal is to construct a sentence from the provided data using the provided rules
    let mut planner = Planner::new(&Goal::with_pattern(from_json!(Datum, {"str": "sen"})),
                                   &Bindings::new(),
                                   &PlanningConfig {
                                       max_depth: 5,
                                       max_increments: 50,
                                       // Don't reuse a given piece of data (ie, a word)
                                       reuse_data: false,
                                   },
                                   data.iter().collect(),
                                   rules.iter().collect());

    // Construct the first interpretation
    let result = planner.next();
    assert_eq!(result.is_some(), true);
    let (final_goal, bindings) = result.unwrap();

    // What are our expected leaves of the goal (ie, the order of parsed sentences)
    let expected_leaves: Vec<Datum> = vec![
    	"a".to_string(),
    	"dog".to_string(),
    	"chased".to_string(),
    	"the".to_string(),
    	"cat".to_string()
    ]
        .into_iter()
        .map(|s| Datum::String(s))
        .collect();

    // Verify that the leaves of our plan are as expected
    assert_eq!(final_goal.gather_leaves(&bindings), expected_leaves);
}
```

Core
====

`Bindings` - similar to a key/value lookup, but with utilities for ensuring that two (or more) keys have the same value.

`BindingsValue` - a trait allowing a data structure to be used by the `Bindings` data structure.

`Unify` - a trait for data structure can be unified with another of the same type.

`Operation` - a trait for mapping some number of `Unify` instances to some number of other `Unify`s.
This is used for implementing [Forward](#forward-inference) and [Backward](#backward-inference) inferencing.

Algorithms
==========

## Constraints

Feature `with-constraint`

A simple and limited library for checking and satisfying constraints.

## Forward Inference

Feature `with-forward-inference`

Implementation of forward-chaining inference - essentially this is inference via Modus Ponens.

## Backward Inference

Feature `with-planner`

Backward chaining inference with backtracking.

## Pedigree

Misc data-structures and code for representing the path taken to derive a given inference.

## Default Trait Implementations

The above algorithms operate on any structure that implements the requisite core traits (`BindingsValue, `Unify` and `Operation`).

`ai_kit` provides default structures that implement the core traits which should be sufficient ofr many use-cases.

## Datum

Feature `with-datum`.

The `datum::Datum` structure implements the `BindingsValue` and `Unify` traits.

```
	#[derive(Clone, Debug, Serialize, Deserialize, PartialOrd)]
	pub enum Datum {
	    Nil,
	    String(String),
	    Int(i64),
	    Float(f64),
	    Variable(String),
	    Vector(Vec<Datum>),
	}
```

## Rule

Feature `with-rule`.

The `rule::Rule` structure implements the `Operation` trait.

```
	#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
	pub struct Rule<T: ConstraintValue, U: Unify<T>> {
	    pub constraints: Vec<Constraint>,
	    pub lhs: Vec<U>,
	    pub rhs: U,
	    _marker: PhantomData<T>,
	}
```

Feature Matrix
==============

Some features depend on other features.  This is summarized in the following table:

| Feature | Requires |
|---------|----------|
| `with-planner`           | `with-constraint` |
| `with-forward-inference` | `with-planner` `with-constraint` |
| `with-rule`              | `with-constraint` |
| `with-constraint`        | N/A|
| `with-pedigree`          | N/A |
| `with-datum`             | N/A |