[![Build Status](https://travis-ci.org/joshmarlow/ai_kit-rs.svg?branch=master)](https://travis-ci.org/joshmarlow/ai_kit-rs)

AI_Kit
======

AI_Kit aims to be a single dependency for various classic AI algorithms.

Core project goals are:

* convenient and ergonomic interfaces to various algorithms by building around traits.
* only build what you need through the use of feature flags
* performance
* easy to understand implementations

All of the algorithms (documented below) operate on several core traits, `BindingsValue`, `Unify`, `Operation`.

`ai_kit` provides [optional data structures](#default-trait-implementations) that implement these traits, allowing all algorithms to be usable out of the box - see [Datum](#datum) and [Rule](#rule).
Quick examples are provided before, followed by more in-depth documentation.

Installation
============

You can use this library by adding the following lines to your Cargo.toml file:

```
[dependencies]
ai_kit = "0.1.0"
```

and adding `extern crate ai_kit` to your crate root.

Documentation
=============

This README provides an introduction.

API Documentation is available [here](http://joshmarlow.github.io/ai_kit-rs/)

Core Concepts
=============

There are three traits and one structure to understand when using this library:

`Bindings` - similar to a key/value lookup, but with utilities for ensuring that two (or more) keys have the same value.

`BindingsValue` - a trait allowing a data structure to be used by the `Bindings` data structure.

`Unify` - a trait for data structure can be unified with another of the same type.

`Operation` - a trait for mapping some number of `Unify` instances to some number of other `Unify`s.
This is used for implementing [Forward](#forward-inference) and [Backward](#backward-inference) inferencing.


Unify
-----

Two data structures can be unified if all of their components are the same or at least of the fields that differ is a variable.  The [Datum](#datum) structure implements `Unify`.  [Here](#datum-implements-unify) is an example of unifying datums.

Bindings
--------

When successful, the unification process returns a `Bindings` structure, which maps variable names to
their values (when known).  It also allows for specifying that two variables are equivalent; in that case,
when the value for one variable is found, it is considerd the value for another.

Anything that implements `ai_kit::core::BindingsValue` can be used with `Bindings`; [Datum](#datum) implements `BindingsValue`:

```rust
// Example of using the ai_kit::datum::Datum for variable bindings.

extern crate ai_kit;

use ai_kit::core::Bindings;
use ai_kit::datum::Datum;

fn main() {
    // Create empty bindings
    let bindings : Bindings<Datum> = Bindings::new();

    // Set the variables "?x" and "?y" equal to each other
    let bindings = bindings
        .set_binding(&"?x".to_string(), Datum::Variable("?y".to_string()));

    // Set the value of "?x"
    let bindings = bindings.set_binding(&"?x".to_string(), Datum::Float(1.0));

    // Verify that "?y" now has the same value
    
    assert_eq!(bindings.get_binding(&"?x".to_string()), Some(Datum::Float(1.0)));
}
```

Operation
---------

There are times when a program has certain facts from which further facts can be inferred.
This is implemented by the `Operation` trait.  This is used to implement [forward inference](#forward-inference) and [planning](#planning).  An example of forward chaining reasoning (also called Modus Ponens), would the following:

```
All men are mortal.
Socrates is a man.
Therefore Socrates is mortal.
```

The [Rule](#rule) struct implements `Operation`, and we [use it to perform the above inference in rust](#rule-implements-operation).

Algorithms
==========

## Constraints

Feature `with-constraint`

A simple and limited library for checking and satisfying constraints.

## Forward Inference

Feature `with-forward-inference`

Implementation of forward-chaining inference - essentially this is inference via Modus Ponens.

[Example](#rule-implements-operation).

## Planning

Feature `with-planner`

Planning with backtracking.

[Example](#planning-examples)

## Pedigree

Misc data-structures and code for representing the path taken to derive a given inference.

## Default Trait Implementations

The above algorithms operate on any structures that implement the appropriate core traits (`BindingsValue`, `Unify` and `Operation`).

`ai_kit` provides default structures that implement the core traits which should be sufficient for many use-cases.

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

## Datum Implements Unify

Because `Datum` implements the `Unify` trait, `Datum` can be unified.


```rust

extern crate ai_kit;

use ai_kit::core::{Bindings, Unify};
use ai_kit::datum::Datum;

fn main() {
    let d = Datum::Float(0.0);
    let empty_bindings : Bindings<Datum> = Bindings::new();

    // These datums are the same, so they can be unified
    let bindings = d.unify(&Datum::Float(0.0), &empty_bindings);
    assert!(bindings.is_some());

    // These datums are not the same, so they cannot be unified
    let bindings = d.unify(&Datum::Float(1.0), &empty_bindings);
    assert!(bindings.is_none());
    
    // These datums differ, but the second is a variable, so they can be unified
    let bindings = d.unify(&Datum::Variable("?x".to_string()), &empty_bindings);
    assert!(bindings.is_some());

    // The bindings returned by unification so that the variable ?x now has the same value as d!
    assert_eq!(bindings.unwrap().get_binding(&"?x".to_string()), Some(d));
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

## Rule Implements Operation

```rust

extern crate ai_kit;

use ai_kit::datum::Datum;
use ai_kit::infer::InferenceEngine;
use ai_kit::rule::Rule;
use std::marker::PhantomData;

fn main() {
    // Encode knowledge about mortality
    let rules = vec![
        (
            "rule_of_mortality".to_string(),    // Rules need ids for inferencing
            Rule {
                constraints: Vec::new(),
                lhs: vec![
                    Datum::Vector(
                        vec![
                            Datum::Variable("?x".to_string()),
                            Datum::String("isa".to_string()),
                            Datum::String("human".to_string()),
                        ]
                    )
                ],
                rhs: Datum::Vector(vec![
                    Datum::Variable("?x".to_string()),
                    Datum::String("isa".to_string()),
                    Datum::String("mortal".to_string()),
                ]),
                _marker: PhantomData,
            }
        ),
    ];
    
    // Setup our initial knowledge about socrates
    let facts = vec![
        (
            "socrates_is_hu``man".to_string(),      // Facts need ids for inferencing
            Datum::Vector(
                vec![
                    Datum::String("socrates".to_string()),
                    Datum::String("isa".to_string()),
                    Datum::String("human".to_string()),
                ]
            )
        ),
    ];

    // Infer new knowledge!
    let mut inf_engine = InferenceEngine::new(
        "demo".to_string(),
        rules.iter().map(|&(ref id, ref f)| (id, f)).collect(),
        facts.iter().map(|&(ref id, ref r)| (id, r)).collect());
    let inferences = inf_engine.chain_forward();
    assert_eq!(inferences.len(), 1);
}
```

## Planning Examples

Sudoku Solver
=============

Forthcoming

N-Queens Solver
===============

Forthcoming


NLP Parser
==========

This example takes a bunch of words, and rules for aggregating words into phrases and sentences, and constructs a valid parse of the words.
It then saves a graph in [GraphViz .dot notation](http://www.graphviz.org/) of the actual goal tree constructed into a "parse.dot" in the current working directory.

```rust

extern crate ai_kit;
#[macro_use]
extern crate serde_json;

use ai_kit::core::Bindings;
use ai_kit::datum::Datum;
use ai_kit::planner::*;
use ai_kit::rule::Rule;
use std::fs::File;
use std::io::Write;
use std::path;

macro_rules! from_json {
    ($type: ty, $json: tt) => ({
        use serde_json;
        let x: $type = serde_json::from_value(json!($json)).expect("Expected json decoding");
        x
    })
}

#[allow(unused)]
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

    // Render the plan using graphviz notation
    let graphviz_rendering : String = final_goal.render_as_graphviz();

    // Save the plan in the current working directory
    File::create(path::Path::new(&"/Users/josh/parse.dot"))
        .and_then(|mut file| file.write_all(graphviz_rendering.as_str().as_bytes()));
}
```

Here is the expected content of "parse.dot":

```
graph "goal tree 'sen'" {

"'sen' [Actor(8)]" -- "'np' [Actor(6)]";
"'np' [Actor(6)]" -- "'det' [Actor(0)]";
"'det' [Actor(0)]" -- "'a' [Datum(0)]";

"'np' [Actor(6)]" -- "'noun' [Actor(4)]";
"'noun' [Actor(4)]" -- "'dog' [Datum(2)]";

"'sen' [Actor(8)]" -- "'vp' [Actor(7)]";
"'vp' [Actor(7)]" -- "'verb' [Actor(2)]";
"'verb' [Actor(2)]" -- "'chased' [Datum(4)]";

"'vp' [Actor(7)]" -- "'np' [Actor(6)]";
"'np' [Actor(6)]" -- "'det' [Actor(1)]";
"'det' [Actor(1)]" -- "'the' [Datum(1)]";

"'np' [Actor(6)]" -- "'noun' [Actor(5)]";
"'noun' [Actor(5)]" -- "'cat' [Datum(3)]";

}
```

If you have graphviz installed locally, you can convert this graph into a PNG file:

```
dot -Tpng parse.dot > parse.png
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


Skeptic Testing
===============

Examples in this document are tested as part of the build process using [skeptic](https://github.com/brson/rust-skeptic).
