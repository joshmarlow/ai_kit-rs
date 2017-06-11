use std;
use std::str;
use std::collections::HashMap;

use sexp;

use core;
use utils;

#[derive(Clone, Debug, Serialize, Deserialize, PartialOrd)]
pub enum Datum {
    #[serde(rename="null")]
    Nil,
    #[serde(rename="str")]
    String(String),
    #[serde(rename="int")]
    Int(i64),
    #[serde(rename="float")]
    Float(f64),
    #[serde(rename="var")]
    Variable(String),
    #[serde(rename="compound")]
    Compound {
        #[serde(rename="head")]
        head: Box<Datum>,
        #[serde(rename="args")]
        args: Vec<Datum>,
    },
}

impl Datum {
    pub fn from_string(s: &String) -> Datum {
        Datum::String(s.clone())
    }

    pub fn from_int(i: i64) -> Datum {
        Datum::Int(i)
    }

    pub fn from_float(f: f64) -> Datum {
        Datum::Float(f)
    }

    pub fn from_variable(s: String) -> Datum {
        Datum::Variable(s)
    }

    pub fn is_compound(&self) -> bool {
        match *self {
            Datum::Compound { head: ref _head, args: ref _args } => true,
            _ => false,
        }
    }

    pub fn string(&self) -> Option<String> {
        match *self {
            Datum::String(ref value) => Some(value.clone()),
            _ => None,
        }
    }

    pub fn int(&self) -> Option<i64> {
        match *self {
            Datum::Int(ref value) => Some(value.clone()),
            _ => None,
        }
    }

    pub fn float(&self) -> Option<f64> {
        match *self {
            Datum::Float(ref f_value) => Some(f_value.clone()),
            Datum::Int(ref i_value) => Some((i_value.clone() as f64)),
            _ => None,
        }
    }

    pub fn variable(&self) -> Option<String> {
        match *self {
            Datum::Variable(ref value) => Some(value.clone()),
            _ => None,
        }
    }

    pub fn head<'a>(&'a self) -> Option<&'a Box<Datum>> {
        match *self {
            Datum::Compound { ref head, args: ref _args } => Some(head),
            _ => None,
        }
    }

    fn from_sexp(s: &sexp::Sexp) -> std::result::Result<Self, core::FromSexpError> {
        fn is_variable_name(s: &String) -> bool {
            match s.chars().next() {
                Some(val) if val == '?' => true,
                Some(_) => false,
                None => false,
            }
        }

        match *s {
            sexp::Sexp::Atom(ref atom) => {
                let x = match *atom {
                    sexp::Atom::S(ref s) if is_variable_name(&s) => Datum::Variable(s.clone()),
                    sexp::Atom::S(ref s) => Datum::String(s.clone()),
                    sexp::Atom::I(ref s) => Datum::Float(s.clone() as f64),
                    sexp::Atom::F(ref s) => Datum::Float(s.clone()),
                };
                Ok(x)
            }
            sexp::Sexp::List(ref elements) => {
                let mut element_iter = elements.iter();
                match element_iter.next() {
                    Some(head_sexp) => {
                        Datum::from_sexp(head_sexp).and_then(|head| {
                            let mut args: Vec<Datum> = Vec::new();
                            for arg_sexp in element_iter {
                                match Datum::from_sexp(arg_sexp) {
                                    Ok(arg) => args.push(arg),
                                    err => return err,
                                }
                            }
                            Ok(Datum::Compound {
                                head: Box::new(head),
                                args: args,
                            })
                        })
                    }
                    None => Ok(Datum::Nil),
                }
            }
        }
    }

    fn to_sexp(&self) -> sexp::Sexp {
        match *self {
            Datum::Nil => sexp::Sexp::List(Vec::new()),
            Datum::String(ref s) => sexp::Sexp::Atom(sexp::Atom::S(s.clone())),
            Datum::Variable(ref v) => sexp::Sexp::Atom(sexp::Atom::S(v.clone())),
            Datum::Int(ref i) => sexp::Sexp::Atom(sexp::Atom::I(i.clone())),
            Datum::Float(ref f) => sexp::Sexp::Atom(sexp::Atom::F(f.clone())),
            Datum::Compound { ref head, ref args } => {
                let mut vec: Vec<sexp::Sexp> = Vec::with_capacity(args.len() + 1 as usize);
                vec.push(Datum::to_sexp(head));
                for arg in args {
                    vec.push(Datum::to_sexp(arg));
                }
                sexp::Sexp::List(vec)
            }
        }
    }
}

impl PartialEq for Datum {
    fn eq(&self, other: &Datum) -> bool {
        match *self {
            Datum::String(ref s) => {
                match *other {
                    Datum::String(ref s2) => s == s2,
                    _ => false,
                }
            }
            Datum::Int(ref i) => {
                match *other {
                    Datum::Int(ref i2) => i == i2,
                    _ => false,
                }
            }
            Datum::Float(ref f) => {
                match *other {
                    Datum::Float(ref f2) => f == f2,
                    _ => false,
                }
            }
            Datum::Variable(ref v) => {
                match *other {
                    Datum::Variable(ref v2) => v == v2,
                    _ => false,
                }
            }
            Datum::Compound { ref head, ref args } => {
                match *other {
                    Datum::Compound { head: ref head2, args: ref args2 } => head == head2 && args == args2,
                    _ => false,
                }
            }
            Datum::Nil => {
                match *other {
                    Datum::Nil => true,
                    _ => false,
                }
            }
        }
    }
}

impl core::ToSexp for Datum {
    fn to_sexp(&self) -> sexp::Sexp {
        self.to_sexp()
    }
    fn from_sexp(s_exp: &sexp::Sexp) -> std::result::Result<Self, core::FromSexpError> {
        Datum::from_sexp(s_exp)
    }
}

impl Eq for Datum {}
impl Ord for Datum {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.partial_cmp(other).unwrap()
    }
}

impl core::BindingsValue for Datum {
    fn to_float(&self) -> Option<f64> {
        self.float()
    }

    fn from_float(f: f64) -> Self {
        Datum::from_float(f)
    }

    fn variable(&self) -> Option<String> {
        self.variable()
    }
}

impl core::Unify<Datum> for Datum {
    fn unify(&self, other: &Datum, bindings: &core::Bindings<Self>) -> Option<core::Bindings<Self>> {

        fn unify_args(args: &Vec<Datum>,
                      args2: &Vec<Datum>,
                      bindings: &core::Bindings<Datum>)
                      -> Option<core::Bindings<Datum>> {
            utils::fold_while_some(bindings.clone(),
                                   &mut args.iter().zip(args2.iter()),
                                   &|bindings: core::Bindings<Datum>, (ref a, ref b): (&Datum, &Datum)| a.unify(&b, &bindings))
        }
        match *self {
            Datum::Variable(ref var_name) => bindings.update_bindings(var_name, other),
            Datum::Compound { ref head, ref args } => {
                match *other {
                    Datum::Compound { head: ref head2, args: ref args2 } if args.len() == args2.len() => {
                        head.unify(&head2, bindings).and_then(|bindings| unify_args(args, args2, &bindings))
                    }
                    Datum::Variable(ref var_name) => bindings.update_bindings(var_name, self),
                    _ => None,
                }
            }
            _ => {
                match *other {
                    Datum::Variable(ref var_name) => bindings.update_bindings(var_name, self),
                    _ => {
                        if self == other {
                            Some(bindings.clone())
                        } else {
                            None
                        }
                    }
                }
            }
        }
    }

    fn apply_bindings(&self, bindings: &core::Bindings<Self>) -> Option<Self> {
        match *self {
            Datum::Variable(ref var_name) => bindings.get_binding(var_name).or_else(|| Some(self.clone())),
            Datum::Compound { ref head, ref args } => {
                head.apply_bindings(bindings).and_then(|bound_head| {
                    utils::filter_map_all(&mut args.iter(), &|arg| arg.apply_bindings(bindings)).and_then(|bound_args| {
                        Some(Datum::Compound {
                            head: Box::new(bound_head),
                            args: bound_args,
                        })
                    })
                })
            }
            _ => Some(self.clone()),
        }
    }

    fn variables(&self) -> Vec<String> {
        match *self {
            Datum::Variable(ref v) => vec![v.clone()],
            Datum::Compound { ref head, ref args } => {
                let mut variables = head.variables();
                for arg in args.iter() {
                    variables.extend(arg.variables().into_iter());
                }
                variables
            }
            _ => Vec::new(),
        }
    }

    fn get_variable(&self, q: &String) -> Option<&Self> {
        match *self {
            Datum::Variable(ref v) if v == q => Some(self),
            Datum::Compound { ref head, ref args } => {
                head.get_variable(q).or_else(|| args.iter().filter_map(|arg| arg.get_variable(q)).next())
            }
            _ => None,
        }
    }

    fn rename_variables(&self, renamed_variables: &HashMap<String, String>) -> Self {
        match *self {
            Datum::Variable(ref v) => {
                renamed_variables.get(v)
                    .and_then(|new_v| Some(Datum::Variable(new_v.clone())))
                    .or_else(|| Some(self.clone()))
                    .unwrap()
            }
            Datum::Compound { ref head, ref args } => {
                Datum::Compound {
                    head: Box::new(head.rename_variables(renamed_variables)),
                    args: args.iter().map(|arg| arg.rename_variables(renamed_variables)).collect(),
                }
            }
            _ => self.clone(),
        }
    }

    fn nil() -> Self {
        Datum::Nil
    }
}

impl std::fmt::Display for Datum {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.to_sexp())
    }
}

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

#[cfg(test)]
mod tests {
    use std::str;
    use super::*;
    use super::super::core::{Bindings, ToSexp, Unify};
    extern crate serde_json;
    extern crate serde_yaml;

    #[test]
    fn test_round_trip_json_serialization() {
        let s = r#"{"compound":{"head":{"str":"isa"},"args":[{"str":"man"},{"str":"mortal"}]}}"#;

        let d: Datum = serde_json::from_str(&s).unwrap();
        let sd = serde_json::to_string(&d).unwrap();
        assert_eq!(sd, s);
    }

    #[test]
    fn test_round_trip_yaml_serialization() {
        let s = r#"---
compound:
  head:
    str: isa
  args:
    -
      str: man
    -
      str: mortal"#;

        let d: Datum = serde_yaml::from_str(&s).unwrap();
        let sd = serde_yaml::to_string(&d).unwrap();
        let d2: Datum = serde_yaml::from_str(&sd).unwrap();
        assert_eq!(d, d2);
    }

    #[test]
    fn test_round_trip_serialization() {
        let s = "(isa man mortal)";
        assert_eq!(format!("{}", Datum::from_sexp_str(&s).unwrap()), s);
    }

    #[test]
    fn test_parse_empty_string() {
        assert_eq!(Datum::from_sexp_str("").is_err(), true);
    }

    #[test]
    fn test_parse_nil() {
        assert_eq!(Datum::from_sexp_str("()").ok(), Some(Datum::Nil));
    }

    #[test]
    fn test_from_str_error_on_malformed_input() {
        assert_eq!(Datum::from_sexp_str(&"(").is_err(), true)
    }

    #[test]
    fn test_unify_passes_when_exact_match() {
        let d = Datum::from_sexp_str("(isa socrates man)").unwrap();
        let bindings = d.unify(&d, &Bindings::new());
        assert_some_value!(bindings, Bindings::new());
    }

    #[test]
    fn test_unify_passes_when_variables_match() {
        let d = Datum::from_sexp_str("(action 1 ?::t1)").unwrap();
        let bindings = Bindings::new().set_binding(&"?::t1".to_string(), Datum::from_float(2.0));
        assert_eq!(d.unify(&d, &bindings), Some(bindings));
    }

    #[test]
    fn test_unify_passes_when_match_with_new_variable_in_self() {
        let d = Datum::from_sexp_str("(isa socrates ?x)").unwrap();
        let d2 = Datum::from_sexp_str("(isa socrates man)").unwrap();
        let expected_bindings: Bindings<Datum> =
            vec![("?x".to_string(), Datum::from_sexp_str("man").unwrap())].into_iter().collect();
        let actual_bindings = d.unify(&d2, &Bindings::new());
        assert_some_value!(actual_bindings, expected_bindings);
    }

    #[test]
    fn test_unify_passes_when_match_with_new_variable_in_other() {
        let d = Datum::from_sexp_str("(isa socrates man)").unwrap();
        let d2 = Datum::from_sexp_str("(isa socrates ?x)").unwrap();
        let expected_bindings: Bindings<Datum> =
            vec![("?x".to_string(), Datum::from_sexp_str("man").unwrap())].into_iter().collect();
        let actual_bindings = d.unify(&d2, &Bindings::new());
        assert_some_value!(actual_bindings, expected_bindings);
    }

    #[test]
    fn test_unify_passes_when_bindings_in_self_match() {
        let d = Datum::from_sexp_str("(isa socrates ?x)").unwrap();
        let d2 = Datum::from_sexp_str("(isa socrates man)").unwrap();
        let bindings: Bindings<Datum> = vec![("?x".to_string(), Datum::from_sexp_str("man").unwrap())].into_iter().collect();
        let actual_bindings = d.unify(&d2, &bindings);
        assert_some_value!(actual_bindings, bindings);
    }

    #[test]
    fn test_unify_passes_when_bindings_in_other_match() {
        let d = Datum::from_sexp_str("(isa socrates man)").unwrap();
        let d2 = Datum::from_sexp_str("(isa socrates ?x)").unwrap();
        let bindings: Bindings<Datum> = vec![("?x".to_string(), Datum::from_sexp_str("man").unwrap())].into_iter().collect();
        let actual_bindings = d.unify(&d2, &bindings);
        assert_some_value!(actual_bindings, bindings);
    }

    #[test]
    fn test_unify_fails_when_bindings_conflict() {
        let d = Datum::from_sexp_str("(isa socrates ?x)").unwrap();
        let d2 = Datum::from_sexp_str("(isa socrates man)").unwrap();
        let bindings: Bindings<Datum> = vec![("?x".to_string(), Datum::from_sexp_str("mortal").unwrap())].into_iter().collect();
        let actual_bindings = d.unify(&d2, &bindings);
        assert_none!(actual_bindings);
    }

    #[test]
    fn test_unify_with_nesting() {
        let d = Datum::from_sexp_str("(reward (value 5) (time 608356800) (type observation))").unwrap();
        let d2 = Datum::from_sexp_str("(reward (value ?rv) (time ?t) (type observation))").unwrap();
        let expected_bindings = vec![("?t".to_string(), Datum::from_sexp_str("608356800").unwrap()),
                                     ("?rv".to_string(), Datum::from_sexp_str("5").unwrap())]
            .into_iter()
            .collect();
        assert_eq!(Some(expected_bindings), d.unify(&d2, &Bindings::new()));
    }

    #[test]
    fn test_unify_fails_when_no_match() {
        let d = Datum::from_sexp_str("(isa socrates mortal)").unwrap();
        let d2 = Datum::from_sexp_str("(isa socrates man)").unwrap();
        let actual_bindings = d.unify(&d2, &Bindings::new());
        assert_none!(actual_bindings);
    }

    #[test]
    fn test_unify_fails_when_arg_counts_differ() {
        let d = Datum::from_sexp_str("(isa socrates mortal)").unwrap();
        let d2 = Datum::from_sexp_str("(isa socrates)").unwrap();
        let actual_bindings = d.unify(&d2, &Bindings::new());
        assert_none!(actual_bindings);
    }
}
