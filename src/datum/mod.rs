use serde_json;
use std;
use std::str;
use std::collections::HashMap;

use constraints::ConstraintValue;
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
    #[serde(rename="vec")]
    Vector(Vec<Datum>),
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
}

impl Default for Datum {
    fn default() -> Self {
        Datum::Nil
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
            Datum::Vector(ref args) => {
                match *other {
                    Datum::Vector(ref args2) => args == args2,
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

impl Eq for Datum {}
impl Ord for Datum {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.partial_cmp(other).unwrap()
    }
}

impl core::BindingsValue for Datum {
    fn variable(&self) -> Option<String> {
        self.variable()
    }
}
impl ConstraintValue for Datum {
    fn to_float(&self) -> Option<f64> {
        self.float()
    }
    fn float(c: f64) -> Self {
        Datum::from_float(c)
    }
}

impl core::Unify<Datum> for Datum {
    fn unify(&self, other: &Datum, bindings: &core::Bindings<Self>) -> Option<core::Bindings<Self>> {

        fn unify_args(args: &Vec<Datum>, args2: &Vec<Datum>, bindings: &core::Bindings<Datum>) -> Option<core::Bindings<Datum>> {
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
            Datum::Vector(ref args) => {
                match *other {
                    Datum::Vector(ref args2) if args.len() == args2.len() => unify_args(args, args2, &bindings),
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
        fn apply_bindings_to_args(args: &Vec<Datum>, bindings: &core::Bindings<Datum>) -> Option<Vec<Datum>> {
            utils::filter_map_all(&mut args.iter(), &|arg| arg.apply_bindings(bindings))
        }
        match *self {
            Datum::Variable(ref var_name) => bindings.get_binding(var_name).or_else(|| Some(self.clone())),
            Datum::Compound { ref head, ref args } => {
                head.apply_bindings(bindings).and_then(|bound_head| {
                    apply_bindings_to_args(args, bindings).and_then(|bound_args| {
                        Some(Datum::Compound {
                            head: Box::new(bound_head),
                            args: bound_args,
                        })
                    })
                })
            }
            Datum::Vector(ref args) => apply_bindings_to_args(args, bindings).and_then(|args| Some(Datum::Vector(args))),
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
            Datum::Vector(ref args) => {
                let mut variables = Vec::new();
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
            Datum::Compound { ref head, ref args } => head.get_variable(q).or_else(|| args.iter().filter_map(|arg| arg.get_variable(q)).next()),
            Datum::Vector(ref args) => args.iter().filter_map(|arg| arg.get_variable(q)).next(),
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
            Datum::Vector(ref args) => Datum::Vector(args.iter().map(|arg| arg.rename_variables(renamed_variables)).collect()),
            _ => self.clone(),
        }
    }

    fn nil() -> Self {
        Datum::Nil
    }
}

impl std::fmt::Display for Datum {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", serde_json::to_string(&self).unwrap())
    }
}

macro_rules! datum_json {
    ($json: tt) => ({
        use serde_json;
        let d: Datum = serde_json::from_value(json!($json)).expect("Expected json decoding");
        d
    })
}

#[cfg(test)]
mod tests;
