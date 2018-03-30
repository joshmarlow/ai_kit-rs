//! The datum module provides a data structure, Datum, that implements the Unify trait.
//! Datum aims to be a drop-in for any algorithm in ai_kit that operates on the Unify trait.

#[cfg(feature = "with-constraint")]
use constraints::ConstraintValue;
use core;
use std;
use std::collections::{BTreeMap, HashMap};
use std::str;
use utils;

#[derive(Clone, Debug, Serialize, Deserialize, PartialOrd)]
pub enum Datum {
    #[serde(rename = "null")] Nil,
    #[serde(rename = "bool")] Bool(bool),
    #[serde(rename = "str")] String(String),
    #[serde(rename = "int")] Int(i64),
    #[serde(rename = "float")] Float(f64),
    #[serde(rename = "var")] Variable(String),
    #[serde(rename = "vec")] Vector(Vec<Datum>),
    #[serde(rename = "map")] Map(BTreeMap<String, Datum>),
    #[serde(rename = "fn")] Function { head: Box<Datum>, args: Vec<Datum> },
}

impl Datum {
    pub fn to_bool(&self) -> Option<bool> {
        match *self {
            Datum::Bool(ref value) => Some(value.clone()),
            _ => None,
        }
    }

    pub fn to_string(&self) -> Option<String> {
        match *self {
            Datum::String(ref value) => Some(value.clone()),
            _ => None,
        }
    }

    pub fn to_int(&self) -> Option<i64> {
        match *self {
            Datum::Int(ref value) => Some(value.clone()),
            _ => None,
        }
    }

    pub fn to_float(&self) -> Option<f64> {
        match *self {
            Datum::Float(ref f_value) => Some(f_value.clone()),
            Datum::Int(ref i_value) => Some(i_value.clone() as f64),
            _ => None,
        }
    }

    pub fn to_variable(&self) -> Option<String> {
        match *self {
            Datum::Variable(ref value) => Some(value.clone()),
            _ => None,
        }
    }

    pub fn function_head<'a>(&'a self) -> Option<&'a Box<Self>> {
        match *self {
            Datum::Function { ref head, .. } => Some(head),
            _ => None,
        }
    }

    pub fn function_args<'a>(&'a self) -> Option<&'a Vec<Self>> {
        match *self {
            Datum::Function { ref args, .. } => Some(args),
            _ => None,
        }
    }

    pub fn pprint(&self) -> String {
        match *self {
            Datum::Nil => format!("nil"),
            Datum::Bool(ref b) => format!("{}", b),
            Datum::String(ref s) => format!("{}", s),
            Datum::Int(ref i) => format!("{}", i),
            Datum::Float(ref f) => format!("{}", f),
            Datum::Variable(ref v) => format!("{}", v),
            Datum::Vector(ref args) => {
                let elements: Vec<String> = args.iter().map(|e| e.pprint()).collect();
                format!("({})", elements.join(","))
            }
            Datum::Map(ref args) => {
                let elements: Vec<String> = args.iter()
                    .map(|(k, v)| format!("{} => {}", k, v.pprint()))
                    .collect();
                format!("({})", elements.join(","))
            }
            Datum::Function { ref head, ref args } => {
                let elements: Vec<String> = args.iter().map(|e| e.pprint()).collect();
                format!("({} ({}))", head, elements.join(","))
            }
        }
    }

    pub fn is_nil(&self) -> bool {
        match *self {
            Datum::Nil => true,
            _ => false,
        }
    }

    pub fn is_bool(&self) -> bool {
        match *self {
            Datum::Bool(_) => true,
            _ => false,
        }
    }

    pub fn is_int(&self) -> bool {
        match *self {
            Datum::Int(_) => true,
            _ => false,
        }
    }

    pub fn is_float(&self) -> bool {
        match *self {
            Datum::Float(_) => true,
            _ => false,
        }
    }

    pub fn is_string(&self) -> bool {
        match *self {
            Datum::String(_) => true,
            _ => false,
        }
    }

    pub fn is_variable(&self) -> bool {
        match *self {
            Datum::Variable(_) => true,
            _ => false,
        }
    }

    pub fn is_vector(&self) -> bool {
        match *self {
            Datum::Vector(_) => true,
            _ => false,
        }
    }

    pub fn is_map(&self) -> bool {
        match *self {
            Datum::Map { .. } => true,
            _ => false,
        }
    }

    pub fn is_function(&self) -> bool {
        match *self {
            Datum::Function { .. } => true,
            _ => false,
        }
    }

    pub fn has_same_shape_as(&self, other: &Datum) -> bool {
        match (self, other) {
            (&Datum::Nil, &Datum::Nil) => true,
            (&Datum::Bool(_), &Datum::Bool(_)) => true,
            (&Datum::Int(_), &Datum::Int(_)) => true,
            (&Datum::Float(_), &Datum::Float(_)) => true,
            (&Datum::String(_), &Datum::String(_)) => true,
            (&Datum::Variable(_), &Datum::Variable(_)) => true,
            (&Datum::Vector(ref x), &Datum::Vector(ref y)) => x.len() == y.len(),
            (&Datum::Map(ref x), &Datum::Map(ref y)) => x.len() == y.len(),
            (
                &Datum::Function { ref head, ref args },
                &Datum::Function {
                    head: ref head2,
                    args: ref args2,
                },
            ) => {
                head.has_same_shape_as(head2)
                    && args.iter()
                        .zip(args2.iter())
                        .all(|(x, y)| x.has_same_shape_as(y))
            }
            _ => false,
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
            Datum::Nil => match *other {
                Datum::Nil => true,
                _ => false,
            },
            Datum::Bool(b) => match *other {
                Datum::Bool(b2) => b == b2,
                _ => false,
            },
            Datum::String(ref s) => match *other {
                Datum::String(ref s2) => s == s2,
                _ => false,
            },
            Datum::Int(ref i) => match *other {
                Datum::Int(ref i2) => i == i2,
                _ => false,
            },
            Datum::Float(ref f) => match *other {
                Datum::Float(ref f2) => f == f2,
                _ => false,
            },
            Datum::Variable(ref v) => match *other {
                Datum::Variable(ref v2) => v == v2,
                _ => false,
            },
            Datum::Vector(ref args) => match *other {
                Datum::Vector(ref args2) => args == args2,
                _ => false,
            },
            Datum::Map(ref args) => match *other {
                Datum::Map(ref args2) => args == args2,
                _ => false,
            },
            Datum::Function { ref head, ref args } => match *other {
                Datum::Function {
                    head: ref head2,
                    args: ref args2,
                } if args.len() == args2.len() =>
                {
                    head == head2 && args == args2
                }
                _ => false,
            },
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
    fn variable(s: &String) -> Option<Self> {
        Some(Datum::Variable(s.clone()))
    }
    fn to_variable(&self) -> Option<String> {
        self.to_variable()
    }
}

#[cfg(feature = "with-constraint")]
impl ConstraintValue for Datum {
    fn to_float(&self) -> Option<f64> {
        self.to_float()
    }
    fn float(c: f64) -> Self {
        Datum::Float(c)
    }
}

impl core::Unify<Datum> for Datum {
    fn unify(&self, other: &Datum, bindings: &core::Bindings<Self>) -> Option<core::Bindings<Self>> {
        fn unify_args(args: &Vec<Datum>, args2: &Vec<Datum>, bindings: &core::Bindings<Datum>) -> Option<core::Bindings<Datum>> {
            utils::fold_while_some(
                bindings.clone(),
                &mut args.iter().zip(args2.iter()),
                &|bindings: core::Bindings<Datum>, (ref a, ref b): (&Datum, &Datum)| a.unify(&b, &bindings),
            )
        }

        fn unify_maps(
            args: &BTreeMap<String, Datum>,
            args2: &BTreeMap<String, Datum>,
            bindings: &core::Bindings<Datum>,
        ) -> Option<core::Bindings<Datum>> {
            if args.len() != args2.len() {
                None
            } else {
                utils::fold_while_some(
                    bindings.clone(),
                    &mut args.iter(),
                    &|bindings: core::Bindings<Datum>, (ref k, ref v): (&String, &Datum)| args2.get(*k).and_then(|v2| v.unify(v2, &bindings)),
                )
            }
        }
        match *self {
            Datum::Variable(ref var_name) => bindings.update_bindings(var_name, other),
            Datum::Vector(ref args) => match *other {
                Datum::Vector(ref args2) if args.len() == args2.len() => unify_args(args, args2, &bindings),
                _ => None,
            },
            Datum::Map(ref args) => match *other {
                Datum::Map(ref args2) if args.len() == args2.len() => unify_maps(args, args2, &bindings),
                _ => None,
            },
            _ => match *other {
                Datum::Variable(ref var_name) => bindings.update_bindings(var_name, self),
                _ => {
                    if self == other {
                        Some(bindings.clone())
                    } else {
                        None
                    }
                }
            },
        }
    }

    fn apply_bindings(&self, bindings: &core::Bindings<Self>) -> Option<Self> {
        fn apply_bindings_to_args(args: &Vec<Datum>, bindings: &core::Bindings<Datum>) -> Option<Vec<Datum>> {
            utils::map_while_some(&mut args.iter(), &|arg| {
                arg.apply_bindings(bindings)
            })
        }
        fn apply_bindings_to_map(args: &BTreeMap<String, Datum>, bindings: &core::Bindings<Datum>) -> Option<BTreeMap<String, Datum>> {
            utils::map_while_some(&mut args.iter(), &|(k, v)| {
                v.apply_bindings(bindings)
                    .and_then(|v| Some((k.clone(), v)))
            }).and_then(|tuple_vec| Some(tuple_vec.into_iter().collect::<BTreeMap<String, Datum>>()))
        }
        match *self {
            Datum::Variable(ref var_name) => bindings
                .get_binding(var_name)
                .or_else(|| Some(self.clone())),
            Datum::Vector(ref args) => apply_bindings_to_args(args, bindings).and_then(|args| Some(Datum::Vector(args))),
            Datum::Map(ref args) => apply_bindings_to_map(args, bindings).and_then(|args| Some(Datum::Map(args))),
            Datum::Function { ref head, ref args } => head.apply_bindings(bindings).and_then(|head| {
                apply_bindings_to_args(args, bindings).map(|args| Datum::Function {
                    head: Box::new(head),
                    args: args,
                })
            }),
            _ => Some(self.clone()),
        }
    }

    fn variables(&self) -> Vec<String> {
        match *self {
            Datum::Variable(ref v) => vec![v.clone()],
            Datum::Vector(ref args) => {
                let mut variables = Vec::new();
                for arg in args.iter() {
                    variables.extend(arg.variables().into_iter());
                }
                variables
            }
            Datum::Map(ref args) => {
                let mut variables = Vec::new();
                for v in args.values() {
                    variables.extend(v.variables().into_iter());
                }
                variables
            }
            _ => Vec::new(),
        }
    }

    fn rename_variables(&self, renamed_variables: &HashMap<String, String>) -> Self {
        match *self {
            Datum::Variable(ref v) => renamed_variables
                .get(v)
                .and_then(|new_v| Some(Datum::Variable(new_v.clone())))
                .or_else(|| Some(self.clone()))
                .unwrap(),
            Datum::Vector(ref args) => Datum::Vector(
                args.iter()
                    .map(|arg| arg.rename_variables(renamed_variables))
                    .collect(),
            ),
            Datum::Map(ref args) => Datum::Map(
                args.iter()
                    .map(|(k, v)| (k.clone(), v.rename_variables(renamed_variables)))
                    .collect(),
            ),
            _ => self.clone(),
        }
    }

    fn nil() -> Self {
        Datum::Nil
    }
}

impl std::fmt::Display for Datum {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.pprint())
    }
}

#[cfg(test)]
mod tests;
