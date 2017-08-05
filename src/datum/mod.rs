

#[cfg(feature = "with-constraint")]
use constraints::ConstraintValue;
use core;
use std;
use std::collections::HashMap;
use std::str;
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
    #[serde(rename="vec")]
    Vector(Vec<Datum>),
}

impl Datum {
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
            Datum::Int(ref i_value) => Some((i_value.clone() as f64)),
            _ => None,
        }
    }

    pub fn to_variable(&self) -> Option<String> {
        match *self {
            Datum::Variable(ref value) => Some(value.clone()),
            _ => None,
        }
    }

    pub fn pprint(&self) -> String {
        match *self {
            Datum::String(ref s) => format!("'{}'", s),
            Datum::Int(ref i) => format!("{}", i),
            Datum::Float(ref f) => format!("{}", f),
            Datum::Variable(ref v) => format!("{}", v),
            Datum::Vector(ref args) => {
                let elements: Vec<String> = args.iter().map(|e| e.pprint()).collect();
                format!("({})", elements.join(","))
            }
            Datum::Nil => format!("nil"),
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
            utils::fold_while_some(bindings.clone(),
                                   &mut args.iter().zip(args2.iter()),
                                   &|bindings: core::Bindings<Datum>, (ref a, ref b): (&Datum, &Datum)| a.unify(&b, &bindings))
        }
        match *self {
            Datum::Variable(ref var_name) => bindings.update_bindings(var_name, other),
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
            utils::map_while_some(&mut args.iter(), &|arg| arg.apply_bindings(bindings))
        }
        match *self {
            Datum::Variable(ref var_name) => bindings.get_binding(var_name).or_else(|| Some(self.clone())),
            Datum::Vector(ref args) => apply_bindings_to_args(args, bindings).and_then(|args| Some(Datum::Vector(args))),
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
            _ => Vec::new(),
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
        write!(f, "{}", self.pprint())
    }
}

#[cfg(test)]
mod tests;
