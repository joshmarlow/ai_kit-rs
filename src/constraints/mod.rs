use std;
use std::collections::HashMap;
use std::marker::PhantomData;
use sexp::{Atom, Sexp};

use core::{Bindings, BindingsValue, FromSexpError, ToSexp};
use utils;

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub enum SolveResult<T: BindingsValue> {
    /// A conflict was found, no solution possible
    Conflict,
    /// Incomplete solution; could not solve all constraints
    Partial(Bindings<T>),
    /// Successful solution
    Success(Bindings<T>),
}

impl<T: BindingsValue> SolveResult<T> {
    pub fn ok(&self) -> Option<Bindings<T>> {
        match *self {
            SolveResult::Success(ref bindings) => Some(bindings.clone()),
            SolveResult::Partial(ref bindings) => Some(bindings.clone()),
            _ => None,
        }
    }

    pub fn and_then(&self, f: &Fn(&Bindings<T>) -> Self) -> Self {
        match *self {
            SolveResult::Success(ref bindings) => f(bindings),
            SolveResult::Partial(ref bindings) => f(bindings),
            _ => self.clone(),
        }
    }

    pub fn if_partial(&self, f: &Fn(&Bindings<T>) -> Self) -> Self {
        match *self {
            SolveResult::Partial(ref bindings) => f(bindings),
            _ => self.clone(),
        }
    }
}

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub enum Constraint<T: BindingsValue> {
    /// (constraint-set (?x CONSTANT))
    /// ?x = CONSTANT
    #[serde(rename="set")]
    Set { variable: String, constant: f64 },
    /// (constraint-sum (?x ?y ?z))
    /// ?x + ?y = ?z
    #[serde(rename="sum")]
    Sum {
        first: String,
        second: String,
        third: String,
        _marker: PhantomData<T>,
    },
    /// (constraint-mul (?x ?y ?z))
    /// ?x * ?y = ?z
    #[serde(rename="mul")]
    Mul {
        first: String,
        second: String,
        third: String,
        _marker: PhantomData<T>,
    },
    /// (constraint-less-than (?x ?y))
    /// ?x < ?y
    #[serde(rename="<")]
    LessThan {
        left: String,
        right: String,
        _marker: PhantomData<T>,
    },
    /// (constraint-greater-than (?x ?y))
    /// ?x > ?y
    #[serde(rename=">")]
    GreaterThan {
        left: String,
        right: String,
        _marker: PhantomData<T>,
    },
}

impl<T: BindingsValue> Eq for Constraint<T> {}

impl<T: BindingsValue> Constraint<T> {
    /// Try to solve this constraint using the information in the bindings
    pub fn solve(&self, bindings: &Bindings<T>) -> SolveResult<T> {
        let to_float =
            |value: &T| -> f64 { value.to_float().expect(&format!("Expected to convert value {:?} to float", value).as_str()) };
        let (key, value) = match *self {
            Constraint::Set { ref variable, ref constant, .. } => {
                match bindings.get_binding(variable) {
                    None => (variable, T::from_float(constant.clone())),
                    Some(ref value) if to_float(value) == *constant => (variable, value.clone()),
                    Some(_) => return SolveResult::Conflict,
                }
            }
            Constraint::Sum { ref first, ref second, ref third, .. } => {
                match (bindings.get_binding(first), bindings.get_binding(second), bindings.get_binding(third)) {
                    (Some(ref value), Some(ref value2), None) => (third, T::from_float(to_float(value) + to_float(value2))),
                    (Some(ref value), None, Some(ref value3)) => (second, T::from_float(to_float(value3) - to_float(value))),
                    (None, Some(ref value2), Some(ref value3)) => (first, T::from_float(to_float(value3) - to_float(value2))),
                    (Some(ref value), Some(ref value2), Some(ref value3)) => {
                        let expected_value1 = T::from_float(to_float(value3) - to_float(value2));
                        if expected_value1 == *value {
                            return SolveResult::Partial(bindings.clone());
                        } else {
                            return SolveResult::Conflict;
                        }
                    }
                    _ => return SolveResult::Partial(bindings.clone()),
                }
            }
            Constraint::Mul { ref first, ref second, ref third, .. } => {
                match (bindings.get_binding(first), bindings.get_binding(second), bindings.get_binding(third)) {
                    (Some(ref value), Some(ref value2), None) => (third, T::from_float(to_float(value) * to_float(value2))),
                    (Some(ref value), None, Some(ref value3)) => (second, T::from_float(to_float(value3) / to_float(value))),
                    (None, Some(ref value2), Some(ref value3)) => (first, T::from_float(to_float(value3) / to_float(value2))),
                    (Some(ref value), Some(ref value2), Some(ref value3)) => {
                        let expected_value1 = T::from_float(to_float(value3) / to_float(value2));
                        if expected_value1 == *value {
                            return SolveResult::Partial(bindings.clone());
                        } else {
                            return SolveResult::Conflict;
                        }
                    }
                    _ => return SolveResult::Partial(bindings.clone()),
                }
            }
            Constraint::LessThan { ref left, ref right, .. } => {
                match (bindings.get_binding(left), bindings.get_binding(right)) {
                    (Some(ref left_value), Some(ref right_value)) if left_value < right_value => {
                        return SolveResult::Success(bindings.clone())
                    }
                    (Some(_), Some(_)) => return SolveResult::Conflict,
                    _ => return SolveResult::Partial(bindings.clone()),
                }
            }
            Constraint::GreaterThan { ref left, ref right, .. } => {
                match (bindings.get_binding(left), bindings.get_binding(right)) {
                    (Some(ref left_value), Some(ref right_value)) if left_value > right_value => {
                        return SolveResult::Success(bindings.clone())
                    }
                    (Some(_), Some(_)) => return SolveResult::Conflict,
                    _ => return SolveResult::Partial(bindings.clone()),
                }
            }
        };
        SolveResult::Success(bindings.set_binding(key, value))
    }

    pub fn solve_many(constraints: Vec<&Constraint<T>>, bindings: &Bindings<T>) -> SolveResult<T> {
        // Aggregate all bindings from the constraints that we can solve
        let fold_result = utils::fold_while_some((Vec::new(), bindings.clone()),
                                                 &mut constraints.iter(),
                                                 &|(mut remaining_constraints, bindings), constraint| {
            let result: SolveResult<T> = constraint.solve(&bindings);
            match result {
                SolveResult::Conflict => None,
                SolveResult::Partial(bindings) => {
                    remaining_constraints.push(constraint.clone());
                    Some((remaining_constraints, bindings.clone()))
                }
                SolveResult::Success(bindings) => Some((remaining_constraints, bindings.clone())),
            }
        });
        match fold_result {
            Some((remaining_constraints, bindings)) => {
                if remaining_constraints.is_empty() {
                    SolveResult::Success(bindings)
                } else if remaining_constraints.len() == constraints.len() {
                    // We've made no progress, this is unsolvable
                    SolveResult::Partial(bindings)
                } else {
                    Constraint::solve_many(remaining_constraints, &bindings)
                }
            }
            None => SolveResult::Conflict,
        }
    }

    pub fn rename_variables(&self, renamed_variables: &HashMap<String, String>) -> Self {
        let lookup = |v: &String| -> String { renamed_variables.get(v).cloned().or_else(|| Some(v.clone())).unwrap() };
        match *self {
            Constraint::Set { ref variable, ref constant, .. } => {
                Constraint::Set {
                    variable: lookup(variable),
                    constant: constant.clone(),
                }
            }
            Constraint::Sum { ref first, ref second, ref third, .. } => {
                Constraint::Sum {
                    first: lookup(first),
                    second: lookup(second),
                    third: lookup(third),
                    _marker: PhantomData,
                }
            }
            Constraint::Mul { ref first, ref second, ref third, .. } => {
                Constraint::Mul {
                    first: lookup(first),
                    second: lookup(second),
                    third: lookup(third),
                    _marker: PhantomData,
                }
            }
            Constraint::LessThan { ref left, ref right, .. } => {
                Constraint::LessThan {
                    left: lookup(left),
                    right: lookup(right),
                    _marker: PhantomData,
                }
            }
            Constraint::GreaterThan { ref left, ref right, .. } => {
                Constraint::GreaterThan {
                    left: lookup(left),
                    right: lookup(right),
                    _marker: PhantomData,
                }
            }
        }
    }

    pub fn variables(&self) -> Vec<String> {
        match *self {
            Constraint::Set { ref variable, .. } => vec![variable.clone()],
            Constraint::Sum { ref first, ref second, ref third, .. } => vec![first.clone(), second.clone(), third.clone()],
            Constraint::Mul { ref first, ref second, ref third, .. } => vec![first.clone(), second.clone(), third.clone()],
            Constraint::LessThan { ref left, ref right, .. } => vec![left.clone(), right.clone()],
            Constraint::GreaterThan { ref left, ref right, .. } => vec![left.clone(), right.clone()],
        }
    }
}

impl<T: BindingsValue> PartialOrd for Constraint<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (&Constraint::Set { ref variable, ref constant, .. },
             &Constraint::Set { variable: ref variable2, constant: ref constant2, .. }) => {
                match variable.partial_cmp(variable2) {
                    Some(std::cmp::Ordering::Equal) => constant.partial_cmp(constant2),
                    ordering => ordering,
                }
            }
            (&Constraint::Sum { ref first, ref second, ref third, .. },
             &Constraint::Sum { first: ref first2, second: ref second2, third: ref third2, .. }) => {
                match first.partial_cmp(first2) {
                    Some(std::cmp::Ordering::Equal) => {
                        match second.partial_cmp(second2) {
                            Some(std::cmp::Ordering::Equal) => third.partial_cmp(third2),
                            ordering => ordering,
                        }
                    }
                    ordering => ordering,
                }
            }
            (&Constraint::Mul { ref first, ref second, ref third, .. },
             &Constraint::Mul { first: ref first2, second: ref second2, third: ref third2, .. }) => {
                match first.partial_cmp(first2) {
                    Some(std::cmp::Ordering::Equal) => {
                        match second.partial_cmp(second2) {
                            Some(std::cmp::Ordering::Equal) => third.partial_cmp(third2),
                            ordering => ordering,
                        }
                    }
                    ordering => ordering,
                }
            }
            (&Constraint::LessThan { ref left, ref right, .. },
             &Constraint::LessThan { left: ref left2, right: ref right2, .. }) => {
                match left.partial_cmp(left2) {
                    Some(std::cmp::Ordering::Equal) => right.partial_cmp(right2),
                    ordering => ordering,
                }
            }
            (&Constraint::GreaterThan { ref left, ref right, .. },
             &Constraint::GreaterThan { left: ref left2, right: ref right2, .. }) => {
                match left.partial_cmp(left2) {
                    Some(std::cmp::Ordering::Equal) => right.partial_cmp(right2),
                    ordering => ordering,
                }
            }
            (_, _) => None,
        }
    }
}

impl<T: BindingsValue> ToSexp for Constraint<T> {
    fn to_sexp(&self) -> Sexp {
        match *self {
            Constraint::Set { ref variable, ref constant, .. } => {
                utils::to_sexp_helper("constraint-set",
                                      Sexp::List(vec![Sexp::Atom(Atom::S(variable.clone())),
                                                      Sexp::Atom(Atom::F(constant.clone()))]))
            }
            Constraint::Sum { ref first, ref second, ref third, .. } => {
                utils::to_sexp_helper("constraint-sum",
                                      Sexp::List(vec![Sexp::Atom(Atom::S(first.clone())),
                                                      Sexp::Atom(Atom::S(second.clone())),
                                                      Sexp::Atom(Atom::S(third.clone()))]))
            }
            Constraint::Mul { ref first, ref second, ref third, .. } => {
                utils::to_sexp_helper("constraint-mul",
                                      Sexp::List(vec![Sexp::Atom(Atom::S(first.clone())),
                                                      Sexp::Atom(Atom::S(second.clone())),
                                                      Sexp::Atom(Atom::S(third.clone()))]))
            }
            Constraint::LessThan { ref left, ref right, .. } => {
                utils::to_sexp_helper("constraint-less-than",
                                      Sexp::List(vec![Sexp::Atom(Atom::S(left.clone())), Sexp::Atom(Atom::S(right.clone()))]))
            }
            Constraint::GreaterThan { ref left, ref right, .. } => {
                utils::to_sexp_helper("constraint-greater-than",
                                      Sexp::List(vec![Sexp::Atom(Atom::S(left.clone())), Sexp::Atom(Atom::S(right.clone()))]))
            }
        }
    }

    fn from_sexp(s_exp: &Sexp) -> std::result::Result<Self, FromSexpError> {
        utils::from_sexp_helper("constraint-set",
                                s_exp,
                                2,
                                &|args| match (&args[0], &args[1]) {
                                    (&Sexp::Atom(Atom::S(ref variable)), &Sexp::Atom(Atom::F(ref constant))) => {
                                        Ok(Constraint::Set {
                                            variable: variable.clone(),
                                            constant: constant.clone(),
                                        })
                                    }
                                    (&Sexp::Atom(Atom::S(ref variable)), &Sexp::Atom(Atom::I(ref constant))) => {
                                        Ok(Constraint::Set {
                                            variable: variable.clone(),
                                            constant: constant.clone() as f64,
                                        })
                                    }
                                    _ => {
                                        Err(FromSexpError {
                                            message: "Expected (atom list), but received (list atom)".to_string(),
                                        })
                                    }
                                })
            .or_else(|_err| {
                utils::from_sexp_helper("constraint-sum",
                                        s_exp,
                                        3,
                                        &|args| match (&args[0], &args[1], &args[2]) {
                                            (&Sexp::Atom(Atom::S(ref first)),
                                             &Sexp::Atom(Atom::S(ref second)),
                                             &Sexp::Atom(Atom::S(ref third))) => {
                                                Ok(Constraint::Sum {
                                                    first: first.clone(),
                                                    second: second.clone(),
                                                    third: third.clone(),
                                                    _marker: PhantomData,
                                                })
                                            }
                                            _ => {
                                                Err(FromSexpError {
                                                    message: "Expected (atom list), but received (list atom)".to_string(),
                                                })
                                            }
                                        })
            })
            .or_else(|_err| {
                utils::from_sexp_helper("constraint-mul",
                                        s_exp,
                                        3,
                                        &|args| match (&args[0], &args[1], &args[2]) {
                                            (&Sexp::Atom(Atom::S(ref first)),
                                             &Sexp::Atom(Atom::S(ref second)),
                                             &Sexp::Atom(Atom::S(ref third))) => {
                                                Ok(Constraint::Mul {
                                                    first: first.clone(),
                                                    second: second.clone(),
                                                    third: third.clone(),
                                                    _marker: PhantomData,
                                                })
                                            }
                                            _ => {
                                                Err(FromSexpError {
                                                    message: "Expected (atom list), but received (list atom)".to_string(),
                                                })
                                            }
                                        })
            })
            .or_else(|_err| {
                utils::from_sexp_helper("constraint-less-than",
                                        s_exp,
                                        2,
                                        &|args| match (&args[0], &args[1]) {
                                            (&Sexp::Atom(Atom::S(ref left_var)), &Sexp::Atom(Atom::S(ref right_var))) => {
                                                Ok(Constraint::LessThan {
                                                    left: left_var.clone(),
                                                    right: right_var.clone(),
                                                    _marker: PhantomData,
                                                })
                                            }
                                            _ => {
                                                Err(FromSexpError {
                                                    message: "Expected (atom list), but received (list atom)".to_string(),
                                                })
                                            }
                                        })
            })
            .or_else(|_err| {
                utils::from_sexp_helper("constraint-greater-than",
                                        s_exp,
                                        2,
                                        &|args| match (&args[0], &args[1]) {
                                            (&Sexp::Atom(Atom::S(ref left_var)), &Sexp::Atom(Atom::S(ref right_var))) => {
                                                Ok(Constraint::GreaterThan {
                                                    left: left_var.clone(),
                                                    right: right_var.clone(),
                                                    _marker: PhantomData,
                                                })
                                            }
                                            _ => {
                                                Err(FromSexpError {
                                                    message: "Expected (atom list), but received (list atom)".to_string(),
                                                })
                                            }
                                        })
            })
            .or_else(|_err| {
                Err(FromSexpError {
                    message: format!("Constraint::from_sexp - sexp does not match any case - '{}'",
                                     s_exp),
                })
            })
    }
}

impl<T: BindingsValue> std::fmt::Display for Constraint<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.to_sexp())
    }
}
