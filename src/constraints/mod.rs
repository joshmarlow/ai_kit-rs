use serde_json;
use std;
use std::collections::HashMap;

use core::{Bindings, BindingsValue};
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

impl<T: BindingsValue> std::fmt::Display for SolveResult<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", serde_json::to_string(&self).unwrap())
    }
}

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub enum Constraint {
    /// ?x = CONSTANT
    #[serde(rename="set")]
    Set { variable: String, constant: f64 },
    /// ?x + ?y = ?z
    #[serde(rename="sum")]
    Sum {
        first: String,
        second: String,
        third: String,
    },
    /// ?x * ?y = ?z
    #[serde(rename="mul")]
    Mul {
        first: String,
        second: String,
        third: String,
    },
    /// ?x > ?y
    #[serde(rename=">")]
    GreaterThan { left: String, right: String },
    /// ?x != ?y
    #[serde(rename="neq")]
    NotEqual { left: String, right: String },
}

impl Eq for Constraint {}

impl Constraint {
    /// Try to solve this constraint using the information in the bindings
    pub fn solve<T: BindingsValue>(&self, bindings: &Bindings<T>) -> SolveResult<T> {
        let to_float = |value: &T| -> f64 { value.to_float().expect(&format!("Expected to convert value {:?} to float", value).as_str()) };
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
            Constraint::GreaterThan { ref left, ref right, .. } => {
                match (bindings.get_binding(left), bindings.get_binding(right)) {
                    (Some(ref left_value), Some(ref right_value)) if left_value > right_value => return SolveResult::Success(bindings.clone()),
                    (Some(_), Some(_)) => return SolveResult::Conflict,
                    _ => return SolveResult::Partial(bindings.clone()),
                }
            }
            Constraint::NotEqual { ref left, ref right, .. } => {
                match (bindings.get_binding(left), bindings.get_binding(right)) {
                    (Some(ref left_value), Some(ref right_value)) if left_value != right_value => return SolveResult::Success(bindings.clone()),
                    (Some(_), Some(_)) => return SolveResult::Conflict,
                    _ => return SolveResult::Partial(bindings.clone()),
                }
            }
        };
        SolveResult::Success(bindings.set_binding(key, value))
    }

    pub fn solve_many<T: BindingsValue>(constraints: Vec<&Constraint>, bindings: &Bindings<T>) -> SolveResult<T> {
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
                }
            }
            Constraint::Mul { ref first, ref second, ref third, .. } => {
                Constraint::Mul {
                    first: lookup(first),
                    second: lookup(second),
                    third: lookup(third),
                }
            }
            Constraint::GreaterThan { ref left, ref right, .. } => {
                Constraint::GreaterThan {
                    left: lookup(left),
                    right: lookup(right),
                }
            }
            Constraint::NotEqual { ref left, ref right, .. } => {
                Constraint::GreaterThan {
                    left: lookup(left),
                    right: lookup(right),
                }
            }
        }
    }

    pub fn variables(&self) -> Vec<String> {
        match *self {
            Constraint::Set { ref variable, .. } => vec![variable.clone()],
            Constraint::Sum { ref first, ref second, ref third, .. } => vec![first.clone(), second.clone(), third.clone()],
            Constraint::Mul { ref first, ref second, ref third, .. } => vec![first.clone(), second.clone(), third.clone()],
            Constraint::GreaterThan { ref left, ref right, .. } => vec![left.clone(), right.clone()],
            Constraint::NotEqual { ref left, ref right, .. } => vec![left.clone(), right.clone()],
        }
    }
}

impl PartialOrd for Constraint {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (&Constraint::Set { ref variable, ref constant, .. }, &Constraint::Set { variable: ref variable2, constant: ref constant2, .. }) => {
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
            (&Constraint::GreaterThan { ref left, ref right, .. }, &Constraint::GreaterThan { left: ref left2, right: ref right2, .. }) => {
                match left.partial_cmp(left2) {
                    Some(std::cmp::Ordering::Equal) => right.partial_cmp(right2),
                    ordering => ordering,
                }
            }
            (&Constraint::NotEqual { ref left, ref right, .. }, &Constraint::NotEqual { left: ref left2, right: ref right2, .. }) => {
                match left.partial_cmp(left2) {
                    Some(std::cmp::Ordering::Equal) => right.partial_cmp(right2),
                    ordering => ordering,
                }
            }
            (_, _) => None,
        }
    }
}

impl std::fmt::Display for Constraint {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", serde_json::to_string(&self).unwrap())
    }
}

#[cfg(test)]
mod tests;
