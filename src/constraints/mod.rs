use serde_json;
use std;
use std::collections::HashMap;
use std::ops::{Add, Div, Mul, Sub};

use core::{Bindings, BindingsValue};
use utils;

pub trait ConstraintValue: BindingsValue {
    /// Constraint a ConstraintValue from a float
    fn float(f64) -> Self;
    /// Attempt to convert this value to a float
    fn to_float(&self) -> Option<f64>;
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub enum SolveResult<T: ConstraintValue> {
    /// A conflict was found, no solution possible
    Conflict,
    /// Incomplete solution; could not solve all constraints
    Partial(Bindings<T>),
    /// Successful solution
    Success(Bindings<T>),
}

impl<T: ConstraintValue> SolveResult<T> {
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

impl<T: ConstraintValue> std::fmt::Display for SolveResult<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", serde_json::to_string(&self).unwrap())
    }
}

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub enum NumericalConstraint {
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

impl Eq for NumericalConstraint {}
impl PartialOrd for NumericalConstraint {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (&NumericalConstraint::Set { ref variable, ref constant, .. },
             &NumericalConstraint::Set { variable: ref variable2, constant: ref constant2, .. }) => {
                match variable.partial_cmp(variable2) {
                    Some(std::cmp::Ordering::Equal) => constant.partial_cmp(constant2),
                    ordering => ordering,
                }
            }
            (&NumericalConstraint::Sum { ref first, ref second, ref third, .. },
             &NumericalConstraint::Sum { first: ref first2, second: ref second2, third: ref third2, .. }) => {
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
            (&NumericalConstraint::Mul { ref first, ref second, ref third, .. },
             &NumericalConstraint::Mul { first: ref first2, second: ref second2, third: ref third2, .. }) => {
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
            (&NumericalConstraint::GreaterThan { ref left, ref right, .. },
             &NumericalConstraint::GreaterThan { left: ref left2, right: ref right2, .. }) => {
                match left.partial_cmp(left2) {
                    Some(std::cmp::Ordering::Equal) => right.partial_cmp(right2),
                    ordering => ordering,
                }
            }
            (&NumericalConstraint::NotEqual { ref left, ref right, .. },
             &NumericalConstraint::NotEqual { left: ref left2, right: ref right2, .. }) => {
                match left.partial_cmp(left2) {
                    Some(std::cmp::Ordering::Equal) => right.partial_cmp(right2),
                    ordering => ordering,
                }
            }
            (_, _) => None,
        }
    }
}

impl std::fmt::Display for NumericalConstraint {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", serde_json::to_string(&self).unwrap())
    }
}

impl NumericalConstraint {
    /// Try to solve this constraint using the information in the bindings
    pub fn solve<T: ConstraintValue>(&self, bindings: &Bindings<T>) -> SolveResult<T> {
        let apply_op =
            |x: &T, y: &T, op: &Fn(f64, f64) -> f64| -> Option<T> { x.to_float().and_then(|x| y.to_float().and_then(|y| Some(T::float(op(x, y))))) };
        macro_rules! apply_op_or_return {
            ($x: ident, $y: ident, $z: ident, $op: expr) => ({
                if let Some(value) = apply_op($x, $y, &$op) {
                    ($z, value)
                } else {
                    return SolveResult::Conflict
                }
            })
        }
        let (key, value) = match *self {
            NumericalConstraint::Set { ref variable, ref constant, .. } => {
                match bindings.get_binding(variable) {
                    None => (variable, T::float(constant.clone())),
                    Some(ref value) if value.to_float() == Some(*constant) => (variable, value.clone()),
                    Some(_) => return SolveResult::Conflict,
                }
            }
            NumericalConstraint::Sum { ref first, ref second, ref third, .. } => {
                match (bindings.get_binding(first), bindings.get_binding(second), bindings.get_binding(third)) {
                    (Some(ref value), Some(ref value2), None) => apply_op_or_return!(value, value2, third, &Add::add),
                    (Some(ref value), None, Some(ref value3)) => apply_op_or_return!(value3, value, second, &Sub::sub),
                    (None, Some(ref value2), Some(ref value3)) => apply_op_or_return!(value3, value2, first, &Sub::sub),
                    (Some(ref value), Some(ref value2), Some(ref value3)) => {
                        if Some(value) == apply_op(value3, value2, &Sub::sub).as_ref() {
                            return SolveResult::Partial(bindings.clone());
                        } else {
                            return SolveResult::Conflict;
                        }
                    }
                    _ => return SolveResult::Partial(bindings.clone()),
                }
            }
            NumericalConstraint::Mul { ref first, ref second, ref third, .. } => {
                match (bindings.get_binding(first), bindings.get_binding(second), bindings.get_binding(third)) {
                    (Some(ref value), Some(ref value2), None) => apply_op_or_return!(value, value2, third, &Mul::mul),
                    (Some(ref value), None, Some(ref value3)) => apply_op_or_return!(value3, value, second, &Div::div),
                    (None, Some(ref value2), Some(ref value3)) => apply_op_or_return!(value3, value2, first, &Div::div),
                    (Some(ref value), Some(ref value2), Some(ref value3)) => {
                        if Some(value) == apply_op(value3, value2, &Div::div).as_ref() {
                            return SolveResult::Partial(bindings.clone());
                        } else {
                            return SolveResult::Conflict;
                        }
                    }
                    _ => return SolveResult::Partial(bindings.clone()),
                }
            }
            NumericalConstraint::GreaterThan { ref left, ref right, .. } => {
                match (bindings.get_binding(left), bindings.get_binding(right)) {
                    (Some(ref left_value), Some(ref right_value)) if left_value > right_value => return SolveResult::Success(bindings.clone()),
                    (Some(_), Some(_)) => return SolveResult::Conflict,
                    _ => return SolveResult::Partial(bindings.clone()),
                }
            }
            NumericalConstraint::NotEqual { ref left, ref right, .. } => {
                match (bindings.get_binding(left), bindings.get_binding(right)) {
                    (Some(ref left_value), Some(ref right_value)) if left_value != right_value => return SolveResult::Success(bindings.clone()),
                    (Some(_), Some(_)) => return SolveResult::Conflict,
                    _ => return SolveResult::Partial(bindings.clone()),
                }
            }
        };
        SolveResult::Success(bindings.set_binding(key, value))
    }

    pub fn rename_variables(&self, renamed_variables: &HashMap<String, String>) -> Self {
        let lookup = |v: &String| -> String { renamed_variables.get(v).cloned().or_else(|| Some(v.clone())).unwrap() };
        match *self {
            NumericalConstraint::Set { ref variable, ref constant, .. } => {
                NumericalConstraint::Set {
                    variable: lookup(variable),
                    constant: constant.clone(),
                }
            }
            NumericalConstraint::Sum { ref first, ref second, ref third, .. } => {
                NumericalConstraint::Sum {
                    first: lookup(first),
                    second: lookup(second),
                    third: lookup(third),
                }
            }
            NumericalConstraint::Mul { ref first, ref second, ref third, .. } => {
                NumericalConstraint::Mul {
                    first: lookup(first),
                    second: lookup(second),
                    third: lookup(third),
                }
            }
            NumericalConstraint::GreaterThan { ref left, ref right, .. } => {
                NumericalConstraint::GreaterThan {
                    left: lookup(left),
                    right: lookup(right),
                }
            }
            NumericalConstraint::NotEqual { ref left, ref right, .. } => {
                NumericalConstraint::GreaterThan {
                    left: lookup(left),
                    right: lookup(right),
                }
            }
        }
    }

    pub fn variables(&self) -> Vec<String> {
        match *self {
            NumericalConstraint::Set { ref variable, .. } => vec![variable.clone()],
            NumericalConstraint::Sum { ref first, ref second, ref third, .. } => vec![first.clone(), second.clone(), third.clone()],
            NumericalConstraint::Mul { ref first, ref second, ref third, .. } => vec![first.clone(), second.clone(), third.clone()],
            NumericalConstraint::GreaterThan { ref left, ref right, .. } => vec![left.clone(), right.clone()],
            NumericalConstraint::NotEqual { ref left, ref right, .. } => vec![left.clone(), right.clone()],
        }
    }
}

/*#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub enum StringConstraint {
    #[serde(rename="set")]
    Set {...},
    #[serde(rename="eq")]
    Eq {...},
    #[serde(rename="neq")]
    Neq {...},
}

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub enum DateTimeConstraint {
    #[serde(rename="set")]
    Set {...},
    #[serde(rename=">")]
    GreaterThan {...},
    #[serde(rename="sum")]
    Sum {...},
}*/

#[derive(Clone, Debug, Deserialize, PartialEq, PartialOrd, Serialize)]
pub enum Constraint {
    #[serde(rename="numerical")]
    Numerical(NumericalConstraint),
}

impl Eq for Constraint {}
impl std::fmt::Display for Constraint {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", serde_json::to_string(&self).unwrap())
    }
}

impl Constraint {
    pub fn solve<T: ConstraintValue>(&self, bindings: &Bindings<T>) -> SolveResult<T> {
        match *self {
            Constraint::Numerical(ref numerical_constraint) => numerical_constraint.solve(bindings),
        }
    }

    pub fn solve_many<T: ConstraintValue>(constraints: Vec<&Constraint>, bindings: &Bindings<T>) -> SolveResult<T> {
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
        match *self {
            Constraint::Numerical(ref numerical_constraint) => Constraint::Numerical(numerical_constraint.rename_variables(renamed_variables)),
        }
    }

    pub fn variables(&self) -> Vec<String> {
        match *self {
            Constraint::Numerical(ref numerical_constraint) => numerical_constraint.variables(),
        }
    }
}

#[cfg(test)]
mod tests;
