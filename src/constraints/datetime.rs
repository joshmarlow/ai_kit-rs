use super::*;
use chrono::{DateTime, Duration, offset};
use serde;
use std;

#[derive(Clone, Debug, Deserialize, PartialEq, PartialOrd, Serialize)]
pub struct TimeSpan {
    seconds: i64,
}

impl TimeSpan {
    pub fn to_duration(&self) -> Duration {
        Duration::seconds(self.seconds)
    }
}

#[derive(Clone, Debug, Deserialize, PartialEq, PartialOrd, Serialize)]
pub enum DateTimeConstraint<TZ>
    where TZ: offset::TimeZone + serde::Deserialize + serde::Serialize + PartialEq,
          DateTime<TZ>: serde::Deserialize + serde::Serialize + PartialEq
{
    #[serde(rename="set")]
    Set { variable: String, dt: DateTime<TZ> },
    #[serde(rename="sum")]
    Sum {
        t1: String,
        duration: TimeSpan,
        t2: String,
    },
    #[serde(rename=">")]
    GreaterThan { left: String, right: String },
}

impl<TZ> std::fmt::Display for DateTimeConstraint<TZ>
    where TZ: offset::TimeZone + serde::Deserialize + serde::Serialize + PartialEq,
          DateTime<TZ>: serde::Deserialize + serde::Serialize + PartialEq
{
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", serde_json::to_string(&self).unwrap())
    }
}

impl<TZ> Eq for DateTimeConstraint<TZ>
    where TZ: offset::TimeZone + serde::Deserialize + serde::Serialize + PartialEq,
          DateTime<TZ>: serde::Deserialize + serde::Serialize + PartialEq
{
}

impl DateTimeConstraint {
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
            DateTimeConstraint::Set { ref variable, ref constant, .. } => {
                match bindings.get_binding(variable) {
                    None => (variable, T::float(constant.clone())),
                    Some(ref value) if value.to_float() == Some(*constant) => (variable, value.clone()),
                    Some(_) => return SolveResult::Conflict,
                }
            }
            DateTimeConstraint::Sum { ref first, ref second, ref third, .. } => {
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
            DateTimeConstraint::GreaterThan { ref left, ref right, .. } => {
                match (bindings.get_binding(left), bindings.get_binding(right)) {
                    (Some(ref left_value), Some(ref right_value)) if left_value > right_value => return SolveResult::Success(bindings.clone()),
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
            DateTimeConstraint::Set { ref variable, ref constant, .. } => {
                DateTimeConstraint::Set {
                    variable: lookup(variable),
                    constant: constant.clone(),
                }
            }
            DateTimeConstraint::Sum { ref first, ref second, ref third, .. } => {
                DateTimeConstraint::Sum {
                    first: lookup(first),
                    second: lookup(second),
                    third: lookup(third),
                }
            }
            DateTimeConstraint::GreaterThan { ref left, ref right, .. } => {
                DateTimeConstraint::GreaterThan {
                    left: lookup(left),
                    right: lookup(right),
                }
            }
        }
    }

    pub fn variables(&self) -> Vec<String> {
        match *self {
            DateTimeConstraint::Set { ref variable, .. } => vec![variable.clone()],
            DateTimeConstraint::Sum { ref first, ref second, ref third, .. } => vec![first.clone(), second.clone(), third.clone()],
            DateTimeConstraint::GreaterThan { ref left, ref right, .. } => vec![left.clone(), right.clone()],
        }
    }
}
