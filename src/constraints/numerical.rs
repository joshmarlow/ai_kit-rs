use super::*;
use serde_json;
use std;

#[derive(Clone, Copy, Debug, Default, Deserialize, PartialEq, PartialOrd, Serialize)]
pub struct Number {
    pub value: f64,
}

impl std::fmt::Display for Number {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl Eq for Number {}
impl BindingsValue for Number {}

impl ConstraintValue for Number {
    fn float(f: f64) -> Self {
        Number { value: f }
    }

    fn to_float(&self) -> Option<f64> {
        Some(self.value)
    }
}

impl Number {
    pub fn new(f: f64) -> Self {
        Number { value: f }
    }
}

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub enum NumericalConstraint {
    /// ?x = CONSTANT
    #[serde(rename = "set")]
    Set { variable: String, constant: f64 },
    /// ?x + ?y = ?z
    #[serde(rename = "sum")]
    Sum {
        first: String,
        second: String,
        third: String,
    },
    /// ?x * ?y = ?z
    #[serde(rename = "mul")]
    Mul {
        first: String,
        second: String,
        third: String,
    },
    /// ?x > ?y
    #[serde(rename = ">")]
    GreaterThan { left: String, right: String },
    /// ?x != ?y
    #[serde(rename = "neq")]
    NotEqual { left: String, right: String },
}

impl Eq for NumericalConstraint {}
impl Ord for NumericalConstraint {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.partial_cmp(other).unwrap_or(std::cmp::Ordering::Less)
    }
}
impl PartialOrd for NumericalConstraint {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (
                &NumericalConstraint::Set {
                    ref variable,
                    ref constant,
                    ..
                },
                &NumericalConstraint::Set {
                    variable: ref variable2,
                    constant: ref constant2,
                    ..
                },
            ) => match variable.partial_cmp(variable2) {
                Some(std::cmp::Ordering::Equal) => constant.partial_cmp(constant2),
                ordering => ordering,
            },
            (
                &NumericalConstraint::Sum {
                    ref first,
                    ref second,
                    ref third,
                    ..
                },
                &NumericalConstraint::Sum {
                    first: ref first2,
                    second: ref second2,
                    third: ref third2,
                    ..
                },
            ) => match first.partial_cmp(first2) {
                Some(std::cmp::Ordering::Equal) => match second.partial_cmp(second2) {
                    Some(std::cmp::Ordering::Equal) => third.partial_cmp(third2),
                    ordering => ordering,
                },
                ordering => ordering,
            },
            (
                &NumericalConstraint::Mul {
                    ref first,
                    ref second,
                    ref third,
                    ..
                },
                &NumericalConstraint::Mul {
                    first: ref first2,
                    second: ref second2,
                    third: ref third2,
                    ..
                },
            ) => match first.partial_cmp(first2) {
                Some(std::cmp::Ordering::Equal) => match second.partial_cmp(second2) {
                    Some(std::cmp::Ordering::Equal) => third.partial_cmp(third2),
                    ordering => ordering,
                },
                ordering => ordering,
            },
            (
                &NumericalConstraint::GreaterThan {
                    ref left,
                    ref right,
                    ..
                },
                &NumericalConstraint::GreaterThan {
                    left: ref left2,
                    right: ref right2,
                    ..
                },
            ) => match left.partial_cmp(left2) {
                Some(std::cmp::Ordering::Equal) => right.partial_cmp(right2),
                ordering => ordering,
            },
            (
                &NumericalConstraint::NotEqual {
                    ref left,
                    ref right,
                    ..
                },
                &NumericalConstraint::NotEqual {
                    left: ref left2,
                    right: ref right2,
                    ..
                },
            ) => match left.partial_cmp(left2) {
                Some(std::cmp::Ordering::Equal) => right.partial_cmp(right2),
                ordering => ordering,
            },
            (&NumericalConstraint::Set { .. }, &NumericalConstraint::Sum { .. }) => Some(std::cmp::Ordering::Less),
            (&NumericalConstraint::Set { .. }, &NumericalConstraint::Mul { .. }) => Some(std::cmp::Ordering::Less),
            (&NumericalConstraint::Set { .. }, &NumericalConstraint::GreaterThan { .. }) => Some(std::cmp::Ordering::Less),
            (&NumericalConstraint::Set { .. }, &NumericalConstraint::NotEqual { .. }) => Some(std::cmp::Ordering::Less),

            (&NumericalConstraint::Sum { .. }, &NumericalConstraint::Set { .. }) => Some(std::cmp::Ordering::Greater),
            (&NumericalConstraint::Sum { .. }, &NumericalConstraint::Mul { .. }) => Some(std::cmp::Ordering::Less),
            (&NumericalConstraint::Sum { .. }, &NumericalConstraint::GreaterThan { .. }) => Some(std::cmp::Ordering::Less),
            (&NumericalConstraint::Sum { .. }, &NumericalConstraint::NotEqual { .. }) => Some(std::cmp::Ordering::Less),

            (&NumericalConstraint::Mul { .. }, &NumericalConstraint::Set { .. }) => Some(std::cmp::Ordering::Greater),
            (&NumericalConstraint::Mul { .. }, &NumericalConstraint::Sum { .. }) => Some(std::cmp::Ordering::Greater),
            (&NumericalConstraint::Mul { .. }, &NumericalConstraint::GreaterThan { .. }) => Some(std::cmp::Ordering::Less),
            (&NumericalConstraint::Mul { .. }, &NumericalConstraint::NotEqual { .. }) => Some(std::cmp::Ordering::Less),

            (&NumericalConstraint::GreaterThan { .. }, &NumericalConstraint::Set { .. }) => Some(std::cmp::Ordering::Greater),
            (&NumericalConstraint::GreaterThan { .. }, &NumericalConstraint::Sum { .. }) => Some(std::cmp::Ordering::Greater),
            (&NumericalConstraint::GreaterThan { .. }, &NumericalConstraint::Mul { .. }) => Some(std::cmp::Ordering::Greater),
            (&NumericalConstraint::GreaterThan { .. }, &NumericalConstraint::NotEqual { .. }) => Some(std::cmp::Ordering::Less),

            (&NumericalConstraint::NotEqual { .. }, &NumericalConstraint::Set { .. }) => Some(std::cmp::Ordering::Greater),
            (&NumericalConstraint::NotEqual { .. }, &NumericalConstraint::Sum { .. }) => Some(std::cmp::Ordering::Greater),
            (&NumericalConstraint::NotEqual { .. }, &NumericalConstraint::Mul { .. }) => Some(std::cmp::Ordering::Greater),
            (&NumericalConstraint::NotEqual { .. }, &NumericalConstraint::GreaterThan { .. }) => Some(std::cmp::Ordering::Greater),
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
        let apply_op = |x: &T, y: &T, op: &Fn(f64, f64) -> f64| -> Option<T> {
            x.to_float()
                .and_then(|x| y.to_float().and_then(|y| Some(T::float(op(x, y)))))
        };
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
            NumericalConstraint::Set {
                ref variable,
                ref constant,
                ..
            } => match bindings.get_binding(variable) {
                None => (variable, T::float(constant.clone())),
                Some(ref value) if value.to_float() == Some(*constant) => (variable, value.clone()),
                Some(_) => return SolveResult::Conflict,
            },
            NumericalConstraint::Sum {
                ref first,
                ref second,
                ref third,
                ..
            } => match (
                bindings.get_binding(first),
                bindings.get_binding(second),
                bindings.get_binding(third),
            ) {
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
            },
            NumericalConstraint::Mul {
                ref first,
                ref second,
                ref third,
                ..
            } => match (
                bindings.get_binding(first),
                bindings.get_binding(second),
                bindings.get_binding(third),
            ) {
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
            },
            NumericalConstraint::GreaterThan {
                ref left,
                ref right,
                ..
            } => match (bindings.get_binding(left), bindings.get_binding(right)) {
                (Some(ref left_value), Some(ref right_value)) if left_value > right_value => return SolveResult::Success(bindings.clone()),
                (Some(_), Some(_)) => return SolveResult::Conflict,
                _ => return SolveResult::Partial(bindings.clone()),
            },
            NumericalConstraint::NotEqual {
                ref left,
                ref right,
                ..
            } => match (bindings.get_binding(left), bindings.get_binding(right)) {
                (Some(ref left_value), Some(ref right_value)) if left_value != right_value => return SolveResult::Success(bindings.clone()),
                (Some(_), Some(_)) => return SolveResult::Conflict,
                _ => return SolveResult::Partial(bindings.clone()),
            },
        };
        SolveResult::Success(bindings.set_binding(key, value))
    }

    pub fn rename_variables(&self, renamed_variables: &HashMap<String, String>) -> Self {
        let lookup = |v: &String| -> String {
            renamed_variables
                .get(v)
                .cloned()
                .or_else(|| Some(v.clone()))
                .unwrap()
        };
        match *self {
            NumericalConstraint::Set {
                ref variable,
                ref constant,
                ..
            } => NumericalConstraint::Set {
                variable: lookup(variable),
                constant: constant.clone(),
            },
            NumericalConstraint::Sum {
                ref first,
                ref second,
                ref third,
                ..
            } => NumericalConstraint::Sum {
                first: lookup(first),
                second: lookup(second),
                third: lookup(third),
            },
            NumericalConstraint::Mul {
                ref first,
                ref second,
                ref third,
                ..
            } => NumericalConstraint::Mul {
                first: lookup(first),
                second: lookup(second),
                third: lookup(third),
            },
            NumericalConstraint::GreaterThan {
                ref left,
                ref right,
                ..
            } => NumericalConstraint::GreaterThan {
                left: lookup(left),
                right: lookup(right),
            },
            NumericalConstraint::NotEqual {
                ref left,
                ref right,
                ..
            } => NumericalConstraint::GreaterThan {
                left: lookup(left),
                right: lookup(right),
            },
        }
    }

    pub fn variables(&self) -> Vec<String> {
        match *self {
            NumericalConstraint::Set { ref variable, .. } => vec![variable.clone()],
            NumericalConstraint::Sum {
                ref first,
                ref second,
                ref third,
                ..
            } => vec![first.clone(), second.clone(), third.clone()],
            NumericalConstraint::Mul {
                ref first,
                ref second,
                ref third,
                ..
            } => vec![first.clone(), second.clone(), third.clone()],
            NumericalConstraint::GreaterThan {
                ref left,
                ref right,
                ..
            } => vec![left.clone(), right.clone()],
            NumericalConstraint::NotEqual {
                ref left,
                ref right,
                ..
            } => vec![left.clone(), right.clone()],
        }
    }
}
