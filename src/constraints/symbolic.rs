use super::*;
use serde_json;
use std;

#[derive(Clone, Debug, Deserialize, PartialEq, PartialOrd, Serialize)]
pub enum SymbolicConstraint {
    Eq { v1: String, v2: String },
    Neq { v1: String, v2: String },
}

impl std::fmt::Display for SymbolicConstraint {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", serde_json::to_string(&self).unwrap())
    }
}

impl SymbolicConstraint {
    /// Try to solve this constraint using the information in the bindings
    pub fn solve<T: ConstraintValue>(&self, bindings: &Bindings<T>) -> SolveResult<T> {
        match *self {
            SymbolicConstraint::Eq { ref v1, ref v2 } => {
                println!(
                    "\n{:?}\n{:?}\n{:?}",
                    bindings.get_binding(v1),
                    bindings.get_binding(v2),
                    T::variable(v1).and_then(|var1| Some(SolveResult::Partial(bindings.set_binding(v2, var1))))
                );
                match (bindings.get_binding(v1), bindings.get_binding(v2)) {
                    (Some(ref val1), Some(ref val2)) => {
                        if val1.eq(val2) {
                            Some(SolveResult::Success(bindings.clone()))
                        } else {
                            None
                        }
                    }
                    _ => Some(SolveResult::Partial(bindings.clone())),
                }
            }
            SymbolicConstraint::Neq { ref v1, ref v2 } => match (bindings.get_binding(v1), bindings.get_binding(v2)) {
                (Some(ref val1), Some(ref val2)) => {
                    if val1.eq(val2) {
                        None
                    } else {
                        Some(SolveResult::Success(bindings.clone()))
                    }
                }
                _ => Some(SolveResult::Partial(bindings.clone())),
            },
        }.unwrap_or(SolveResult::Conflict)
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
            SymbolicConstraint::Eq { ref v1, ref v2 } => SymbolicConstraint::Eq {
                v1: lookup(v1),
                v2: lookup(v2),
            },
            SymbolicConstraint::Neq { ref v1, ref v2 } => SymbolicConstraint::Neq {
                v1: lookup(v1),
                v2: lookup(v2),
            },
        }
    }

    pub fn variables(&self) -> Vec<String> {
        match *self {
            SymbolicConstraint::Eq { ref v1, ref v2 } => vec![v1.clone(), v2.clone()],
            SymbolicConstraint::Neq { ref v1, ref v2 } => vec![v1.clone(), v2.clone()],
        }
    }
}
