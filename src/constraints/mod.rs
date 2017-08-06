//! The constraints module implements a very basic system for checking and solving constraints.
//!
//! Examples
//!
//! ```
//! use ai_kit::core::Bindings;
//! use ai_kit::constraints::{Constraint, Number, NumericalConstraint, SolveResult};
//!
//! // Construct a vector of constraints
//! let constraints = vec![
//!    // Specify a value for a variable
//!    Constraint::Numerical(NumericalConstraint::Set{
//!        variable: "x".to_string(),
//!        constant: 1.0,
//!    }),
//!    // Specify a value for a second variable
//!    Constraint::Numerical(NumericalConstraint::Set{
//!        variable: "sum".to_string(),
//!        constant: 3.0,
//!    }),
//!    // Specify the relation of the first two variables and a third unknown variable
//!    Constraint::Numerical(NumericalConstraint::Sum{
//!        first: "x".to_string(),
//!        second: "y".to_string(),
//!        third: "sum".to_string()
//!    }),
//! ];
//!
//! // Check that constraints are satisfied and infer any new values that we can
//! let result : SolveResult<Number> = Constraint::solve_many(
//!     constraints.iter().collect(),
//!     &Bindings::new(),
//! );
//!
//! // Verify that the unknown variable (y) has the expected inferred value
//! let expected_inferred_value = Number { value: 2.0 };
//!
//! if let SolveResult::Success(bindings) = result {
//!    assert_eq!(
//!      bindings.get_binding(&"y".to_string()).unwrap(),
//!      expected_inferred_value
//!    );
//! }
//! ```

pub use self::numerical::*;

use core::{Bindings, BindingsValue};
use serde_json;
use std;
use std::collections::HashMap;
use std::ops::{Add, Div, Mul, Sub};
use utils;
mod numerical;

pub use self::symbolic::*;
mod symbolic;

pub trait ConstraintValue: BindingsValue {
    /// Construct a ConstraintValue from a float
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

#[derive(Clone, Debug, Deserialize, PartialEq, PartialOrd, Serialize)]
pub enum Constraint {
    #[serde(rename="numerical")]
    Numerical(NumericalConstraint),
    Symbolic(SymbolicConstraint),
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
            Constraint::Symbolic(ref symbolic_constraint) => symbolic_constraint.solve(bindings),
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
            Constraint::Symbolic(ref symbolic_constraint) => Constraint::Symbolic(symbolic_constraint.rename_variables(renamed_variables)),
        }
    }

    pub fn variables(&self) -> Vec<String> {
        match *self {
            Constraint::Numerical(ref numerical_constraint) => numerical_constraint.variables(),
            Constraint::Symbolic(ref symbolic_constraint) => symbolic_constraint.variables(),
        }
    }
}

#[cfg(test)]
mod tests;
