//! The rule module provides a data structure, Rule, that implements the Operation trait.
//! Rule aims to be a drop-in for any algorithm in ai_kit that operates on the Operation trait.

use constraints::{Constraint, ConstraintValue};
use core::{Bindings, Operation, Unify};
use std;
use std::collections::HashMap;
use std::marker::PhantomData;
use utils;

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Rule<T: ConstraintValue, U: Unify<T>> {
    #[serde(default)]
    pub constraints: Vec<Constraint>,
    #[serde(default = "Vec::default")]
    pub lhs: Vec<U>,
    pub rhs: U,
    #[serde(default)]
    pub _marker: PhantomData<T>,
}

impl<T, U> Operation<T, U> for Rule<T, U>
where
    T: ConstraintValue,
    U: Unify<T>,
{
    fn input_patterns(&self) -> Vec<U> {
        self.lhs.clone()
    }

    fn apply_match(&self, bindings: &Bindings<T>) -> Option<Vec<U>> {
        self.solve_constraints(&bindings).and_then(|bindings| {
            self.rhs
                .apply_bindings(&bindings)
                .and_then(|bound_rhs| Some(vec![bound_rhs]))
        })
    }

    fn r_apply_match(&self, fact: &U) -> Option<(Vec<U>, Bindings<T>)> {
        self.rhs
            .unify(fact, &Bindings::new())
            .and_then(|bindings| self.solve_constraints(&bindings))
            .and_then(|bindings| {
                utils::map_while_some(&mut self.lhs.iter(), &|f| {
                    f.apply_bindings(&bindings)
                }).and_then(|inputs| Some((inputs, bindings)))
            })
    }

    fn constraints<'a>(&'a self) -> Vec<&'a Constraint> {
        self.constraints.iter().collect()
    }

    /// Construct a new version of this rule but with all variables updated to be unique for this invocation
    fn snowflake(&self, suffix: String) -> Self {
        // Gather all variables
        let mut variables = self.rhs.variables();
        for lhs in self.lhs.iter() {
            variables.extend(lhs.variables());
        }
        for constraint in self.constraints.iter() {
            variables.extend(constraint.variables());
        }

        let renamed_variable: HashMap<String, String> = variables
            .into_iter()
            .map(|var| (var.clone(), format!("{}::{}", var, suffix)))
            .collect();

        let rhs = self.rhs.rename_variables(&renamed_variable);
        let lhs: Vec<U> = self.lhs
            .iter()
            .map(|lhs| lhs.rename_variables(&renamed_variable))
            .collect();
        let constraints: Vec<Constraint> = self.constraints
            .iter()
            .map(|constraint| constraint.rename_variables(&renamed_variable))
            .collect();

        Rule {
            constraints: constraints,
            lhs: lhs,
            rhs: rhs,
            _marker: PhantomData,
        }
    }
}

impl<T, U> Rule<T, U>
where
    T: ConstraintValue,
    U: Unify<T>,
{
    pub fn new(lhs: Vec<U>, rhs: U, constraints: Vec<Constraint>) -> Self {
        Rule {
            constraints: constraints,
            lhs: lhs,
            rhs: rhs,
            _marker: PhantomData,
        }
    }

    pub fn unify(&self, facts: &Vec<&U>, bindings: &Bindings<T>) -> Option<Bindings<T>> {
        utils::fold_while_some(
            bindings.clone(),
            &mut self.lhs.iter().zip(facts.iter()),
            &|bindings, (t1, t2)| t1.unify(t2, &bindings),
        )
    }

    pub fn apply_bindings(&self, bindings: &Bindings<T>) -> Option<U> {
        self.rhs.apply_bindings(bindings)
    }

    fn solve_constraints(&self, bindings: &Bindings<T>) -> Option<Bindings<T>> {
        Constraint::solve_many(self.constraints.iter().collect(), bindings).ok()
    }

    pub fn pprint(&self) -> String {
        let lhs_string_vec: Vec<String> = self.lhs.iter().map(|o| format!("{}", o)).collect();
        let lhs_string = lhs_string_vec.join("\n\t\t");
        let constraints_string_vec: Vec<String> = self.constraints.iter().map(|o| format!("{}", o)).collect();
        let constraints_string = constraints_string_vec.join("\n");
        format!(
            "Rule {{\n\tlhs:\n\t\t{},\n\trhs: {},\n\tconstraints: {} }}",
            lhs_string, self.rhs, constraints_string
        )
    }
}

impl<T, U> std::fmt::Display for Rule<T, U>
where
    T: ConstraintValue,
    U: Unify<T>,
{
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.pprint())
    }
}

impl<T, U> Eq for Rule<T, U>
where
    T: ConstraintValue,
    U: Unify<T>,
{
}

#[cfg(test)]
mod tests;
