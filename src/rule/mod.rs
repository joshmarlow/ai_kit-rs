//! The rule module provides two data structures, Rule and MultiRule, that implement the Operation trait.
//! Rule aims to be a drop-in for any algorithm in ai_kit that operates on the Operation trait.  It is useful when you have one possible result for an operation.
//! MultiRule aims to be a drop-in for any algorithm in ai_kit that operates on the Operation trait.  It is useful when you have multiple results for an operation.

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
            "Rule {{\n\tlhs:\n\t\t{},\n\trhs:\n\t\t{},\n\tconstraints:\n\t\t{} }}",
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

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct MultiRule<T: ConstraintValue, U: Unify<T>> {
    #[serde(default)]
    pub constraints: Vec<Constraint>,
    #[serde(default = "Vec::default")]
    pub lhs: Vec<U>,
    pub rhs: Vec<U>,
    #[serde(default)]
    pub _marker: PhantomData<T>,
}

impl<T, U> Operation<T, U> for MultiRule<T, U>
where
    T: ConstraintValue,
    U: Unify<T>,
{
    fn input_patterns(&self) -> Vec<U> {
        self.lhs.clone()
    }

    fn apply_match(&self, bindings: &Bindings<T>) -> Option<Vec<U>> {
        self.solve_constraints(&bindings)
            .and_then(|bindings| self.apply_bindings(&bindings))
    }

    /// Note: MultiRule's implementation of r_apply_match will only return Some result if
    /// a consistent set of bindings can be used when unifying all of the rhs elements
    fn r_apply_match(&self, fact: &U) -> Option<(Vec<U>, Bindings<T>)> {
        utils::fold_while_some(
            (Vec::new(), Bindings::new()),
            &mut self.rhs.iter(),
            &|(mut bound_rhs, bindings), rhs| {
                let results = rhs.unify(fact, &bindings)
                    .and_then(|bindings| self.solve_constraints(&bindings))
                    .and_then(|bindings| {
                        utils::map_while_some(&mut self.lhs.iter(), &|f| {
                            f.apply_bindings(&bindings)
                        }).and_then(|inputs| Some((inputs, bindings)))
                    });
                if let Some((inputs, new_bindings)) = results {
                    bound_rhs.extend(inputs.into_iter());
                    Some((bound_rhs, new_bindings))
                } else {
                    None
                }
            },
        )
    }

    fn constraints<'a>(&'a self) -> Vec<&'a Constraint> {
        self.constraints.iter().collect()
    }

    /// Construct a new version of this rule but with all variables updated to be unique for this invocation
    fn snowflake(&self, suffix: String) -> Self {
        // Gather all variables
        let mut variables: Vec<String> = self.rhs.iter().flat_map(|rhs| rhs.variables()).collect();

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

        let rhs: Vec<U> = self.rhs
            .iter()
            .map(|rhs| rhs.rename_variables(&renamed_variable))
            .collect();
        let lhs: Vec<U> = self.lhs
            .iter()
            .map(|lhs| lhs.rename_variables(&renamed_variable))
            .collect();
        let constraints: Vec<Constraint> = self.constraints
            .iter()
            .map(|constraint| constraint.rename_variables(&renamed_variable))
            .collect();

        MultiRule {
            constraints: constraints,
            lhs: lhs,
            rhs: rhs,
            _marker: PhantomData,
        }
    }
}

impl<T, U> MultiRule<T, U>
where
    T: ConstraintValue,
    U: Unify<T>,
{
    pub fn new(lhs: Vec<U>, rhs: Vec<U>, constraints: Vec<Constraint>) -> Self {
        MultiRule {
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

    pub fn apply_bindings(&self, bindings: &Bindings<T>) -> Option<Vec<U>> {
        utils::map_while_some(&mut self.rhs.iter(), &|rhs| {
            rhs.apply_bindings(bindings)
        })
    }

    fn solve_constraints(&self, bindings: &Bindings<T>) -> Option<Bindings<T>> {
        Constraint::solve_many(self.constraints.iter().collect(), bindings).ok()
    }

    pub fn pprint(&self) -> String {
        let lhs_string: String = self.lhs
            .iter()
            .map(|o| format!("{}", o))
            .collect::<Vec<String>>()
            .join("\n\t\t");
        let rhs_string: String = self.rhs
            .iter()
            .map(|o| format!("{}", o))
            .collect::<Vec<String>>()
            .join("\n\t\t");

        let constraints_string_vec: Vec<String> = self.constraints.iter().map(|o| format!("{}", o)).collect();
        let constraints_string = constraints_string_vec.join("\n");

        format!(
            "MultiRule {{\n\tlhs:\n\t\t{},\n\trhs:\n\t\t{},\n\tconstraints:\n\t\t{} }}",
            lhs_string, rhs_string, constraints_string
        )
    }

    /// Rename any variables in this structure with another variable name
    pub fn rename_variables(&self, renamed_variables: &HashMap<String, String>) -> Self {
        MultiRule::new(
            self.lhs
                .iter()
                .map(|u| u.rename_variables(renamed_variables))
                .collect(),
            self.rhs
                .iter()
                .map(|u| u.rename_variables(renamed_variables))
                .collect(),
            self.constraints
                .iter()
                .map(|c| c.rename_variables(renamed_variables))
                .collect(),
        )
    }
}

impl<T, U> std::fmt::Display for MultiRule<T, U>
where
    T: ConstraintValue,
    U: Unify<T>,
{
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.pprint())
    }
}

impl<T, U> Eq for MultiRule<T, U>
where
    T: ConstraintValue,
    U: Unify<T>,
{
}

#[cfg(test)]
mod tests;
