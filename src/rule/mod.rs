use constraints::{Constraint, ConstraintValue};
use core::{Apply, Bindings, Unify};
use serde_json;
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
    _marker: PhantomData<T>,
}

impl<T, U> Apply<T, U> for Rule<T, U>
    where T: ConstraintValue,
          U: Unify<T>
{
    fn arg_count(&self) -> usize {
        self.lhs.len()
    }

    fn apply(&self, facts: &Vec<&U>, bindings: &Bindings<T>) -> Option<(U, Bindings<T>)> {
        self.unify(facts, bindings)
            .and_then(|bindings| self.solve_constraints(&bindings))
            .and_then(|bindings| self.apply_bindings(&bindings).and_then(|rhs| Some((rhs, bindings))))
    }

    fn constraints<'a>(&'a self) -> Vec<&'a Constraint> {
        self.constraints.iter().collect()
    }

    fn r_apply(&self, fact: &U, bindings: &Bindings<T>) -> Option<(Vec<U>, Bindings<T>)> {
        self.rhs
            .apply_bindings(bindings)
            .and_then(|bound_rhs| {
                bound_rhs.unify(&fact, bindings)
                    .and_then(|bindings| self.solve_constraints(&bindings))
                    .and_then(|bindings| {
                        utils::filter_map_all(&mut self.lhs.iter(), &|f| f.apply_bindings(&bindings))
                            .and_then(|bound_lhs| Some((bound_lhs, bindings.clone())))
                    })
            })
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

        let renamed_variable: HashMap<String, String> = variables.into_iter()
            .map(|var| (var.clone(), format!("{}::{}", var, suffix)))
            .collect();

        let rhs = self.rhs.rename_variables(&renamed_variable);
        let lhs: Vec<U> = self.lhs.iter().map(|lhs| lhs.rename_variables(&renamed_variable)).collect();
        let constraints: Vec<Constraint> = self.constraints.iter().map(|constraint| constraint.rename_variables(&renamed_variable)).collect();

        Rule {
            constraints: constraints,
            lhs: lhs,
            rhs: rhs,
            _marker: PhantomData,
        }
    }
}

impl<T, U> Rule<T, U>
    where T: ConstraintValue,
          U: Unify<T>
{
    pub fn unify(&self, facts: &Vec<&U>, bindings: &Bindings<T>) -> Option<Bindings<T>> {
        utils::fold_while_some(bindings.clone(),
                               &mut self.lhs.iter().zip(facts.iter()),
                               &|bindings, (t1, t2)| t1.unify(t2, &bindings))
    }

    pub fn apply_bindings(&self, bindings: &Bindings<T>) -> Option<U> {
        self.rhs.apply_bindings(bindings)
    }

    fn solve_constraints(&self, bindings: &Bindings<T>) -> Option<Bindings<T>> {
        Constraint::solve_many(self.constraints.iter().collect(), bindings).ok()
    }
}

impl<T, U> std::fmt::Display for Rule<T, U>
    where T: ConstraintValue,
          U: Unify<T>
{
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", serde_json::to_string(&self).unwrap())
    }
}

impl<T, U> Eq for Rule<T, U>
    where T: ConstraintValue,
          U: Unify<T>
{
}


#[cfg(test)]
mod tests;
