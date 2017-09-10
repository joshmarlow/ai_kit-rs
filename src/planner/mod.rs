//! The planner module implements a basic system for backtracking planner.
//!
//! It supports features like:
//!
//! * specifying a goal with constraints that must be satisfied by the resultant plan.
//!
//! * the ability to solve a conjunction of goal
//!
//! * rendering of a plan in graphviz format for easier visualization
//!

use constraints::{Constraint, ConstraintValue};
use core::{Operation, Bindings, Unify};
use itertools::FoldWhile::{Continue, Done};
use itertools::Itertools;
use std;
use std::collections::HashSet;
use std::marker::PhantomData;
use utils;

#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub enum UnificationIndex {
    #[serde(rename="init")]
    Init,
    #[serde(rename="actor")]
    Actor(usize),
    #[serde(rename="datum")]
    Datum(usize),
    #[serde(rename="exhausted")]
    Exhausted,
}

impl UnificationIndex {
    pub fn datum_idx(&self) -> Option<usize> {
        match *self {
            UnificationIndex::Datum(datum_idx) => Some(datum_idx),
            _ => None,
        }
    }

    pub fn actor_idx(&self) -> Option<usize> {
        match *self {
            UnificationIndex::Actor(actor_idx) => Some(actor_idx),
            _ => None,
        }
    }

    pub fn is_exhausted(&self) -> bool {
        match *self {
            UnificationIndex::Exhausted => true,
            _ => false,
        }
    }

    pub fn is_init(&self) -> bool {
        match *self {
            UnificationIndex::Init => true,
            _ => false,
        }
    }
}

impl std::fmt::Display for UnificationIndex {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match *self {
            UnificationIndex::Init => write!(f, "Init"),
            UnificationIndex::Actor(idx) => write!(f, "Actor({})", idx),
            UnificationIndex::Datum(idx) => write!(f, "Datum({})", idx),
            UnificationIndex::Exhausted => write!(f, "Exhausted"),
        }
    }
}

impl Default for UnificationIndex {
    fn default() -> Self {
        UnificationIndex::Init
    }
}

fn increment_unification_index(current_unification_index: &UnificationIndex, datum_count: usize, rule_count: usize) -> UnificationIndex {
    let initial_rule_index = if rule_count > 0 {
        UnificationIndex::Actor(0)
    } else {
        UnificationIndex::Exhausted
    };

    let initial_datum_index = if datum_count > 0 {
        UnificationIndex::Datum(0)
    } else {
        initial_rule_index.clone()
    };

    match *current_unification_index {
        UnificationIndex::Exhausted => UnificationIndex::Exhausted,
        UnificationIndex::Init => initial_datum_index.clone(),
        UnificationIndex::Datum(current_idx) => {
            if current_idx + 1 < datum_count {
                UnificationIndex::Datum(current_idx + 1)
            } else {
                initial_rule_index
            }
        }
        UnificationIndex::Actor(current_idx) => {
            if current_idx + 1 < rule_count {
                UnificationIndex::Actor(current_idx + 1)
            } else {
                UnificationIndex::Exhausted
            }
        }
    }
}

/// Determine the first goal to increment
pub fn first_goal_to_increment(unification_indices: &Vec<UnificationIndex>) -> Option<usize> {
    if unification_indices.is_empty() {
        None
    } else {
        // Check subgoals find index that is in the Init state
        let last_index = unification_indices.len() - 1;
        let mut idx = 0;

        loop {
            if unification_indices[idx] == UnificationIndex::Init {
                return Some(idx);
            }
            if unification_indices[idx] == UnificationIndex::Exhausted {
                if idx == 0 {
                    return None;
                } else {
                    return Some(idx - 1);
                }
            }
            if idx == last_index {
                return Some(idx);
            }
            idx += 1;
        }
    }
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct Goal<T: ConstraintValue, U: Unify<T>, A: Operation<T, U>> {
    #[serde(default)]
    pub bindings_at_creation: Bindings<T>,
    #[serde(default="Vec::new")]
    pub constraints: Vec<Constraint>,
    #[serde(default="Vec::new")]
    pub parental_constraints: Vec<Constraint>,
    pub pattern: U,
    #[serde(default="Vec::new")]
    pub subgoals: Vec<Goal<T, U, A>>,
    #[serde(default)]
    pub unification_index: UnificationIndex,
    #[serde(default)]
    _a_marker: PhantomData<A>,
    #[serde(default)]
    _t_marker: PhantomData<T>,
}

impl<T, U, A> Goal<T, U, A>
    where T: ConstraintValue,
          U: Unify<T>,
          A: Operation<T, U>
{
    pub fn new(pattern: U,
               parental_constraints: Vec<Constraint>,
               constraints: Vec<Constraint>,
               bindings_at_creation: Bindings<T>,
               unification_index: UnificationIndex,
               subgoals: Vec<Self>)
               -> Self {
        Goal {
            bindings_at_creation: bindings_at_creation,
            constraints: constraints,
            pattern: pattern,
            parental_constraints: parental_constraints,
            subgoals: subgoals,
            unification_index: unification_index,
            _a_marker: PhantomData,
            _t_marker: PhantomData,
        }
    }

    pub fn constraints(&self) -> Vec<&Constraint> {
        self.parental_constraints.iter().chain(self.constraints.iter()).collect()
    }

    pub fn with_pattern(pattern: U) -> Self {
        Goal::new(pattern,
                  Vec::new(),
                  Vec::new(),
                  Bindings::new(),
                  UnificationIndex::Init,
                  Vec::new())
    }

    pub fn solve(goal: &Self, data: &Vec<&U>, rules: &Vec<&A>, increments: usize, config: &PlanningConfig) -> Option<(usize, Self, Bindings<T>)> {
        if increments < config.max_increments {
            goal.increment(&data, &rules, increments, config.max_depth).and_then(|goal| match goal.validate(data, rules, &Bindings::new(), config) {
                Ok(bindings) => Some((increments, goal.clone(), bindings.clone())),
                Err(_) => Goal::solve(&goal, data, rules, increments + 1, config),
            })
        } else {
            None
        }
    }

    pub fn solve_conjunction(goals: Vec<&Self>,
                             data: &Vec<&U>,
                             rules: &Vec<&A>,
                             increments: usize,
                             config: &PlanningConfig)
                             -> Option<(Vec<Self>, Bindings<T>)> {
        if increments < config.max_increments {
            Goal::increment_conjunction(goals, data, rules, increments, config.max_depth).and_then(|goals| {
                let validated_bindings = utils::fold_while_some(Bindings::new(),
                                                                &mut goals.iter(),
                                                                &|bindings, goal| goal.validate(data, rules, &bindings, config).ok());
                match validated_bindings {
                    Some(bindings) => Some((goals, bindings)),
                    None => Goal::solve_conjunction(goals.iter().collect(), data, rules, increments + 1, config),
                }
            })
        } else {
            None
        }
    }

    pub fn solve_conjunction_with_criteria<X>(goals: Vec<&Self>,
                                              data: &Vec<&U>,
                                              rules: &Vec<&A>,
                                              increments: usize,
                                              config: &PlanningConfig,
                                              criteria: &Fn(&Vec<Self>, &Bindings<T>) -> Option<X>)
                                              -> Option<(Vec<Self>, Bindings<T>, X)> {
        if increments < config.max_increments {
            Goal::solve_conjunction(goals, data, rules, increments, config).and_then(|(goals, bindings)| match criteria(&goals, &bindings) {
                Some(result) => Some((goals, bindings, result)),
                None => {
                    Goal::solve_conjunction_with_criteria(goals.iter().collect(),
                                                          data,
                                                          rules,
                                                          increments,
                                                          config,
                                                          criteria)
                }
            })
        } else {
            None
        }
    }

    /// Verify that this plan does not break any of the planning specifications and that it is consistent
    pub fn validate(&self, data: &Vec<&U>, rules: &Vec<&A>, bindings: &Bindings<T>, config: &PlanningConfig) -> Result<Bindings<T>, InvalidPlan> {
        config.validate_plan(self).and_then(|_| match self.satisified(&data, &rules, bindings) {
            Some(bindings) => Ok(bindings),
            None => Err(InvalidPlan::BindingsConflict),
        })
    }

    /// Construct a mutated plan
    pub fn increment(&self, data: &Vec<&U>, rules: &Vec<&A>, snowflake_prefix_id: usize, max_depth: usize) -> Option<Self> {
        if max_depth == 0 {
            return None;
        }
        let mut goal = self.clone();

        // If there are any subgoals, increment them first
        if !self.subgoals.is_empty() {
            if let Some(subgoals) = Goal::increment_conjunction(self.subgoals.iter().collect(),
                                                                data,
                                                                rules,
                                                                snowflake_prefix_id,
                                                                max_depth) {
                goal.subgoals = subgoals;
                return Some(goal);
            }
        }

        // If subgoals cannot be incremented, increment this goal
        loop {
            goal.unification_index = increment_unification_index(&goal.unification_index, data.len(), rules.len());

            // Attempt to increment this goal
            match goal.unification_index {
                UnificationIndex::Datum(_idx) => {
                    if goal.satisified(data, rules, &goal.bindings_at_creation).is_some() {
                        return Some(goal);
                    }
                }
                UnificationIndex::Actor(idx) => {
                    let rule = rules[idx].snowflake(format!("{}", snowflake_prefix_id));

                    if let Some(subgoals) = Self::create_subgoals(&self.pattern,
                                                                  &rule,
                                                                  &goal.constraints(),
                                                                  data,
                                                                  rules,
                                                                  snowflake_prefix_id,
                                                                  max_depth) {
                        goal.subgoals = subgoals;
                        return Some(goal);
                    }
                }
                // If this goal cannot be incremented, return None
                UnificationIndex::Exhausted => return None,
                UnificationIndex::Init => panic!("Init after incrementing; this should never happen"),
            }
        }
    }

    /// Determine if the plan is valid
    pub fn satisified(&self, data: &Vec<&U>, rules: &Vec<&A>, bindings: &Bindings<T>) -> Option<Bindings<T>> {
        let bindings = bindings.merge(&self.bindings_at_creation);
        match self.unification_index {
            UnificationIndex::Datum(datum_idx) => {
                self.pattern
                    .unify(data[datum_idx], &bindings)
                    .and_then(|bindings| Constraint::solve_many(self.constraints(), &bindings).ok())
            }
            UnificationIndex::Actor(_actor_idx) => {
                self.subgoals
                    .iter()
                    .fold_while(Some(bindings),
                                |bindings, subgoal| match subgoal.satisified(data, rules, bindings.as_ref().unwrap()) {
                                    Some(subgoal_bindings) => Continue(Some(subgoal_bindings.clone())),
                                    None => Done(None),
                                })
            }
            UnificationIndex::Init => self.pattern.unify(&U::nil(), &bindings),
            UnificationIndex::Exhausted => None,
        }
    }

    pub fn create_subgoals(r_pattern: &U,
                           rule: &A,
                           parent_constraints: &Vec<&Constraint>,
                           data: &Vec<&U>,
                           rules: &Vec<&A>,
                           snowflake_prefix_id: usize,
                           max_depth: usize)
                           -> Option<Vec<Self>> {
        rule.r_apply_match(r_pattern).and_then(|(subgoal_patterns, bindings)| {
            let subgoals: Vec<Option<Self>> = subgoal_patterns.into_iter()
                .map(|pattern| {
                    Goal::new(pattern.apply_bindings(&bindings).unwrap(),
                              parent_constraints.iter().map(|c| (*c).clone()).collect(),
                              rule.constraints().iter().map(|c| (*c).clone()).collect(),
                              bindings.clone(),
                              UnificationIndex::default(),
                              Vec::new())
                        .increment(data, rules, snowflake_prefix_id + 1, max_depth - 1)
                })
                .collect();

            if subgoals.iter().any(|x| x.is_none()) {
                None
            } else {
                Some(subgoals.into_iter().map(|sg| sg.unwrap()).collect())
            }
        })
    }

    pub fn increment_conjunction(goals: Vec<&Self>,
                                 data: &Vec<&U>,
                                 rules: &Vec<&A>,
                                 snowflake_prefix_id: usize,
                                 max_depth: usize)
                                 -> Option<Vec<Self>> {
        let mut goals: Vec<Self> = goals.into_iter().map(|g| g.clone()).collect();
        let goal_count = goals.len();
        let is_last_goal = |idx| idx + 1 == goal_count;
        loop {
            let unification_indices = goals.iter().map(|sg| sg.unification_index.clone()).collect();
            let goal_idx_to_increment = first_goal_to_increment(&unification_indices);
            match goal_idx_to_increment {
                None => return None,
                Some(idx) => {
                    if let Some(new_goal) = goals[idx].increment(data, rules, snowflake_prefix_id, max_depth - 1) {
                        goals[idx] = new_goal;

                        if is_last_goal(idx) {
                            return Some(goals);
                        } else {
                            goals[idx + 1].unification_index = UnificationIndex::Init;
                        }
                    } else {
                        goals[idx].unification_index = UnificationIndex::Exhausted;
                    }
                }
            }
        }
    }

    pub fn render_as_graphviz(&self) -> String {
        let subtree_string = self.render_subtree_as_graphviz(None);
        format!("graph \"goal tree {}\" {{\n{}\n}}",
                self.pattern,
                subtree_string)
    }

    fn render_subtree_as_graphviz(&self, parent: Option<String>) -> String {
        let goal_rendering = format!("{} [{}]", self.pattern, self.unification_index);
        let subtree_string_vec: Vec<String> = self.subgoals
            .iter()
            .map(|subgoal| subgoal.render_subtree_as_graphviz(Some(goal_rendering.clone())))
            .collect();
        let subtree_string = subtree_string_vec.join("\n");
        let goal_parent_str = if let Some(parent_goal) = parent {
            format!("\"{}\" -- \"{}\";", parent_goal, goal_rendering)
        } else {
            String::new()
        };
        format!("{}\n{}", goal_parent_str, subtree_string)
    }

    pub fn pprint(&self, ntabs: usize, only_render_spine: bool) -> String {
        let tabs = utils::concat_tabs(ntabs);
        let subgoal_v: Vec<String> = self.subgoals.iter().map(|sg| sg.pprint(ntabs + 1, only_render_spine)).collect();
        let subgoal_s = subgoal_v.join("\n");

        let parental_constraint_v: Vec<String> =
            self.parental_constraints.iter().map(|c| format!("{}{}", utils::concat_tabs(ntabs + 1), c)).collect();
        let parental_constraint_s = parental_constraint_v.join("\n");

        let constraint_v: Vec<String> = self.constraints.iter().map(|c| format!("{}{}", utils::concat_tabs(ntabs + 1), c)).collect();
        let constraint_s = constraint_v.join("\n");

        if only_render_spine {
            format!("{}{} @ {}\n{}",
                    tabs,
                    self.pattern,
                    self.unification_index,
                    subgoal_s)
        } else {
            format!("{}{} @ {}\n\t{}bindings at creation: {}\n\t{}parental constraints:\n{}\n\t{}constraints:\n{}\n{}",
                    tabs,
                    self.pattern,
                    self.unification_index,
                    tabs,
                    self.bindings_at_creation,
                    tabs,
                    parental_constraint_s,
                    tabs,
                    constraint_s,
                    subgoal_s)
        }
    }

    pub fn apply_bindings(&self, bindings: &Bindings<T>) -> Option<Self> {
        self.pattern.apply_bindings(&bindings).and_then(|pattern| {
            let mut clone = Goal::new(pattern,
                                      self.parental_constraints.clone(),
                                      self.constraints.clone(),
                                      self.bindings_at_creation.clone(),
                                      self.unification_index.clone(),
                                      Vec::with_capacity(self.subgoals.len()));
            for subgoal in self.subgoals.iter() {
                if let Some(applied_subgoal) = subgoal.apply_bindings(&bindings) {
                    clone.subgoals.push(applied_subgoal);
                } else {
                    return None;
                }
            }
            Some(clone)
        })
    }

    /// Traverse the tree and determine if any datum is being used more than once
    pub fn find_reused_datum(&self, used_data: &mut HashSet<usize>) -> Option<usize> {
        match self.unification_index {
            UnificationIndex::Datum(ref datum_idx) => {
                if used_data.contains(datum_idx) {
                    return Some(datum_idx.clone());
                } else {
                    used_data.insert(datum_idx.clone());
                    return None;
                }
            }
            UnificationIndex::Actor(_actor_idx) => {
                for subgoal in self.subgoals.iter() {
                    if let Some(idx) = subgoal.find_reused_datum(used_data) {
                        return Some(idx);
                    }
                }
                None
            }
            _ => None,
        }
    }

    /// Traverse the goal tree using a depth-first search and gather the leaves of the plan
    pub fn gather_leaves(&self, bindings: &Bindings<T>) -> Vec<U> {
        let mut leaves = Vec::new();

        if self.subgoals.is_empty() {
            leaves.push(self.pattern.apply_bindings(&bindings).expect("Bindings should be applicable"));
        } else {
            for sg in self.subgoals.iter() {
                leaves.extend(sg.gather_leaves(bindings).into_iter());
            }
        }

        leaves
    }
}

impl<T, U, A> std::fmt::Display for Goal<T, U, A>
    where T: ConstraintValue,
          U: Unify<T>,
          A: Operation<T, U>
{
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "Goal tree:\n{}", self.pprint(1, true))
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct PlanningConfig {
    pub max_depth: usize,
    pub max_increments: usize,
    pub reuse_data: bool,
}

impl Default for PlanningConfig {
    fn default() -> Self {
        PlanningConfig {
            max_depth: 3,
            max_increments: 100,
            reuse_data: true,
        }
    }
}

impl PlanningConfig {
    /// Verify that nothing in plan contradicts specifications set in the PlanningConfig
    pub fn validate_plan<T, U, A>(&self, goal: &Goal<T, U, A>) -> Result<(), InvalidPlan>
        where T: ConstraintValue,
              U: Unify<T>,
              A: Operation<T, U>
    {
        if !self.reuse_data {
            if let Some(idx) = goal.find_reused_datum(&mut HashSet::new()) {
                return Err(InvalidPlan::ReusedData { idx: idx });
            }
        }
        Ok(())
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum InvalidPlan {
    BindingsConflict,
    ReusedData { idx: usize },
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Planner<'a, T, U, A>
    where T: ConstraintValue,
          U: 'a + Unify<T>,
          A: 'a + Operation<T, U>
{
    bindings: Bindings<T>,
    config: PlanningConfig,
    data: Vec<&'a U>,
    goal: Goal<T, U, A>,
    total_increments: usize,
    rules: Vec<&'a A>,
}

impl<'a, T, U, A> Planner<'a, T, U, A>
    where T: ConstraintValue,
          U: 'a + Unify<T>,
          A: 'a + Operation<T, U>
{
    pub fn new(goal: &Goal<T, U, A>, bindings: &Bindings<T>, config: &PlanningConfig, data: Vec<&'a U>, rules: Vec<&'a A>) -> Self {
        Planner {
            bindings: bindings.clone(),
            config: config.clone(),
            data: data,
            goal: goal.clone(),
            total_increments: 0,
            rules: rules,
        }
    }
}

impl<'a, T, U, A> Iterator for Planner<'a, T, U, A>
    where T: ConstraintValue,
          U: 'a + Unify<T>,
          A: 'a + Operation<T, U>
{
    type Item = (Goal<T, U, A>, Bindings<T>);

    fn next(&mut self) -> Option<(Goal<T, U, A>, Bindings<T>)> {
        for _i in self.total_increments..self.config.max_increments {
            self.total_increments += 1;

            if let Some((increments, goal, bindings)) =
                Goal::solve(&self.goal,
                            &self.data,
                            &self.rules,
                            self.total_increments,
                            &self.config) {
                self.goal = goal;
                self.total_increments += increments;
                return Some((self.goal.clone(), bindings.clone()));
            } else {
                break;
            }
        }
        None
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ConjunctivePlanner<'a, T, U, A>
    where T: ConstraintValue,
          U: 'a + Unify<T>,
          A: 'a + Operation<T, U>
{
    bindings: Bindings<T>,
    config: PlanningConfig,
    data: Vec<&'a U>,
    goals: Vec<Goal<T, U, A>>,
    total_increments: usize,
    rules: Vec<&'a A>,
}

impl<'a, T, U, A> ConjunctivePlanner<'a, T, U, A>
    where T: ConstraintValue,
          U: 'a + Unify<T>,
          A: 'a + Operation<T, U>
{
    pub fn new(goals: Vec<Goal<T, U, A>>, bindings: &Bindings<T>, config: &PlanningConfig, data: Vec<&'a U>, rules: Vec<&'a A>) -> Self {
        ConjunctivePlanner {
            bindings: bindings.clone(),
            config: config.clone(),
            data: data,
            goals: goals,
            total_increments: 0,
            rules: rules,
        }
    }
}

impl<'a, T, U, A> Iterator for ConjunctivePlanner<'a, T, U, A>
    where T: ConstraintValue,
          U: 'a + Unify<T>,
          A: 'a + Operation<T, U>
{
    type Item = (Vec<Goal<T, U, A>>, Bindings<T>);

    fn next(&mut self) -> Option<(Vec<Goal<T, U, A>>, Bindings<T>)> {
        for _i in self.total_increments..self.config.max_increments {
            self.total_increments += 1;

            if let Some((goals, bindings)) =
                Goal::solve_conjunction(self.goals.iter().collect(),
                                        &self.data,
                                        &self.rules,
                                        self.total_increments,
                                        &self.config) {
                self.goals = goals;
                return Some((self.goals.clone(), bindings.clone()));
            } else {
                break;
            }
        }
        None
    }
}

#[cfg(test)]
mod tests;
