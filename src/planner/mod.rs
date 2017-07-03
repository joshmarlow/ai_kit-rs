use constraints::Constraint;
use core::{Apply, Bindings, BindingsValue, Unify};
use itertools::Itertools;
use itertools::FoldWhile::{Continue, Done};
use std;
use std::collections::HashSet;
use std::marker::PhantomData;

#[cfg(test)]
mod tests;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum UnificationIndex {
    Init,
    Actor(usize),
    Datum(usize),
    Exhausted,
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

/// Determine the first subgoal to increment
pub fn first_subgoal_to_increment(unification_indices: &Vec<UnificationIndex>) -> Option<usize> {
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

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Goal<T: BindingsValue, U: Unify<T>, A: Apply<T, U>> {
    pub bindings_at_creation: Bindings<T>,
    pub constraints: Vec<Constraint<T>>,
    pub parental_constraints: Vec<Constraint<T>>,
    pub pattern: U,
    pub subgoals: Vec<Goal<T, U, A>>,
    pub unification_index: UnificationIndex,
    _a_marker: PhantomData<A>,
    _t_marker: PhantomData<T>,
}

impl<T: BindingsValue, U: Unify<T>, A: Apply<T, U>> Goal<T, U, A> {
    pub fn new(pattern: U,
               parental_constraints: Vec<Constraint<T>>,
               constraints: Vec<Constraint<T>>,
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

    pub fn constraints(&self) -> Vec<&Constraint<T>> {
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

    /// Construct a mutated plan
    pub fn increment(&self, data: &Vec<&U>, rules: &Vec<&A>, snowflake_prefix_id: usize, max_depth: usize) -> Option<Self> {
        if max_depth == 0 {
            return None;
        }
        let mut goal = self.clone();

        // If there are any subgoals, increment them first
        if !self.subgoals.is_empty() {
            if let Some(subgoals) = self.increment_subgoals(data, rules, snowflake_prefix_id, max_depth) {
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
                           parent_constraints: &Vec<&Constraint<T>>,
                           data: &Vec<&U>,
                           rules: &Vec<&A>,
                           snowflake_prefix_id: usize,
                           max_depth: usize)
                           -> Option<Vec<Self>> {
        rule.r_apply(r_pattern, &Bindings::new()).and_then(|(subgoal_patterns, bindings)| {
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

    pub fn increment_subgoals(&self, data: &Vec<&U>, rules: &Vec<&A>, snowflake_prefix_id: usize, max_depth: usize) -> Option<Vec<Self>> {
        let mut subgoals = self.subgoals.clone();
        let subgoal_count = subgoals.len();
        let is_last_subgoal = |idx| idx + 1 == subgoal_count;
        loop {
            let unification_indices = subgoals.iter().map(|sg| sg.unification_index.clone()).collect();
            let subgoal_idx_to_increment = first_subgoal_to_increment(&unification_indices);
            match subgoal_idx_to_increment {
                None => return None,
                Some(idx) => {
                    if let Some(new_subgoal) = subgoals[idx].increment(data, rules, snowflake_prefix_id, max_depth - 1) {
                        subgoals[idx] = new_subgoal;

                        if is_last_subgoal(idx) {
                            return Some(subgoals);
                        } else {
                            subgoals[idx + 1].unification_index = UnificationIndex::Init;
                        }
                    } else {
                        subgoals[idx].unification_index = UnificationIndex::Exhausted;
                    }
                }
            }
        }
    }

    pub fn pprint(&self, ntabs: usize, only_render_spine: bool) -> String {
        fn concat_tabs(ntabs: usize) -> String {
            let tabv: Vec<String> = (0..ntabs).map(|_| "\t".to_string()).collect();
            tabv.join("")
        }
        let tabs = concat_tabs(ntabs);
        let subgoal_v: Vec<String> = self.subgoals.iter().map(|sg| sg.pprint(ntabs + 1, only_render_spine)).collect();
        let subgoal_s = subgoal_v.join("\n");

        let parental_constraint_v: Vec<String> = self.parental_constraints.iter().map(|c| format!("{}{}", concat_tabs(ntabs + 1), c)).collect();
        let parental_constraint_s = parental_constraint_v.join("\n");

        let constraint_v: Vec<String> = self.constraints.iter().map(|c| format!("{}{}", concat_tabs(ntabs + 1), c)).collect();
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

impl<T: BindingsValue, U: Unify<T>, A: Apply<T, U>> std::fmt::Display for Goal<T, U, A> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "Goal tree:\n{}", self.pprint(1, true))
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct PlanningConfig {
    pub max_depth: usize,
    pub reuse_data: bool,
}

impl Default for PlanningConfig {
    fn default() -> Self {
        PlanningConfig {
            max_depth: 3,
            reuse_data: true,
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum InvalidPlan {
    BindingsConflict,
    ReusedData { idx: usize },
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Planner<'a, T: BindingsValue, U: 'a + Unify<T>, A: 'a + Apply<T, U>> {
    bindings: Bindings<T>,
    config: PlanningConfig,
    data: Vec<&'a U>,
    goal: Goal<T, U, A>,
    max_increments: usize,
    total_increments: usize,
    rules: Vec<&'a A>,
}

impl<'a, T: BindingsValue, U: 'a + Unify<T>, A: 'a + Apply<T, U>> Planner<'a, T, U, A> {
    pub fn new(goal: &Goal<T, U, A>,
               bindings: &Bindings<T>,
               config: &PlanningConfig,
               data: Vec<&'a U>,
               rules: Vec<&'a A>,
               max_increments: usize)
               -> Planner<'a, T, U, A> {
        Planner {
            bindings: bindings.clone(),
            config: config.clone(),
            data: data,
            goal: goal.clone(),
            max_increments: max_increments,
            total_increments: 0,
            rules: rules,
        }
    }

    pub fn validate(&self, goal: &Goal<T, U, A>, config: &PlanningConfig) -> Result<Bindings<T>, InvalidPlan> {
        if !config.reuse_data {
            if let Some(idx) = goal.find_reused_datum(&mut HashSet::new()) {
                return Err(InvalidPlan::ReusedData { idx: idx });
            }
        }
        match goal.satisified(&self.data, &self.rules, &self.bindings) {
            Some(bindings) => Ok(bindings),
            None => Err(InvalidPlan::BindingsConflict),
        }
    }
}

impl<'a, T: BindingsValue, U: Unify<T>, A: Apply<T, U>> Iterator for Planner<'a, T, U, A> {
    type Item = (Goal<T, U, A>, Bindings<T>);

    fn next(&mut self) -> Option<(Goal<T, U, A>, Bindings<T>)> {
        for i in self.total_increments..self.max_increments {
            self.total_increments += 1;

            if let Some(goal) = self.goal.increment(&self.data, &self.rules, i, self.config.max_depth) {
                self.goal = goal;
                match self.validate(&self.goal, &self.config) {
                    Ok(bindings) => return Some((self.goal.clone(), bindings)),
                    Err(_) => continue,
                }
            } else {
                break;
            }
        }
        None
    }
}
