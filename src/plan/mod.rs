use core::{Apply, Bindings, BindingsValue, Unify};
use itertools::Itertools;
use itertools::FoldWhile::{Continue, Done};
use std;
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

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct PlanningConfig {
    pub debug: bool,
    pub max_depth: usize,
    pub reuse_data: bool,
}

fn increment_unification_index(current_unification_index: &UnificationIndex,
                               datum_count: usize,
                               rule_count: usize)
                               -> UnificationIndex {
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
    pub pattern: U,
    pub subgoals: Vec<Goal<T, U, A>>,
    pub unification_index: UnificationIndex,
    _a_marker: PhantomData<A>,
    _t_marker: PhantomData<T>,
}

impl<T: BindingsValue, U: Unify<T>, A: Apply<T, U>> Goal<T, U, A> {
    pub fn new(pattern: U, unification_index: UnificationIndex, subgoals: Vec<Self>) -> Self {
        Goal {
            pattern: pattern,
            subgoals: subgoals,
            unification_index: unification_index,
            _a_marker: PhantomData,
            _t_marker: PhantomData,
        }
    }

    pub fn with_pattern(pattern: U) -> Self {
        Goal::new(pattern, UnificationIndex::Init, Vec::new())
    }

    /// Construct a mutated plan
    pub fn increment(&self, data: &Vec<&U>, rules: &Vec<&A>, snowflake_prefix_id: usize) -> Option<Self> {
        let mut goal = self.clone();

        // If there are any subgoals, increment them first
        if !self.subgoals.is_empty() {
            if let Some(subgoals) = self.increment_subgoals(data, rules, snowflake_prefix_id) {
                goal.subgoals = subgoals;
                return Some(goal);
            }
        }

        // If subgoals cannot be incrementped, increment this goal
        loop {
            goal.unification_index = increment_unification_index(&goal.unification_index, data.len(), rules.len());
            match goal.unification_index {
                UnificationIndex::Datum(_idx) => return Some(goal),
                UnificationIndex::Actor(idx) => {
                    if let Some(subgoals) = Self::create_subgoals(&self.pattern, idx, rules, snowflake_prefix_id) {
                        goal.subgoals = subgoals;
                        return Some(goal);
                    }
                }
                // If this goal cannot be incrementped, return None
                UnificationIndex::Exhausted => return None,
                UnificationIndex::Init => panic!("Init after incrementing; this should never happen"),
            }
        }
    }

    /// Determine if the plan is valid
    pub fn satisifed(&self, data: &Vec<&U>, bindings: &Bindings<T>) -> Option<Bindings<T>> {
        match self.unification_index {
            UnificationIndex::Datum(datum_idx) => self.pattern.unify(data[datum_idx], bindings),
            UnificationIndex::Actor(_actor_idx) => {
                self.subgoals.iter().fold_while(Some(bindings.clone()), |bindings, subgoal| {
                    let bindings = subgoal.satisifed(data, bindings.as_ref().unwrap());
                    if bindings.is_some() {
                        Continue(bindings)
                    } else {
                        Done(None)
                    }
                })
            }
            UnificationIndex::Init => self.pattern.unify(&U::nil(), &bindings),
            UnificationIndex::Exhausted => None,
        }
    }

    pub fn create_subgoals(r_pattern: &U, idx: usize, rules: &Vec<&A>, snowflake_prefix_id: usize) -> Option<Vec<Self>> {
        let rule = rules[idx].snowflake(format!("{}", snowflake_prefix_id));
        rule.r_apply(r_pattern, &Bindings::new()).and_then(|(subgoal_patterns, bindings)| {
            Some(subgoal_patterns.into_iter()
                .map(|pattern| Goal::with_pattern(pattern.apply_bindings(&bindings).unwrap()))
                .collect())
        })
    }

    pub fn increment_subgoals(&self, data: &Vec<&U>, rules: &Vec<&A>, snowflake_prefix_id: usize) -> Option<Vec<Self>> {
        let unification_indices = self.subgoals.iter().map(|sg| sg.unification_index.clone()).collect();
        first_subgoal_to_increment(&unification_indices).and_then(|idx| {
            self.subgoals[idx].increment(data, rules, snowflake_prefix_id).and_then(|new_subgoal| {
                let mut subgoals = self.subgoals.clone();
                subgoals[idx] = new_subgoal;
                Some(subgoals)
            })
        })
    }

    pub fn pprint(&self, ntabs: usize) -> String {
        let tabv: Vec<String> = (0..ntabs).map(|_| "\t".to_string()).collect();
        let tabs = tabv.join("");
        let subgoal_v: Vec<String> = self.subgoals.iter().map(|sg| sg.pprint(ntabs + 1)).collect();
        let subgoal_s = subgoal_v.join("\n");
        format!("{}{} @ {}\n{}",
                tabs,
                self.pattern,
                self.unification_index,
                subgoal_s)
    }
}

impl<T: BindingsValue, U: Unify<T>, A: Apply<T, U>> std::fmt::Display for Goal<T, U, A> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "Goal tree:\n{}", self.pprint(1))
    }
}
