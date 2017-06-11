use std;
use std::marker::PhantomData;

use constraints::Constraint;
use core::{Apply, Bindings, BindingsValue, ToSexp, Unify};
use pedigree;
use utils;
use simplan::SubgoalUnificationIndex;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct PlanParameters<T: BindingsValue> {
    pub bindings: Bindings<T>,
    pub constraints: Vec<Constraint<T>>,
}

impl<T: BindingsValue> PlanParameters<T> {
    pub fn new() -> Self {
        PlanParameters {
            bindings: Bindings::new(),
            constraints: Vec::new(),
        }
    }
}

impl<T: BindingsValue> std::fmt::Display for PlanParameters<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let constraints_vec: Vec<String> = self.constraints.iter().map(|constraint| constraint.to_sexp_string()).collect();
        write!(f,
               "(plan-parameters bindings: {:?}, constraints: {})",
               self.bindings,
               constraints_vec.join(","),
            )
    }
}

macro_rules! solve_next {
    ($subgoals:expr, $idx:ident, $plan_parameters:ident) => (
        {
            if $idx + 1 < $subgoals.len() {
                $idx += 1;
                $subgoals[$idx].current_plan_parameters = $plan_parameters.clone();
            } else {
                return Some($plan_parameters)
            }
        }
    )
}

macro_rules! backtrack {
    ($planner:ident, $idx:ident) => (
        {
            if $idx == 0 {
                return None
            } else {
                $idx -= 1;
            }
        }
    )
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Subgoal<T: BindingsValue, U: Unify<T>, A: Apply<T, U>> {
    pub current_plan_parameters: PlanParameters<T>,
    pub pattern: U,
    pub subgoals: Vec<Subgoal<T, U, A>>,
    pub unification_index: SubgoalUnificationIndex,
    _marker: PhantomData<A>,
}

impl<T: BindingsValue, U: Unify<T>, A: Apply<T, U>> Subgoal<T, U, A> {
    pub fn new(pattern: U, constraints: &Vec<Constraint<T>>) -> Self {
        Subgoal {
            current_plan_parameters: PlanParameters {
                bindings: Bindings::new(),
                constraints: constraints.clone(),
            },
            pattern: pattern,
            subgoals: Vec::new(),
            unification_index: SubgoalUnificationIndex::Init,
            _marker: PhantomData,
        }
    }

    pub fn set_plan_parameters(&mut self, plan_parameters: &PlanParameters<T>) {
        let mut new_plan_parameters = plan_parameters.clone();
        new_plan_parameters.constraints.extend(self.current_plan_parameters.constraints.clone().into_iter());
        self.current_plan_parameters = new_plan_parameters;
    }

    fn finalize(&self, plan_parameters: &PlanParameters<T>) -> Self {
        let subgoals: Vec<Self> = self.subgoals.iter().map(|sg| sg.finalize(plan_parameters)).collect();
        Subgoal {
            pattern: self.bound_pattern(plan_parameters).expect("Finalize error"),
            subgoals: subgoals,
            current_plan_parameters: plan_parameters.clone(),
            unification_index: self.unification_index.clone(),
            _marker: PhantomData,
        }
    }

    fn bound_pattern(&self, plan_parameters: &PlanParameters<T>) -> Option<U> {
        Constraint::solve_many(plan_parameters.constraints.iter().collect(),
                               &plan_parameters.bindings)
            .ok()
            .and_then(|bindings| self.pattern.apply_bindings(&bindings))
    }

    fn pattern_with_current_parameters(&self) -> U {
        self.bound_pattern(&self.current_plan_parameters).expect("Expected pattern with current parameters to be valid")
    }

    fn increment_unification_index(&mut self, datum_count: usize, rule_count: usize) -> SubgoalUnificationIndex {
        let initial_rule_index = if rule_count > 0 {
            SubgoalUnificationIndex::Actor(0)
        } else {
            SubgoalUnificationIndex::Exhausted
        };

        let initial_datum_index = if datum_count > 0 {
            SubgoalUnificationIndex::Datum(0)
        } else {
            initial_rule_index.clone()
        };

        self.unification_index = match self.unification_index {
            SubgoalUnificationIndex::Exhausted => SubgoalUnificationIndex::Exhausted,
            SubgoalUnificationIndex::Init => initial_datum_index.clone(),
            SubgoalUnificationIndex::Datum(current_idx) => {
                if current_idx + 1 < datum_count {
                    SubgoalUnificationIndex::Datum(current_idx + 1)
                } else {
                    initial_rule_index
                }
            }
            SubgoalUnificationIndex::Actor(current_idx) => {
                if current_idx + 1 < rule_count {
                    SubgoalUnificationIndex::Actor(current_idx + 1)
                } else {
                    SubgoalUnificationIndex::Exhausted
                }
            }
        };
        self.unification_index.clone()
    }

    fn all_subgoals_in_init_state(&self) -> bool {
        self.subgoals
            .iter()
            .map(|subgoal| subgoal.unification_index.clone())
            .all(|state| state == SubgoalUnificationIndex::Init)
    }

    fn all_subgoals_in_exhausted_state(&self) -> bool {
        self.subgoals
            .iter()
            .map(|subgoal| subgoal.unification_index.clone())
            .all(|state| state == SubgoalUnificationIndex::Exhausted)
    }

    fn has_subgoals(&self) -> bool {
        self.subgoals.len() > 0
    }

    fn increment_subgoals_starting_at(&mut self,
                                      planner: &mut Planner<T, U, A>,
                                      idx: usize,
                                      max_depth: usize)
                                      -> Option<PlanParameters<T>> {
        let mut idx = idx;

        loop {
            planner.log(max_depth,
                        format!("Subgoal::increment_subgoals_starting_at: {} {} of {}",
                                self.pattern,
                                idx,
                                self.subgoals.len()));
            match self.subgoals[idx].increment_plan(planner, max_depth) {
                Some(plan_parameters) => solve_next!(self.subgoals, idx, plan_parameters),
                None => backtrack!(planner, idx),
            }
        }
    }

    fn increment_subgoals(&mut self, planner: &mut Planner<T, U, A>, max_depth: usize) -> Option<PlanParameters<T>> {
        // invariant - the status of the subgoals is either:
        // 1) all Init - indicating no solution yet found
        // 2) all Exhausted - indicating no further exploration possible
        // 3) all either Datum(_) or Actor(_) indicating a potential solution
        // if 1, then tick each subgoal forward, backtracking if one becomes Exhausted
        // if 2, increment self
        // if 3, increment last subgoal, backtracking if it becomes Exhausted
        if self.all_subgoals_in_init_state() {
            // 1) all Init
            self.increment_subgoals_starting_at(planner, 0, max_depth - 1)
        } else if self.all_subgoals_in_exhausted_state() {
            // 2) all Exhausted
            None
        } else {
            // 3) not all Init and not all Exhausted, so find the next subgoal solution
            let subgoal_count = self.subgoals.len();
            if subgoal_count > 1 {
                self.increment_subgoals_starting_at(planner, subgoal_count - 1, max_depth - 1)
            } else {
                None
            }
        }
    }

    fn increment_self(&mut self, planner: &mut Planner<T, U, A>) -> Option<PlanParameters<T>> {
        planner.tick_ops();
        match self.increment_unification_index(planner.datum_count(), planner.rule_count()) {
            SubgoalUnificationIndex::Datum(datum_idx) => {
                self.pattern_with_current_parameters()
                    .unify(planner.data[datum_idx],
                           &self.current_plan_parameters.bindings)
                    .and_then(|bindings| {
                        Constraint::solve_many(self.current_plan_parameters.constraints.iter().collect(),
                                               &bindings)
                            .ok()
                            .and_then(|bindings| {
                                let mut plan_parameters = self.current_plan_parameters.clone();
                                plan_parameters.bindings = bindings;
                                Some(plan_parameters)
                            })
                    })
            }
            SubgoalUnificationIndex::Actor(rule_idx) => {
                let rule = planner.actors[rule_idx].snowflake(format!("{}", planner.ops));
                rule.r_apply(&self.pattern_with_current_parameters(),
                             &self.current_plan_parameters.bindings)
                    .and_then(|(subgoal_patterns, bindings)| {
                        let mut updated_constraints = self.current_plan_parameters.constraints.clone();
                        updated_constraints.extend(rule.constraints().into_iter().cloned());

                        Constraint::solve_many(updated_constraints.iter().collect(), &bindings).ok().and_then(|bindings| {
                            let plan_parameters = PlanParameters {
                                bindings: bindings,
                                constraints: updated_constraints,
                            };
                            self.subgoals = subgoal_patterns.into_iter()
                                .map(|subgoal_pattern| {
                                    let mut sg = Subgoal::new(subgoal_pattern.apply_bindings(&plan_parameters.bindings)
                                                                  .unwrap(),
                                                              &plan_parameters.constraints);
                                    sg.set_plan_parameters(&plan_parameters);
                                    sg
                                })
                                .collect();
                            if self.has_subgoals() {
                                None
                            } else {
                                // No subgoals required, return now
                                Some(plan_parameters)
                            }
                        })
                    })
            }
            SubgoalUnificationIndex::Exhausted => None,
            SubgoalUnificationIndex::Init => panic!("WTF"),
        }
    }

    fn increment_plan(&mut self, planner: &mut Planner<T, U, A>, max_depth: usize) -> Option<PlanParameters<T>> {
        if max_depth == 0 {
            planner.log(max_depth,
                        format!("Subgoal::increment_plan {} bottoming out", self.pattern));
            return None;
        }
        planner.log(max_depth,
                    format!("Subgoal::increment_plan {}, current unification_index: {}",
                            self.pattern,
                            self.unification_index));
        loop {
            if self.unification_index == SubgoalUnificationIndex::Exhausted {
                planner.log(max_depth,
                            format!("Subgoal::increment_plan {}, EXHAUSTED", self.pattern));
                return None;
            }
            // If there are subgoals, attempt to solve the tree by updating subgoals
            if self.has_subgoals() {
                let plan = self.increment_subgoals(planner, max_depth);
                planner.log(max_depth,
                            format!("Subgoal::increment_plan {}, got plan? {}",
                                    self.pattern,
                                    plan.is_some()));
                if plan.is_some() {
                    return plan;
                }
            }
            // No purely-subgoal based solution has been found so clear subgoals from any previous match
            // and increment this goal
            self.subgoals.clear();

            let plan = self.increment_self(planner);
            planner.log(max_depth,
                        format!("Subgoal::increment_plan {}, {}",
                                self.pattern,
                                self.unification_index));
            if plan.is_some() {
                planner.log(max_depth,
                            format!("Subgoal::increment_plan {} got plan", self.pattern));
                return plan;
            }
        }
    }

    fn extract_plan(&self,
                    planner: &Planner<T, U, A>,
                    bindings: &Bindings<T>,
                    depth: usize)
                    -> Option<(String, Vec<(String, U, pedigree::Origin)>)> {
        match self.unification_index {
            // Plan was not yet constructed
            SubgoalUnificationIndex::Init => None,
            // No plan can be constructed
            SubgoalUnificationIndex::Exhausted => None,
            // Goal was already in the provided data, or was already computed, so do not add it to the list of new data
            SubgoalUnificationIndex::Datum(datum_idx) => Some((planner.data_ids[datum_idx].clone(), Vec::new())),
            // Goal was constructed from an rule, so add it to the list of new data
            SubgoalUnificationIndex::Actor(rule_idx) => {
                utils::fold_while_some((Vec::new(), Vec::new()),
                                       &mut self.subgoals.iter(),
                                       &|(mut new_data, mut parent_arg_ids), subgoal| {
                    subgoal.extract_plan(planner, &bindings, depth + 1)
                        .and_then(|(subgoal_id, new_data_for_subgoal)| {
                            parent_arg_ids.push(subgoal_id);
                            new_data.extend(new_data_for_subgoal.into_iter());
                            Some((new_data, parent_arg_ids))
                        })
                })
                    .and_then(|(mut new_data, parent_arg_ids)| {
                        let goal_id = format!("{}::datum::{}",
                                              planner.prefix,
                                              planner.datum_count() + new_data.len());
                        new_data.push((goal_id.clone(),
                                       self.pattern.apply_bindings(bindings).expect("extract_plan - Should work"),
                                       pedigree::Origin {
                                           source_id: planner.actor_ids[rule_idx].clone(),
                                           args: parent_arg_ids,
                                       }));

                        Some((goal_id, new_data))
                    })
            }
        }
    }

    pub fn render_goal_tree(&self, plan_parameters: &PlanParameters<T>) -> String {
        let subtree_string = self.render_subtree(plan_parameters, None);
        format!("graph \"goal tree {}\" {{\n{}\n}}",
                self.pattern,
                subtree_string)
    }

    pub fn render_subtree(&self, plan_parameters: &PlanParameters<T>, parent: Option<String>) -> String {
        let goal_rendering = format!("{}/{} [{}]",
                                     self.pattern,
                                     self.bound_pattern(plan_parameters).unwrap(),
                                     self.unification_index);
        let subtree_string_vec: Vec<String> = self.subgoals
            .iter()
            .map(|subgoal| subgoal.render_subtree(plan_parameters, Some(goal_rendering.clone())))
            .collect();
        let subtree_string = subtree_string_vec.join("\n");
        let goal_parent_str = if let Some(parent_goal) = parent {
            format!("\"{}\" -- \"{}\";",
                    parent_goal,
                    goal_rendering,
                    )
        } else {
            String::new()
        };
        format!("{}\n{}", goal_parent_str, subtree_string)
    }
}

impl<T: BindingsValue, U: Unify<T>, A: Apply<T, U>> std::fmt::Display for Subgoal<T, U, A> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f,
               "(Subgoal pattern: {}, index: {})",
               self.pattern,
               self.unification_index)
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Plan<T: BindingsValue, U: Unify<T>, A: Apply<T, U>> {
    pub elements: Vec<(String, U, pedigree::Origin)>,
    pub parameters: PlanParameters<T>,
    pub root: Subgoal<T, U, A>,
    pub root_id: String,
}

impl<T: BindingsValue, U: Unify<T>, A: Apply<T, U>> Plan<T, U, A> {
    pub fn new() -> Self {
        Plan {
            elements: Vec::new(),
            parameters: PlanParameters::new(),
            root: Subgoal::new(U::nil(), &Vec::new()),
            root_id: String::new(),
        }
    }
}

#[derive(Clone, Debug, Default, Eq, Ord, PartialEq, PartialOrd)]
pub struct PlannerConfig {
    pub max_depth: usize,
    pub reuse_data: bool,
}

impl PlannerConfig {
    pub fn new() -> Self {
        PlannerConfig {
            max_depth: 5,
            reuse_data: true,
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Planner<'a, T: BindingsValue, U: 'a + Unify<T>, A: 'a + Apply<T, U>> {
    pub config: PlannerConfig,
    pub data_ids: Vec<&'a String>,
    pub data: Vec<&'a U>,
    pub actor_ids: Vec<&'a String>,
    pub actors: Vec<&'a A>,
    pub goal: Subgoal<T, U, A>,
    pub prefix: String,
    pub ops: usize,
    debug: bool,
}

impl<'a, T: BindingsValue, U: Unify<T>, A: Apply<T, U>> Planner<'a, T, U, A> {
    fn log(&self, depth: usize, msg: String) {
        if self.debug {
            println!("[{:?}] {}", self.config.max_depth - depth, msg);
        }
    }

    fn tick_ops(&mut self) {
        self.ops += 1;
    }

    fn construct_next_plan(&mut self) -> Option<Plan<T, U, A>> {
        let max_depth = self.config.max_depth;
        let mut goal = self.goal.clone();
        println!("Constructing next plan");
        let result = goal.increment_plan(self, max_depth);
        println!("Constructed? {}", result.is_some());
        self.goal = goal;
        result.and_then(|plan_parameters| {
            Constraint::solve_many(plan_parameters.constraints.iter().collect(),
                                   &plan_parameters.bindings)
                .ok()
                .and_then(|bindings| {
                    self.goal.extract_plan(self, &bindings, 0).and_then(|(root_id, elements)| {
                        let plan_parameters = PlanParameters {
                            bindings: bindings,
                            constraints: plan_parameters.constraints.clone(),
                        };
                        Some(Plan {
                            elements: elements,
                            root: self.goal.finalize(&plan_parameters),
                            root_id: root_id,
                            parameters: plan_parameters,
                        })
                    })
                })
        })
    }

    pub fn datum_count(&self) -> usize {
        self.data.len()
    }

    pub fn rule_count(&self) -> usize {
        self.actors.len()
    }
}

impl<'a, T: BindingsValue, U: Unify<T>, A: Apply<T, U>> Iterator for Planner<'a, T, U, A> {
    type Item = Plan<T, U, A>;

    fn next(&mut self) -> Option<Plan<T, U, A>> {
        self.construct_next_plan()
    }
}

#[cfg(test)]
mod tests {
    use constraints;
    use core;
    use core::ToSexp;
    use datum::Datum;
    use infer;

    type Rule = infer::Rule<Datum, Datum>;
    type Bindings = core::Bindings<Datum>;
    type Constraint = constraints::Constraint<Datum>;

    pub fn setup_rules() -> (Vec<Rule>, Vec<String>) {
        let physics_rule = Rule::from_sexp_str("(defrule (((timeline-entry ((current-state ?s1) ((time ?t1)) \
                                                  ()))
                        (timeline-entry ((action 2) ((time ?t1)) \
                                                  ())))
                      (timeline-entry ((current-state ?s2) ((time \
                                                  ?t2)) ()))
                      ((constraint-set (?diff1 1))
                       \
                                                  (constraint-set (?diff2 2))
                       (constraint-sum (?s1 \
                                                  ?diff2 ?s2))
                       (constraint-sum (?t1 ?diff1 ?t2)))))")
            .unwrap();
        let physics_rule_2 = Rule::from_sexp_str("(defrule (((timeline-entry ((current-state ?s0) ((time ?t0)) ()))
                                          \
                                                   (timeline-entry ((action 1) ((time ?t0)) ())))
                                      \
                                                   (timeline-entry ((current-state ?s1) ((time ?t1)) ()))
                                      \
                                                   ((constraint-set (?diff1 1))
                                       \
                                                   (constraint-sum (?s0 ?diff1 ?s1))
                                       \
                                                   (constraint-sum (?t0 ?diff1 ?t1)))))")
            .unwrap();
        let interaction_model_add_2 = Rule::from_sexp_str("(defrule (() (timeline-entry ((action 2) ((time ?t)) ())) ()))")
            .unwrap();

        let interaction_model_add_1 = Rule::from_sexp_str("(defrule (() (timeline-entry ((action 1) ((time ?t)) ())) ()))")
            .unwrap();

        return (vec![physics_rule, interaction_model_add_2, physics_rule_2, interaction_model_add_1],
                vec!["physics_rule".to_string(),
                     "interaction_model_add_2".to_string(),
                     "physics_rule_2".to_string(),
                     "interaction_model_add_1".to_string()]);
    }

    fn setup() -> (Vec<Rule>, Vec<Datum>, Vec<String>, Vec<String>) {
        let initial_state = Datum::from_sexp_str("(timeline-entry ((current-state 0) ((time 1)) ()))").unwrap();

        let data: Vec<Datum> = vec![initial_state];
        let data_ids = vec!["initial_state".to_string()];

        let (rules, rule_ids) = setup_rules();
        (rules, data, rule_ids, data_ids)
    }


    #[cfg(test)]
    mod increment_unification_index_tests {
        use std::marker::PhantomData;
        use core::ToSexp;
        use super::super::{PlanParameters, Subgoal, SubgoalUnificationIndex};
        use datum::Datum;

        #[test]
        fn test_new_subgoal_is_in_init() {
            let subgoal: Subgoal<Datum, Datum, super::Rule> =
                Subgoal::new(Datum::from_sexp_str(&"(timeline-entry ((current-state 0) ((time 0)) ()))").unwrap(),
                             &Vec::new());
            assert_eq!(subgoal.unification_index, SubgoalUnificationIndex::Init);
        }

        #[test]
        fn test_initial_value_transitions_to_first_datum() {
            let mut subgoal: Subgoal<Datum, Datum, super::Rule> = Subgoal {
                current_plan_parameters: PlanParameters::new(),
                pattern: Datum::from_sexp_str(&"(timeline-entry ((current-state 0) ((time 0)) ()))").unwrap(),
                subgoals: Vec::new(),
                unification_index: SubgoalUnificationIndex::Init,
                _marker: PhantomData,
            };
            assert_eq!(subgoal.increment_unification_index(1, 1),
                       SubgoalUnificationIndex::Datum(0));
            assert_eq!(subgoal.unification_index, SubgoalUnificationIndex::Datum(0));
        }

        #[test]
        fn test_last_datum_transitions_to_first_rule() {
            let mut subgoal: Subgoal<Datum, Datum, super::Rule> = Subgoal {
                current_plan_parameters: PlanParameters::new(),
                pattern: Datum::from_sexp_str(&"(timeline-entry ((current-state 0) ((time 0)) ()))").unwrap(),
                subgoals: Vec::new(),
                unification_index: SubgoalUnificationIndex::Datum(0),
                _marker: PhantomData,
            };
            assert_eq!(subgoal.increment_unification_index(1, 1),
                       SubgoalUnificationIndex::Actor(0));
            assert_eq!(subgoal.unification_index, SubgoalUnificationIndex::Actor(0));
        }

        #[test]
        fn test_last_rule_transitions_to_exhausted() {
            let mut subgoal: Subgoal<Datum, Datum, super::Rule> = Subgoal {
                current_plan_parameters: PlanParameters::new(),
                pattern: Datum::from_sexp_str(&"(timeline-entry ((current-state 0) ((time 0)) ()))").unwrap(),
                subgoals: Vec::new(),
                unification_index: SubgoalUnificationIndex::Actor(0),
                _marker: PhantomData,
            };
            assert_eq!(subgoal.increment_unification_index(1, 1),
                       SubgoalUnificationIndex::Exhausted);
            assert_eq!(subgoal.unification_index,
                       SubgoalUnificationIndex::Exhausted);
        }
    }

    #[cfg(test)]
    mod subgoal_tests {
        use std::marker::PhantomData;
        use core::ToSexp;
        use datum::Datum;
        use pedigree::Origin;
        use super::super::{Planner, PlannerConfig, PlanParameters, Subgoal, SubgoalUnificationIndex};
        use super::{Rule, Bindings};

        fn assert_subgoal_trees_equivalent(actual: &Subgoal<Datum, Datum, Rule>, expected: &Subgoal<Datum, Datum, Rule>) {
            assert_eq!(actual.pattern,
                       expected.pattern,
                       "\nSubgoal pattern mismatch - expected:\n{}\nactual:\n{}\n\n",
                       expected.pattern,
                       actual.pattern);
            assert_eq!(actual.unification_index,
                       expected.unification_index,
                       "\n\nSubgoal unification_index mismatch - expected:\n{}\nactual:\n{}\n\n",
                       expected.unification_index,
                       actual.unification_index);
            assert_eq!(actual.subgoals.len(), expected.subgoals.len());
            for idx in 0..actual.subgoals.len() {
                assert_subgoal_trees_equivalent(&actual.subgoals[idx], &expected.subgoals[idx])
            }
        }

        #[test]
        fn test_initial_value_transitions_to_first_datum() {
            let mut goal = Subgoal {
                current_plan_parameters: PlanParameters::new(),
                pattern: Datum::from_sexp_str(&"(timeline-entry ((current-state 0) ((time ?t)) ()))").unwrap(),
                subgoals: Vec::new(),
                unification_index: SubgoalUnificationIndex::Init,
                _marker: PhantomData,
            };
            let (rules, data, rule_ids, data_ids) = super::setup();
            let mut plan_iterator = Planner {
                actors: rules.iter().collect(),
                actor_ids: rule_ids.iter().collect(),
                config: PlannerConfig::new(),
                data_ids: data_ids.iter().collect(),
                data: data.iter().collect(),
                prefix: "test".to_string(),
                goal: goal.clone(),
                ops: 0,
                debug: false,
            };

            let parameters = goal.increment_self(&mut plan_iterator);
            assert_eq!(plan_iterator.ops, 1);
            assert_eq!(parameters,
                       Some(PlanParameters {
                           bindings: Bindings::new().set_binding(&"?t".to_string(), Datum::from_float(1.0)),
                           constraints: Vec::new(),
                       }));
            assert_eq!(goal.unification_index, SubgoalUnificationIndex::Datum(0));
        }

        #[test]
        fn test_last_datum_transitions_to_first_rule() {
            let mut goal = Subgoal {
                current_plan_parameters: PlanParameters::new(),
                pattern: Datum::from_sexp_str(&"(timeline-entry ((current-state 2) ((time ?t)) ()))").unwrap(),
                subgoals: Vec::new(),
                unification_index: SubgoalUnificationIndex::Datum(0),
                _marker: PhantomData,
            };
            let (rules, data, rule_ids, data_ids) = super::setup();
            let mut plan_iterator = Planner {
                actors: rules.iter().collect(),
                actor_ids: rule_ids.iter().collect(),
                config: PlannerConfig::new(),
                data_ids: data_ids.iter().collect(),
                data: data.iter().collect(),
                prefix: "test".to_string(),
                goal: goal.clone(),
                ops: 1,
                debug: false,
            };

            let mut expected_goal = goal.clone();
            expected_goal.unification_index = SubgoalUnificationIndex::Actor(0);
            expected_goal.subgoals =
                vec![Subgoal {
                         current_plan_parameters: PlanParameters::new(),
                         pattern: Datum::from_sexp_str(&"(timeline-entry ((current-state 0) ((time ?t1::2)) ()))")
                             .expect("Parsing expected state goal"),
                         subgoals: Vec::new(),
                         unification_index: SubgoalUnificationIndex::Init,
                         _marker: PhantomData,
                     },
                     Subgoal {
                         current_plan_parameters: PlanParameters::new(),
                         pattern: Datum::from_sexp_str(&"(timeline-entry ((action 2) ((time ?t1::2)) ()))")
                             .expect("Parsing expected action goal"),
                         subgoals: Vec::new(),
                         unification_index: SubgoalUnificationIndex::Init,
                         _marker: PhantomData,
                     }];

            let parameters = goal.increment_self(&mut plan_iterator);

            assert_eq!(parameters, None);
            assert_eq!(plan_iterator.ops, 2);
            assert_subgoal_trees_equivalent(&goal, &expected_goal);
        }

        #[test]
        fn test_last_rule_transitions_to_exhausted() {
            let mut goal = Subgoal {
                current_plan_parameters: PlanParameters::new(),
                pattern: Datum::from_sexp_str(&"(timeline-entry ((current-state 2) ((time ?t)) ()))").unwrap(),
                subgoals: Vec::new(),
                unification_index: SubgoalUnificationIndex::Actor(3),
                _marker: PhantomData,
            };
            let (rules, data, rule_ids, data_ids) = super::setup();
            let mut plan_iterator = Planner {
                actors: rules.iter().collect(),
                actor_ids: rule_ids.iter().collect(),
                config: PlannerConfig::new(),
                data_ids: data_ids.iter().collect(),
                data: data.iter().collect(),
                prefix: "test".to_string(),
                goal: goal.clone(),
                ops: 5,
                debug: false,
            };

            let parameters = goal.increment_self(&mut plan_iterator);
            assert_eq!(parameters, None);
            assert_eq!(plan_iterator.ops, 6);
            assert_eq!(goal.unification_index, SubgoalUnificationIndex::Exhausted);
        }

        #[test]
        fn test_exhausted_remains_exhausted() {
            let mut goal = Subgoal {
                current_plan_parameters: PlanParameters::new(),
                pattern: Datum::from_sexp_str(&"(timeline-entry ((current-state 0) ((time ?t)) ()))").unwrap(),
                subgoals: Vec::new(),
                unification_index: SubgoalUnificationIndex::Exhausted,
                _marker: PhantomData,
            };
            let (rules, data, rule_ids, data_ids) = super::setup();
            let mut plan_iterator = Planner {
                actors: rules.iter().collect(),
                actor_ids: rule_ids.iter().collect(),
                config: PlannerConfig::new(),
                data_ids: data_ids.iter().collect(),
                data: data.iter().collect(),
                prefix: "test".to_string(),
                goal: goal.clone(),
                ops: 0,
                debug: false,
            };

            let parameters = goal.increment_self(&mut plan_iterator);
            assert_eq!(parameters, None);
            assert_eq!(goal.unification_index, SubgoalUnificationIndex::Exhausted);
        }

        #[test]
        fn test_increment_subgoals_starting_at_0_when_all_init() {
            let expected_return_parameters = PlanParameters {
                bindings: Bindings::new()
                    .set_binding(&"?t1::1".to_string(), Datum::from_float(1.0))
                    .set_binding(&"?t::6".to_string(), Datum::from_float(1.0)),
                constraints: Vec::new(),
            };

            let mut goal = Subgoal {
                current_plan_parameters: PlanParameters::new(),
                pattern: Datum::from_sexp_str(&"(timeline-entry ((current-state 2) ((time ?t)) ()))").unwrap(),
                unification_index: SubgoalUnificationIndex::Actor(0),
                subgoals: vec![Subgoal {
                                   current_plan_parameters: PlanParameters::new(),
                                   pattern: Datum::from_sexp_str(&"(timeline-entry ((current-state 0) ((time \
                                                                             ?t1::1)) ()))")
                                       .expect("Parsing expected state goal"),
                                   subgoals: Vec::new(),
                                   unification_index: SubgoalUnificationIndex::Init,
                                   _marker: PhantomData,
                               },
                               Subgoal {
                                   current_plan_parameters: PlanParameters::new(),
                                   pattern: Datum::from_sexp_str(&"(timeline-entry ((action 2) ((time ?t1::1)) ()))")
                                       .expect("Parsing expected action goal"),
                                   subgoals: Vec::new(),
                                   unification_index: SubgoalUnificationIndex::Init,
                                   _marker: PhantomData,
                               }],
                _marker: PhantomData,
            };
            let (rules, data, rule_ids, data_ids) = super::setup();
            let mut plan_iterator = Planner {
                actors: rules.iter().collect(),
                actor_ids: rule_ids.iter().collect(),
                config: PlannerConfig::new(),
                data_ids: data_ids.iter().collect(),
                data: data.iter().collect(),
                prefix: "test".to_string(),
                goal: goal.clone(),
                ops: 2,
                debug: false,
            };

            let mut expected_goal = goal.clone();
            expected_goal.subgoals =
                vec![Subgoal {
                         current_plan_parameters: PlanParameters::new(),
                         pattern: Datum::from_sexp_str(&"(timeline-entry ((current-state 0) ((time ?t1::1)) ()))")
                             .expect("Parsing expected state goal"),
                         subgoals: Vec::new(),
                         unification_index: SubgoalUnificationIndex::Datum(0),
                         _marker: PhantomData,
                     },
                     Subgoal {
                         current_plan_parameters: PlanParameters::new(),
                         pattern: Datum::from_sexp_str(&"(timeline-entry ((action 2) ((time ?t1::1)) ()))")
                             .expect("Parsing expected action goal"),
                         subgoals: Vec::new(),
                         unification_index: SubgoalUnificationIndex::Actor(1),
                         _marker: PhantomData,
                     }];

            let actual_parameters = goal.increment_subgoals_starting_at(&mut plan_iterator, 0, 3);
            assert_subgoal_trees_equivalent(&goal, &expected_goal);
            assert_eq!(actual_parameters, Some(expected_return_parameters));
        }

        #[test]
        fn test_increment_subgoals_when_all_exhuasted() {
            let mut goal = Subgoal {
                current_plan_parameters: PlanParameters::new(),
                pattern: Datum::from_sexp_str(&"(timeline-entry ((current-state 2) ((time ?t)) ()))").unwrap(),
                unification_index: SubgoalUnificationIndex::Actor(0),
                _marker: PhantomData,
                subgoals: vec![Subgoal {
                                   current_plan_parameters: PlanParameters::new(),
                                   pattern: Datum::from_sexp_str(&"(timeline-entry ((current-state 0) ((time \
                                                                             ?t1::1)) ()))")
                                       .expect("Parsing expected state goal"),
                                   subgoals: Vec::new(),
                                   unification_index: SubgoalUnificationIndex::Exhausted,
                                   _marker: PhantomData,
                               },
                               Subgoal {
                                   current_plan_parameters: PlanParameters::new(),
                                   pattern: Datum::from_sexp_str(&"(timeline-entry ((action 2) ((time ?t1::1)) ()))")
                                       .expect("Parsing expected action goal"),
                                   subgoals: Vec::new(),
                                   unification_index: SubgoalUnificationIndex::Exhausted,
                                   _marker: PhantomData,
                               }],
            };
            let (rules, data, rule_ids, data_ids) = super::setup();
            let mut plan_iterator = Planner {
                actors: rules.iter().collect(),
                actor_ids: rule_ids.iter().collect(),
                config: PlannerConfig::new(),
                data_ids: data_ids.iter().collect(),
                data: data.iter().collect(),
                prefix: "test".to_string(),
                goal: goal.clone(),
                ops: 27,
                debug: false,
            };

            let mut expected_goal = goal.clone();
            expected_goal.subgoals =
                vec![Subgoal {
                         current_plan_parameters: PlanParameters::new(),
                         pattern: Datum::from_sexp_str(&"(timeline-entry ((current-state 0) ((time ?t1::1)) ()))")
                             .expect("Parsing expected state goal"),
                         subgoals: Vec::new(),
                         unification_index: SubgoalUnificationIndex::Exhausted,
                         _marker: PhantomData,
                     },
                     Subgoal {
                         current_plan_parameters: PlanParameters::new(),
                         pattern: Datum::from_sexp_str(&"(timeline-entry ((action 2) ((time ?t1::1)) ()))")
                             .expect("Parsing expected action goal"),
                         subgoals: Vec::new(),
                         unification_index: SubgoalUnificationIndex::Exhausted,
                         _marker: PhantomData,
                     }];

            let actual_parameters = goal.increment_subgoals(&mut plan_iterator, 3);
            assert_eq!(plan_iterator.ops, 27);
            assert_subgoal_trees_equivalent(&goal, &expected_goal);
            assert_eq!(actual_parameters, None);
        }

        #[test]
        fn test_extract_plan() {
            let (rules, data, rule_ids, data_ids) = super::setup();
            let initial_goal_pattern = Datum::from_sexp_str("(timeline-entry ((current-state 2) ((time ?t2)) ()))").unwrap();
            let expected_goal_pattern = Datum::from_sexp_str("(timeline-entry ((current-state 2) ((time 2)) ()))").unwrap();

            let goal_bindings: Bindings = vec![("?s1".to_string(), Datum::from_float(0.0)),
                                               ("?s2".to_string(), Datum::from_float(2.0)),
                                               ("?t1".to_string(), Datum::from_float(1.0)),
                                               ("?t2".to_string(), Datum::from_float(2.0)),
                                               ("?_t2".to_string(), Datum::from_float(2.0)),
                                               ("?diff1".to_string(), Datum::from_float(1.0)),
                                               ("?diff2".to_string(), Datum::from_float(2.0)),
                                               ("?_t2".to_string(), Datum::from_variable("?t2".to_string()))]
                .into_iter()
                .collect();

            let plan_iterator = Planner {
                actors: rules.iter().collect(),
                actor_ids: rule_ids.iter().collect(),
                config: PlannerConfig::new(),
                data_ids: data_ids.iter().collect(),
                data: data.iter().collect(),
                prefix: "test".to_string(),
                ops: 0,
                debug: false,
                goal: Subgoal {
                    current_plan_parameters: PlanParameters::new(),
                    unification_index: SubgoalUnificationIndex::Actor(0),
                    pattern: initial_goal_pattern.clone(),
                    _marker: PhantomData,
                    subgoals: vec![Subgoal {
                                       current_plan_parameters: PlanParameters::new(),
                                       pattern: Datum::from_sexp_str(&"(timeline-entry ((current-state 0) ((time \
                                                                                 1)) ()))")
                                           .unwrap(),
                                       subgoals: vec![],
                                       unification_index: SubgoalUnificationIndex::Datum(0),
                                       _marker: PhantomData,
                                   },
                                   Subgoal {
                                       current_plan_parameters: PlanParameters::new(),
                                       pattern: Datum::from_sexp_str(&"(timeline-entry ((action 2) ((time 1)) ()))").unwrap(),
                                       subgoals: vec![],
                                       unification_index: SubgoalUnificationIndex::Actor(1),
                                       _marker: PhantomData,
                                   }],
                },
            };

            let expected_plan: (String, Vec<(String, Datum, Origin)>) =
                ("test::datum::2".to_string(),
                 vec![("test::datum::1".to_string(),
                       Datum::from_sexp_str(&"(timeline-entry ((action 2) ((time 1)) ()))").unwrap(),
                       Origin {
                           source_id: "interaction_model_add_2".to_string(),
                           args: vec![],
                       }),
                      ("test::datum::2".to_string(),
                       expected_goal_pattern,
                       Origin {
                           source_id: "physics_rule".to_string(),
                           args: vec!["initial_state".to_string(), "test::datum::1".to_string()],
                       })]);
            let actual_plan = plan_iterator.goal.extract_plan(&plan_iterator, &goal_bindings, 0);
            assert_eq!(actual_plan, Some(expected_plan));
        }
    }

    #[cfg(test)]
    mod plan_iterator_tests {
        use core::ToSexp;
        use super::super::{Planner, PlannerConfig, Subgoal, SubgoalUnificationIndex};
        use super::Datum;

        #[test]
        fn test_construct_next_plan() {
            let (rules, data, rule_ids, data_ids) = super::setup();
            let expected_goal_1_pattern = Datum::from_sexp_str("(timeline-entry ((current-state 2) ((time 2)) ()))").unwrap();
            let expected_goal_2_pattern = Datum::from_sexp_str("(timeline-entry ((current-state 2) ((time 3)) ()))").unwrap();

            let mut plan_iterator = Planner {
                actors: rules.iter().collect(),
                actor_ids: rule_ids.iter().collect(),
                config: PlannerConfig::new(),
                data_ids: data_ids.iter().collect(),
                data: data.iter().collect(),
                prefix: "test".to_string(),
                ops: 0,
                goal: Subgoal::new(Datum::from_sexp_str("(timeline-entry ((current-state 2) ((time ?t2)) ()))").unwrap(),
                                   &Vec::new()),
                debug: false,
            };

            let plan = plan_iterator.construct_next_plan().expect("Expected initial plan to be constructed");
            assert_eq!(plan_iterator.goal.unification_index,
                       SubgoalUnificationIndex::Actor(0));
            assert_eq!(plan_iterator.goal.bound_pattern(&plan.parameters).unwrap(),
                       expected_goal_1_pattern.clone(),
                       "Planner goal.pattern is not as expected");

            // Check the next element of the iterator
            let plan = plan_iterator.construct_next_plan().expect("Expected second plan to be constructed");
            assert_eq!(plan_iterator.goal.unification_index,
                       SubgoalUnificationIndex::Actor(2));
            assert_eq!(plan_iterator.goal.bound_pattern(&plan.parameters).unwrap(),
                       expected_goal_2_pattern,
                       "Planner goal.pattern is not as expected");

            // There should be no further plans possible
            assert_eq!(plan_iterator.construct_next_plan().is_some(),
                       false,
                       "Expected no further plan to be constructed");
        }

        #[test]
        fn test_construct_next_plan_with_goal_constraint() {
            let (rules, data, rule_ids, data_ids) = super::setup();
            let constraints: Vec<super::Constraint> = vec!["(constraint-set (?min_time 2))",
                                                           "(constraint-greater-than (?t2 ?min_time))"]
                .into_iter()
                .map(|s| super::Constraint::from_sexp_str(s).unwrap())
                .collect();
            let goal_pattern = Datum::from_sexp_str("(timeline-entry ((current-state 2) ((time ?t2)) ()))").unwrap();

            let mut plan_iterator = Planner {
                actors: rules.iter().collect(),
                actor_ids: rule_ids.iter().collect(),
                config: PlannerConfig::new(),
                data_ids: data_ids.iter().collect(),
                data: data.iter().collect(),
                prefix: "test".to_string(),
                ops: 0,
                goal: Subgoal::new(goal_pattern.clone(), &constraints),
                debug: false,
            };

            let plan = plan_iterator.construct_next_plan().expect("Expected plan to be constructed");
            let expected_goal_pattern = Datum::from_sexp_str("(timeline-entry ((current-state 2) ((time 3)) ()))").unwrap();

            assert_eq!(plan_iterator.goal.unification_index,
                       SubgoalUnificationIndex::Actor(2));
            assert_eq!(plan_iterator.goal.bound_pattern(&plan.parameters).unwrap(),
                       expected_goal_pattern,
                       "Planner goal.pattern is not as expected");

            // There should be no further plans possible
            assert_eq!(plan_iterator.construct_next_plan().is_some(),
                       false,
                       "Expected no further plan to be constructed");
        }

        #[test]
        fn test_plan_iterator() {
            let (rules, data, rule_ids, data_ids) = super::setup();
            let mut plan_iterator = Planner {
                actors: rules.iter().collect(),
                actor_ids: rule_ids.iter().collect(),
                config: PlannerConfig::new(),
                data_ids: data_ids.iter().collect(),
                data: data.iter().collect(),
                prefix: "test".to_string(),
                ops: 0,
                goal: Subgoal::new(Datum::from_sexp_str("(timeline-entry ((current-state 2) ((time ?_t2)) ()))").unwrap(),
                                   &Vec::new()),
                debug: false,
            };
            let expected_goal_1 = Datum::from_sexp_str("(timeline-entry ((current-state 2) ((time 2)) ()))").unwrap();
            let expected_goal_2 = Datum::from_sexp_str("(timeline-entry ((current-state 2) ((time 3)) ()))").unwrap();

            let plan_1_opt = plan_iterator.next();
            assert_eq!(plan_1_opt.is_some(), true);
            let plan_1 = plan_1_opt.expect("Expected first solution");

            assert_eq!(plan_1.root.pattern,
                       expected_goal_1,
                       "First plan is not as expected, {}",
                       plan_1.root);

            let plan_2_opt = plan_iterator.next();
            assert_eq!(plan_2_opt.is_some(), true);
            let plan_2 = plan_2_opt.expect("Expected second solution");

            assert_eq!(plan_2.root.pattern,
                       expected_goal_2,
                       "Second plan is not as expected, {}",
                       plan_2.root);
        }
    }
}
