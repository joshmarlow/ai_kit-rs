use std;
use std::collections::HashSet;
use std::marker::PhantomData;
use uuid::Uuid;

use constraints::Constraint;
use core::{Apply, Bindings, BindingsValue, ToSexp, Unify};

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum SubgoalUnificationIndex {
    Init,
    Actor(usize),
    Datum(usize),
    Exhausted,
}

impl std::fmt::Display for SubgoalUnificationIndex {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match *self {
            SubgoalUnificationIndex::Init => write!(f, "Init"),
            SubgoalUnificationIndex::Actor(idx) => write!(f, "Actor({})", idx),
            SubgoalUnificationIndex::Datum(idx) => write!(f, "Datum({})", idx),
            SubgoalUnificationIndex::Exhausted => write!(f, "Exhausted"),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct PlanParameters<T: BindingsValue> {
    pub bindings: Bindings<T>,
    pub constraints: Vec<Constraint<T>>,
    pub used_data: HashSet<usize>,
}

impl<T: BindingsValue> PlanParameters<T> {
    pub fn new(bindings: Bindings<T>, constraints: Vec<Constraint<T>>) -> Self {
        PlanParameters {
            bindings: bindings,
            constraints: constraints,
            used_data: HashSet::new(),
        }
    }

    pub fn empty() -> Self {
        PlanParameters {
            bindings: Bindings::new(),
            constraints: Vec::new(),
            used_data: HashSet::new(),
        }
    }

    pub fn solve_constraints(&self) -> Option<Bindings<T>> {
        Constraint::solve_many(self.constraints.iter().collect(), &self.bindings).ok()
    }
}

impl<T: BindingsValue> std::fmt::Display for PlanParameters<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let constraints_vec: Vec<String> = self.constraints.iter().map(|constraint| constraint.to_sexp_string()).collect();
        write!(f,
               "(plan-parameters bindings: {}, constraints: {})",
               self.bindings,
               constraints_vec.join(","),
            )
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct PlanningConfig {
    pub debug: bool,
    pub max_depth: usize,
    pub reuse_data: bool,
}

pub fn solve_subgoals<T: BindingsValue, U: Unify<T>, A: Apply<T, U>>(subgoals: Vec<&mut Subgoal<T, U, A>>,
                                                                     idx: usize,
                                                                     data: &Vec<&U>,
                                                                     rules: &Vec<&A>,
                                                                     plan_parameters: &PlanParameters<T>,
                                                                     max_depth: usize)
                                                                     -> Option<PlanParameters<T>> {
    let mut subgoals = subgoals;
    let subgoal_count = subgoals.len();
    let is_last_goal = |idx| idx == subgoal_count;
    let is_first_goal = |idx| idx == 0;

    let mut idx = idx;
    let mut param_stack: Vec<PlanParameters<T>> = vec![plan_parameters.clone()]
        .into_iter()
        .chain(subgoals.iter()
            .map(|sg| sg.current_plan_parameters.clone()))
        .collect();

    if subgoal_count == 0 {
        Some(plan_parameters.clone())
    } else {
        loop {
            let current_plan_parameters_opt = {
                let ref mut subgoal = subgoals[idx];
                subgoal.current_plan_parameters = param_stack[idx].clone();
                subgoal.sub_plan(data, rules, max_depth - 1)
            };
            if let Some(current_plan_parameters) = current_plan_parameters_opt {
                param_stack[idx + 1] = current_plan_parameters.clone();
                idx += 1;
                if is_last_goal(idx) {
                    return Some(current_plan_parameters);
                }
            } else {
                if is_first_goal(idx) {
                    return None;
                } else {
                    idx -= 1;
                }
            }
        }
    }
}


#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Subgoal<T: BindingsValue, U: Unify<T>, A: Apply<T, U>> {
    pub config: PlanningConfig,
    pub current_plan_parameters: PlanParameters<T>,
    pub pattern: U,
    pub subgoals: Vec<Subgoal<T, U, A>>,
    pub unification_index: SubgoalUnificationIndex,
    _marker: PhantomData<A>,
}

impl<T: BindingsValue, U: Unify<T>, A: Apply<T, U>> Subgoal<T, U, A> {
    pub fn new(pattern: U, initial_parameters: &PlanParameters<T>, config: PlanningConfig) -> Self {
        Subgoal {
            config: config,
            current_plan_parameters: initial_parameters.clone(),
            pattern: pattern,
            subgoals: Vec::new(),
            unification_index: SubgoalUnificationIndex::Init,
            _marker: PhantomData,
        }
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

    pub fn plan(&mut self, data: &Vec<&U>, rules: &Vec<&A>) -> Option<PlanParameters<T>> {
        let max_depth = self.config.max_depth.clone();
        self.sub_plan(data, rules, max_depth)
    }

    pub fn sub_plan(&mut self, data: &Vec<&U>, rules: &Vec<&A>, max_depth: usize) -> Option<PlanParameters<T>> {
        // Try to have subgoals replan before changing subgoals
        loop {
            let initial_plan_parameters = self.current_plan_parameters.clone();
            let subgoal_count = self.subgoals.len();
            if subgoal_count > 0 {
                let plan_parameters = if subgoal_count > 2 {
                    self.subgoals[subgoal_count - 2].current_plan_parameters.clone()
                } else {
                    initial_plan_parameters.clone()
                };
                if let Some(parameters) = solve_subgoals(self.subgoals.iter_mut().collect(),
                                                         subgoal_count - 1,
                                                         data,
                                                         rules,
                                                         &plan_parameters,
                                                         max_depth) {
                    return Some(parameters);
                }
            }
            // Subgoals can't find a solution, so replan subgoals
            if self.increment_unification_index(data.len(), rules.len()) == SubgoalUnificationIndex::Exhausted {
                return None;
            }
            if let Some(plan_parameters) = self.satisfied(data, rules, max_depth) {
                if let Some(bindings) = plan_parameters.solve_constraints() {
                    return Some(PlanParameters {
                        bindings: bindings,
                        constraints: plan_parameters.constraints.clone(),
                        used_data: plan_parameters.used_data.clone(),
                    });
                }
            }
            // Reset parameters and try again
            self.current_plan_parameters = initial_plan_parameters.clone();
        }
    }

    pub fn sub_plan_legacy(&mut self, data: &Vec<&U>, rules: &Vec<&A>, max_depth: usize) -> Option<PlanParameters<T>> {
        let initial_plan_parameters = self.current_plan_parameters.clone();
        loop {
            if self.increment_unification_index(data.len(), rules.len()) == SubgoalUnificationIndex::Exhausted {
                return None;
            }
            if let Some(plan_parameters) = self.satisfied(data, rules, max_depth) {
                if let Some(bindings) = plan_parameters.solve_constraints() {
                    return Some(PlanParameters {
                        bindings: bindings,
                        constraints: plan_parameters.constraints.clone(),
                        used_data: plan_parameters.used_data.clone(),
                    });
                }
            }
            // Reset parameters and try again
            self.current_plan_parameters = initial_plan_parameters.clone();
        }
    }

    pub fn can_use_datum(&self, idx: usize) -> bool {
        self.config.reuse_data || !self.current_plan_parameters.used_data.contains(&idx)
    }

    pub fn satisfied(&mut self, data: &Vec<&U>, rules: &Vec<&A>, max_depth: usize) -> Option<PlanParameters<T>> {
        if max_depth == 0 {
            return None;
        }

        match self.unification_index {
            SubgoalUnificationIndex::Datum(idx) => {
                if self.can_use_datum(idx) {
                    data[idx].unify(&self.pattern, &self.current_plan_parameters.bindings).and_then(|bindings| {
                        self.log(max_depth,
                                 format!("Unified: {} with {}", self.pattern, data[idx]));
                        self.current_plan_parameters.bindings = bindings;
                        self.current_plan_parameters.used_data.insert(idx);
                        Some(self.current_plan_parameters.clone())
                    })
                } else {
                    None
                }
            }
            SubgoalUnificationIndex::Actor(idx) => {
                let rule = rules[idx].snowflake(self.construct_snowflake_prefix());
                rule.r_apply(&self.pattern, &self.current_plan_parameters.bindings).and_then(|(subgoal_patterns, bindings)| {
                    self.log(max_depth, format!("Applied: {}", self.pattern));
                    for sgp in subgoal_patterns.iter() {
                        self.log(max_depth, format!("\tSubgoal: {}", sgp));
                    }
                    let mut updated_constraints = self.current_plan_parameters.constraints.clone();
                    updated_constraints.extend(rule.constraints().into_iter().cloned());

                    Constraint::solve_many(updated_constraints.iter().collect(), &bindings).ok().and_then(|bindings| {
                        self.current_plan_parameters.bindings = bindings;
                        self.current_plan_parameters.constraints = updated_constraints;
                        self.subgoals = subgoal_patterns.into_iter()
                            .map(|sg_p| Subgoal::new(sg_p, &self.current_plan_parameters, self.config.clone()))
                            .collect();
                        solve_subgoals(self.subgoals.iter_mut().collect(),
                                       0,
                                       data,
                                       rules,
                                       &self.current_plan_parameters,
                                       max_depth)
                    })
                })
            }
            SubgoalUnificationIndex::Exhausted => None,
            SubgoalUnificationIndex::Init => panic!("SHOULD NOT HAPPEN"),
        }
    }

    pub fn apply_bindings(&self, bindings: &Bindings<T>) -> Self {
        let mut root = self.clone();
        root.pattern = root.pattern.apply_bindings(bindings).expect("Bindings should be applicable");
        root.subgoals = root.subgoals.iter().map(|sg| sg.apply_bindings(&bindings)).collect();
        root
    }

    fn log(&self, depth: usize, msg: String) {
        if self.config.debug {
            println!("[{:?}] {}", self.config.max_depth - depth, msg);
        }
    }

    pub fn render(&self) -> String {
        let subtree_string = self.render_subtree(None);
        format!("graph \"goal tree {}\" {{\n{}\n}}",
                self.pattern,
                subtree_string)
    }

    pub fn render_subtree(&self, parent: Option<String>) -> String {
        let goal_rendering = format!("{} [{}]", self.pattern, self.unification_index);
        let subtree_string_vec: Vec<String> = self.subgoals
            .iter()
            .map(|subgoal| subgoal.render_subtree(Some(goal_rendering.clone())))
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

    pub fn construct_snowflake_prefix(&self) -> String {
        let mut s = Uuid::new_v4().to_string();
        s.truncate(5);
        s
    }

    /// Traverse the subgoal tree using a depth-first search and gather the leaves of the plan
    pub fn gather_leaves(&self, plan_parameters: &PlanParameters<T>) -> Vec<U> {
        let mut leaves = Vec::new();

        if self.subgoals.is_empty() {
            leaves.push(self.pattern.apply_bindings(&plan_parameters.bindings).expect("Bindings should be applicable"));
        } else {
            for sg in self.subgoals.iter() {
                leaves.extend(sg.gather_leaves(plan_parameters).into_iter());
            }
        }

        leaves
    }
}

impl<T: BindingsValue, U: Unify<T>, A: Apply<T, U>> std::fmt::Display for Subgoal<T, U, A> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "Subgoal tree:\n{}", self.pprint(1))
    }
}

#[cfg(test)]
mod simplan_tests {
    use std::collections::HashSet;
    use core::{Bindings, ToSexp};
    use datum::Datum;
    use infer::Rule;
    use super::*;

    pub fn setup_rules() -> Vec<Rule<Datum, Datum>> {
        let physics_rule = Rule::from_sexp_str(r#"(defrule (((timeline-entry ((current-state ?s1) ((time ?t1)) ()))
                          (timeline-entry ((action 2) ((time ?t1)) ())))
                         (timeline-entry ((current-state ?s2) ((time ?t2)) ()))
                      ((constraint-set (?diff1 1))
                       (constraint-set (?diff2 2))
                       (constraint-sum (?s1 ?diff2 ?s2))
                       (constraint-sum (?t1 ?diff1 ?t2)))))"#)
            .unwrap();
        let physics_rule_2 = Rule::from_sexp_str(r#"(defrule (((timeline-entry ((current-state ?s0) ((time ?t0)) ()))
                          (timeline-entry ((action 1) ((time ?t0)) ())))
                         (timeline-entry ((current-state ?s1) ((time ?t1)) ()))
                       ((constraint-set (?diff1 1))
                        (constraint-sum (?s0 ?diff1 ?s1))
                        (constraint-sum (?t0 ?diff1 ?t1)))))"#)
            .unwrap();
        let interaction_model_add_2 = Rule::from_sexp_str("(defrule (() (timeline-entry ((action 2) ((time ?t)) ())) ()))")
            .unwrap();

        let interaction_model_add_1 = Rule::from_sexp_str("(defrule (() (timeline-entry ((action 1) ((time ?t)) ())) ()))")
            .unwrap();

        vec![physics_rule, interaction_model_add_2, physics_rule_2, interaction_model_add_1]
    }

    #[test]
    fn test_solve_subgoals_when_all_init() {
        let rules = setup_rules();
        let data = vec![Datum::from_sexp_str("(timeline-entry ((current-state 0) ((time 0)) ()))").unwrap()];

        let config = PlanningConfig {
            debug: true,
            max_depth: 5,
            reuse_data: true,
        };
        let mut initial_plan_parameters = PlanParameters::empty();
        initial_plan_parameters.bindings.set_binding_mut(&"?s0".to_string(), Datum::Int(2));

        let mut subgoals: Vec<Subgoal<Datum, Datum, Rule<Datum, Datum>>> =
            vec![Subgoal::new(Datum::from_sexp_str(&"(timeline-entry ((current-state 2) ((time ?t)) ()))".to_string())
                                  .unwrap(),
                              &PlanParameters::empty(),
                              config.clone()),
                 Subgoal::new(Datum::from_sexp_str(&"(timeline-entry ((action 2) ((time ?t)) ()))".to_string()).unwrap(),
                              &PlanParameters::empty(),
                              config.clone())];

        println!("\n");

        // Construct initial plan and verify it is as expected
        let plan_parameters = solve_subgoals(subgoals.iter_mut().collect(),
                                             0,
                                             &data.iter().collect(),
                                             &rules.iter().collect(),
                                             &initial_plan_parameters,
                                             config.max_depth);
        assert_eq!(plan_parameters.is_some(), true);
        assert_eq!(subgoals[0].unification_index,
                   SubgoalUnificationIndex::Actor(0));
        assert_eq!(subgoals[1].unification_index,
                   SubgoalUnificationIndex::Actor(1));

        println!("\n\n******** REPLANNING ********\n\n");
        // Replan and verify expectations
        let plan_parameters = solve_subgoals(subgoals.iter_mut().collect(),
                                             0,
                                             &data.iter().collect(),
                                             &rules.iter().collect(),
                                             &initial_plan_parameters,
                                             config.max_depth);
        assert_eq!(plan_parameters.is_some(), true);
        assert_eq!(subgoals[0].unification_index,
                   SubgoalUnificationIndex::Actor(0));
        assert_eq!(subgoals[1].unification_index,
                   SubgoalUnificationIndex::Actor(1));
    }

    #[test]
    fn test_plan_with_goal_constraint() {
        let rules = setup_rules();
        let constraints: Vec<super::Constraint<Datum>> = vec!["(constraint-set (?min_time 2))",
                                                              "(constraint-greater-than (?t2 ?min_time))"]
            .into_iter()
            .map(|s| super::Constraint::from_sexp_str(s).expect("Expected constraint to be parsed"))
            .collect();
        let data = vec![Datum::from_sexp_str("(timeline-entry ((current-state 0) ((time 1)) ()))").expect("d1")];
        let goal_pattern = Datum::from_sexp_str("(timeline-entry ((current-state 2) ((time ?t2)) ()))").expect("d2");

        let mut goal = Subgoal::new(goal_pattern,
                                    &PlanParameters {
                                        bindings: Bindings::new(),
                                        constraints: constraints,
                                        used_data: HashSet::new(),
                                    },
                                    PlanningConfig {
                                        debug: true,
                                        max_depth: 3,
                                        reuse_data: true,
                                    });

        let plan_parameters = goal.plan(&data.iter().collect(), &rules.iter().collect());
        assert_eq!(plan_parameters.is_some(), true);
        let plan_parameters = plan_parameters.unwrap();
        println!("{}", plan_parameters);
        assert_eq!(plan_parameters.bindings.get_binding(&"?t2".to_string()).expect("Expected valuel"),
                   Datum::from_float(3.0));
    }

    #[test]
    fn test_plan_with_backtracking_simple() {
        let rules = setup_rules();
        let data = vec![Datum::from_sexp_str("(timeline-entry ((current-state 0) ((time 1)) ()))").unwrap()];
        let goal_pattern = Datum::from_sexp_str("(timeline-entry ((current-state 2) ((time ?t2)) ()))").unwrap();

        let mut goal = Subgoal::new(goal_pattern,
                                    &PlanParameters {
                                        bindings: Bindings::new(),
                                        constraints: Vec::new(),
                                        used_data: HashSet::new(),
                                    },
                                    PlanningConfig {
                                        debug: true,
                                        max_depth: 3,
                                        reuse_data: true,
                                    });

        let plan_parameters = goal.plan(&data.iter().collect(), &rules.iter().collect());
        assert_eq!(plan_parameters.is_some(), true);

        let expected_leaves: Vec<Datum> = vec![Datum::from_sexp_str(&"(timeline-entry ((current-state 0) ((time 1)) ()))")
                                                   .unwrap(),
                                               Datum::from_sexp_str(&"(timeline-entry ((action 2) ((time 1)) ()))").unwrap()];
        let actual_leaves = goal.gather_leaves(&plan_parameters.unwrap());
        assert_eq!(actual_leaves, expected_leaves);

        // And backtrack!
        let plan_parameters = goal.plan(&data.iter().collect(), &rules.iter().collect());
        assert_eq!(plan_parameters.is_some(), true);

        let expected_leaves: Vec<Datum> = vec![Datum::from_sexp_str(&"(timeline-entry ((current-state 0) ((time 1)) ()))")
                                                   .unwrap(),
                                               Datum::from_sexp_str(&"(timeline-entry ((action 1) ((time 1)) ()))").unwrap(),
                                               Datum::from_sexp_str(&"(timeline-entry ((action 1) ((time 2)) ()))").unwrap()];
        let actual_leaves = goal.gather_leaves(&plan_parameters.unwrap());
        println!("\n");
        print!("{}", goal);
        println!("Actual\n");
        for l in actual_leaves.iter() {
            println!("\t{}", l);
        }
        println!("Expected\n");
        for l in expected_leaves.iter() {
            println!("\t{}", l);
        }
        assert_eq!(actual_leaves, expected_leaves);
    }

    #[test]
    fn test_plan_with_backtracking_complex() {
        let rules = setup_rules();
        let data = vec![Datum::from_sexp_str("(timeline-entry ((current-state 0) ((time 1)) ()))").unwrap()];
        let goal_pattern = Datum::from_sexp_str("(timeline-entry ((current-state 4) ((time ?t2)) ()))").unwrap();

        let mut goal = Subgoal::new(goal_pattern,
                                    &PlanParameters {
                                        bindings: Bindings::new(),
                                        constraints: Vec::new(),
                                        used_data: HashSet::new(),
                                    },
                                    PlanningConfig {
                                        debug: false,
                                        max_depth: 3,
                                        reuse_data: true,
                                    });

        let plan_parameters = goal.plan(&data.iter().collect(), &rules.iter().collect());
        assert_eq!(plan_parameters.is_some(), true);
        println!("\n{}", goal);

        let expected_leaves: Vec<Datum> = vec![Datum::from_sexp_str(&"(timeline-entry ((current-state 0) ((time 1)) ()))")
                                                   .unwrap(),
                                               Datum::from_sexp_str(&"(timeline-entry ((action 2) ((time 1)) ()))").unwrap(),
                                               Datum::from_sexp_str(&"(timeline-entry ((action 2) ((time 2)) ()))").unwrap()];
        let actual_leaves = goal.gather_leaves(&plan_parameters.unwrap());
        println!("\n");
        println!("Actual\n");
        for l in actual_leaves.iter() {
            println!("\t{}", l);
        }
        println!("Expected\n");
        for l in expected_leaves.iter() {
            println!("\t{}", l);
        }
        assert_eq!(actual_leaves, expected_leaves);

        println!("\n\n*********************\n\n");

        // And backtrack!
        let plan_parameters = goal.plan(&data.iter().collect(), &rules.iter().collect());
        assert_eq!(plan_parameters.is_some(), true);

        let expected_leaves: Vec<Datum> = vec![Datum::from_sexp_str(&"(timeline-entry ((current-state 0) ((time 1)) ()))")
                                                   .unwrap(),
                                               Datum::from_sexp_str(&"(timeline-entry ((action 2) ((time 1)) ()))").unwrap(),
                                               Datum::from_sexp_str(&"(timeline-entry ((action 1) ((time 2)) ()))").unwrap(),
                                               Datum::from_sexp_str(&"(timeline-entry ((action 1) ((time 3)) ()))").unwrap()];
        let actual_leaves = goal.gather_leaves(&plan_parameters.unwrap());
        println!("\n");
        println!("Actual\n");
        for l in actual_leaves.iter() {
            println!("\t{}", l);
        }
        println!("Expected\n");
        for l in expected_leaves.iter() {
            println!("\t{}", l);
        }
        assert_eq!(actual_leaves, expected_leaves);
    }

    #[test]
    fn test_plan_for_nlp() {
        // the dog chased a cat

        // (det a)
        // (det the)
        // (verb chased)
        // (noun dog)
        // (noun cat)

        // (np det noun)
        // (vp verb np)

        // (sen np vp)
        let rules: Vec<Rule<Datum, Datum>> = vec![Rule::from_sexp_str("(defrule ((a) (det) ()))").unwrap(),
                                                  Rule::from_sexp_str("(defrule ((the) (det) ()))").unwrap(),
                                                  Rule::from_sexp_str("(defrule ((chased) (verb) ()))").unwrap(),
                                                  Rule::from_sexp_str("(defrule ((dog) (noun) ()))").unwrap(),
                                                  Rule::from_sexp_str("(defrule ((cat) (noun) ()))").unwrap(),
                                                  Rule::from_sexp_str("(defrule (((det) (noun)) (np) ()))").unwrap(),
                                                  Rule::from_sexp_str("(defrule (((verb) (np)) (vp) ()))").unwrap(),
                                                  Rule::from_sexp_str("(defrule (((np) (vp)) (sen) ()))").unwrap()];

        let data: Vec<Datum> = vec![Datum::from_sexp_str("a").unwrap(),
                                    Datum::from_sexp_str("the").unwrap(),
                                    Datum::from_sexp_str("dog").unwrap(),
                                    Datum::from_sexp_str("cat").unwrap(),
                                    Datum::from_sexp_str("chased").unwrap()];

        let mut goal = Subgoal::new(Datum::from_sexp_str("(sen)").unwrap(),
                                    &PlanParameters::new(Bindings::new(), Vec::new()),
                                    PlanningConfig {
                                        debug: false,
                                        max_depth: 5,
                                        reuse_data: false,
                                    });
        let plan_parameters = goal.plan(&data.iter().collect(), &rules.iter().collect());
        assert_eq!(plan_parameters.is_some(), true);
        let expected_leaves: Vec<Datum> =
            vec!["a".to_string(), "dog".to_string(), "chased".to_string(), "the".to_string(), "cat".to_string()]
                .into_iter()
                .map(|s| Datum::String(s))
                .collect();
        assert_eq!(goal.gather_leaves(&plan_parameters.unwrap()),
                   expected_leaves);
    }
}
