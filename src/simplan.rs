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
                    if let Some(bindings) = data[idx].unify(&self.pattern, &self.current_plan_parameters.bindings) {
                        self.current_plan_parameters = PlanParameters {
                            bindings: bindings,
                            constraints: self.current_plan_parameters.constraints.clone(),
                            used_data: self.current_plan_parameters.used_data.clone(),
                        };
                        self.current_plan_parameters.used_data.insert(idx);
                        Some(self.current_plan_parameters.clone())
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            SubgoalUnificationIndex::Actor(idx) => {
                let rule = rules[idx].snowflake(self.construct_snowflake_prefix());
                rule.r_apply(&self.pattern, &self.current_plan_parameters.bindings).and_then(|(subgoal_patterns, bindings)| {
                    let mut updated_constraints = self.current_plan_parameters.constraints.clone();
                    updated_constraints.extend(rule.constraints().into_iter().cloned());

                    Constraint::solve_many(updated_constraints.iter().collect(), &bindings).ok().and_then(|bindings| {
                        let current_plan_parameters = PlanParameters {
                            bindings: bindings,
                            constraints: updated_constraints.clone(),
                            used_data: self.current_plan_parameters.used_data.clone(),
                        };
                        self.current_plan_parameters = current_plan_parameters.clone();
                        self.subgoals = subgoal_patterns.into_iter()
                            .map(|sg_p| Subgoal::new(sg_p, &self.current_plan_parameters, self.config.clone()))
                            .collect();
                        if self.subgoals.len() > 0 {
                            self.solve_subgoal(0, data, rules, current_plan_parameters.clone(), max_depth)
                        } else {
                            Some(self.current_plan_parameters.clone())
                        }
                    })
                })
            }
            SubgoalUnificationIndex::Exhausted => None,
            SubgoalUnificationIndex::Init => panic!("SHOULD NOT HAPPEN"),
        }
    }

    pub fn solve_subgoal(&mut self,
                         idx: usize,
                         data: &Vec<&U>,
                         rules: &Vec<&A>,
                         plan_parameters: PlanParameters<T>,
                         max_depth: usize)
                         -> Option<PlanParameters<T>> {
        let subgoal_count = self.subgoals.len();
        loop {
            let current_plan_parameters_opt = {
                let ref mut subgoal = self.subgoals[idx];
                subgoal.current_plan_parameters = plan_parameters.clone();
                subgoal.sub_plan(data, rules, max_depth - 1)
            };
            if let Some(current_plan_parameters) = current_plan_parameters_opt {
                if idx + 1 == subgoal_count {
                    return Some(current_plan_parameters);
                } else {
                    let result = self.solve_subgoal(idx + 1, data, rules, current_plan_parameters, max_depth);
                    if result.is_some() {
                        // Subsequent goals satisfied, return solution
                        return result;
                    }
                }
            } else {
                return None;
            }
        }
    }

    pub fn apply_bindings(&self, bindings: &Bindings<T>) -> Self {
        let mut root = self.clone();
        root.pattern = root.pattern.apply_bindings(bindings).unwrap();
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

    fn render_subtree(&self, parent: Option<String>) -> String {
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
        Uuid::new_v4().to_string()
    }
}

impl<T: BindingsValue, U: Unify<T>, A: Apply<T, U>> std::fmt::Display for Subgoal<T, U, A> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "Subgoal tree:\n{}", self.pprint(1))
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;
    use core::{Bindings, ToSexp};
    use datum::Datum;
    use infer::Rule;
    use super::{PlanningConfig, PlanParameters, Subgoal};

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
    fn test_plan_with_goal_constraint() {
        let rules = setup_rules();
        let constraints: Vec<super::Constraint<Datum>> = vec!["(constraint-set (?min_time 2))",
                                                              "(constraint-greater-than (?t2 ?min_time))"]
            .into_iter()
            .map(|s| super::Constraint::from_sexp_str(s).unwrap())
            .collect();
        let data = vec![Datum::from_sexp_str("(timeline-entry ((current-state 0) ((time 1)) ()))").unwrap()];
        let goal_pattern = Datum::from_sexp_str("(timeline-entry ((current-state 2) ((time ?t2)) ()))").unwrap();

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

        println!("\n");
        let plan_parameters = goal.plan(&data.iter().collect(), &rules.iter().collect());
        assert_eq!(plan_parameters.is_some(), true);
        println!("\nBindings: {}", plan_parameters.as_ref().unwrap().bindings);
        let plan_parameters = plan_parameters.unwrap();
        assert_eq!(plan_parameters.bindings.get_binding(&"?t2".to_string()).unwrap(),
                   Datum::from_float(3.0));
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
                                        reuse_data: true,
                                    });
        assert_eq!(goal.plan(&data.iter().collect(), &rules.iter().collect()).is_some(),
                   true);
    }
}
