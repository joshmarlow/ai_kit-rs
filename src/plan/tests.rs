use plan::*;

#[cfg(test)]
mod fplan_tests {
    use core::ToSexp;
    use datum::Datum;
    use infer::Rule;
    use super::*;

    #[cfg(test)]
    mod first_subgoal_to_step_tests {
        use super::*;

        #[test]
        fn test_first_subgoal_to_step_with_all_init() {
            let subgoals = vec![UnificationIndex::Init, UnificationIndex::Init];
            assert_eq!(first_subgoal_to_step(&subgoals), Some(0));
        }

        #[test]
        fn test_first_subgoal_to_step_with_partial_init() {
            let subgoals = vec![UnificationIndex::Datum(0), UnificationIndex::Init];
            assert_eq!(first_subgoal_to_step(&subgoals), Some(1));
        }

        #[test]
        fn test_first_subgoal_to_step_with_no_init() {
            let subgoals = vec![UnificationIndex::Datum(0), UnificationIndex::Datum(1)];
            assert_eq!(first_subgoal_to_step(&subgoals), Some(1));
        }

        #[test]
        fn test_first_subgoal_to_step_with_exhausted_subgoal() {
            let subgoals = vec![UnificationIndex::Datum(0), UnificationIndex::Exhausted];
            assert_eq!(first_subgoal_to_step(&subgoals), Some(0));
        }

        #[test]
        fn test_first_subgoal_to_step_with_all_exhausted() {
            let subgoals = vec![UnificationIndex::Exhausted, UnificationIndex::Exhausted];
            assert_eq!(first_subgoal_to_step(&subgoals), None);
        }
    }

    pub fn setup_rules() -> Vec<Rule<Datum, Datum>> {
        let physics_rule = Rule::from_sexp_str(r#"(defrule
            ((((current-state ?s1) ((time ?t1)))
              ((action 2) ((time ?t1))))
             ((current-state ?s2) ((time ?t2)))
             ((constraint-set (?diff1 1))
              (constraint-set (?diff2 2))
              (constraint-sum (?s1 ?diff2 ?s2))
              (constraint-sum (?t1 ?diff1 ?t2)))))"#)
            .expect("physics rule 1");
        let physics_rule_2 = Rule::from_sexp_str(r#"(defrule
         ((((current-state ?s1) ((time ?t1)))
            ((action 1) ((time ?t1))))
           ((current-state ?s2) ((time ?t2)))
           ((constraint-set (?diff 1))
            (constraint-sum (?t1 ?diff ?t2))
            (constraint-sum (?s1 ?diff ?s2)))))"#)
            .expect("physics rule 2");
        let interaction_model_add_2 = Rule::from_sexp_str(r#"(defrule
            (()
            ((action 2) ((time ?t)))
            ()))"#)
            .expect("interaction model 2");

        let interaction_model_add_1 = Rule::from_sexp_str(r#"(defrule
            (()
            ((action 1) ((time ?t)))
            ()))"#)
            .expect("interaction model 1");

        vec![physics_rule, interaction_model_add_2, physics_rule_2, interaction_model_add_1]
    }

    #[test]
    fn test_initial_step_of_root_goal() {
        let data = vec![Datum::from_sexp_str("((current-state 0) ((time 0)))").expect("Test datum")];
        let rules = setup_rules();
        let data_refs: Vec<&Datum> = data.iter().collect();
        let rule_refs: Vec<&Rule<Datum, Datum>> = rules.iter().collect();

        let goal = Goal::new(Datum::from_sexp_str("((current-state 4) ((time ?t)))").expect("Goal datum"));

        // Test step 1
        let expected_stepped_goal: Goal<Datum, Datum, Rule<Datum, Datum>> = Goal {
            pattern: goal.pattern.clone(),
            unification_index: UnificationIndex::Datum(0),
            subgoals: Vec::new(),
            _a_marker: PhantomData,
            _t_marker: PhantomData,
        };
        let stepped_goal = goal.step(&data_refs, &rule_refs, 0).expect("First plan");
        assert_eq!(stepped_goal, expected_stepped_goal, "First stepped goal");

        // Test step 2
        let expected_stepped_goal_2: Goal<Datum, Datum, Rule<Datum, Datum>> = Goal {
            pattern: goal.pattern.clone(),
            unification_index: UnificationIndex::Actor(0),
            subgoals: vec![Goal {
                               pattern: Datum::from_sexp_str("((current-state 2) ((time ?t1::1)))").expect("SubGoal 1 datum"),
                               unification_index: UnificationIndex::Init,
                               subgoals: Vec::new(),
                               _a_marker: PhantomData,
                               _t_marker: PhantomData,
                           },
                           Goal {
                               pattern: Datum::from_sexp_str("((action 2) ((time ?t1::1)))").expect("SubGoal 2 datum"),
                               unification_index: UnificationIndex::Init,
                               subgoals: Vec::new(),
                               _a_marker: PhantomData,
                               _t_marker: PhantomData,
                           }],
            _a_marker: PhantomData,
            _t_marker: PhantomData,
        };
        let stepped_goal_2 = stepped_goal.step(&data_refs, &rule_refs, 1).expect("Second plan");

        assert_eq!(stepped_goal_2,
                   expected_stepped_goal_2,
                   "Second stepped goal");

        // Test step 3
        let expected_stepped_goal_3: Goal<Datum, Datum, Rule<Datum, Datum>> = Goal {
            pattern: goal.pattern.clone(),
            unification_index: UnificationIndex::Actor(0),
            subgoals: vec![Goal {
                               pattern: Datum::from_sexp_str("((current-state 2) ((time ?t1::1)))").expect("SubGoal 1 datum"),
                               unification_index: UnificationIndex::Datum(0),
                               subgoals: vec![],
                               _a_marker: PhantomData,
                               _t_marker: PhantomData,
                           },
                           Goal {
                               pattern: Datum::from_sexp_str("((action 2) ((time ?t1::1)))").expect("SubGoal 2 datum"),
                               unification_index: UnificationIndex::Init,
                               subgoals: Vec::new(),
                               _a_marker: PhantomData,
                               _t_marker: PhantomData,
                           }],
            _a_marker: PhantomData,
            _t_marker: PhantomData,
        };
        let stepped_goal_3 = stepped_goal_2.step(&data_refs, &rule_refs, 2).expect("Third plan");
        assert_eq!(stepped_goal_3,
                   expected_stepped_goal_3,
                   "Second stepped goal");

        // Test step 4
        let expected_stepped_goal_4: Goal<Datum, Datum, Rule<Datum, Datum>> = Goal {
            pattern: goal.pattern.clone(),
            unification_index: UnificationIndex::Actor(0),
            subgoals: vec![Goal {
                               pattern: Datum::from_sexp_str("((current-state 2) ((time ?t1::1)))").expect("SubGoal 1 datum"),
                               unification_index: UnificationIndex::Datum(0),
                               subgoals: vec![],
                               _a_marker: PhantomData,
                               _t_marker: PhantomData,
                           },
                           Goal {
                               pattern: Datum::from_sexp_str("((action 2) ((time ?t1::1)))").expect("SubGoal 2 datum"),
                               unification_index: UnificationIndex::Datum(0),
                               subgoals: Vec::new(),
                               _a_marker: PhantomData,
                               _t_marker: PhantomData,
                           }],
            _a_marker: PhantomData,
            _t_marker: PhantomData,
        };
        let stepped_goal_4 = stepped_goal_3.step(&data_refs, &rule_refs, 2).expect("Third plan");
        assert_eq!(stepped_goal_4,
                   expected_stepped_goal_4,
                   "Second stepped goal");

        // Test step 5
        let expected_stepped_goal_5: Goal<Datum, Datum, Rule<Datum, Datum>> = Goal {
            pattern: goal.pattern.clone(),
            unification_index: UnificationIndex::Actor(0),
            subgoals: vec![Goal {
                               pattern: Datum::from_sexp_str("((current-state 2) ((time ?t1::1)))").expect("SubGoal 1 datum"),
                               unification_index: UnificationIndex::Datum(0),
                               subgoals: vec![],
                               _a_marker: PhantomData,
                               _t_marker: PhantomData,
                           },
                           Goal {
                               pattern: Datum::from_sexp_str("((action 2) ((time ?t1::1)))").expect("SubGoal 2 datum"),
                               unification_index: UnificationIndex::Actor(1),
                               subgoals: Vec::new(),
                               _a_marker: PhantomData,
                               _t_marker: PhantomData,
                           }],
            _a_marker: PhantomData,
            _t_marker: PhantomData,
        };
        let stepped_goal_5 = stepped_goal_4.step(&data_refs, &rule_refs, 2).expect("Third plan");
        print!("\nPrevious Goal tree:\n{}\n", stepped_goal_4);
        print!("\nExpected Goal tree:\n{}\n", expected_stepped_goal_5);
        print!("\nStepped Goal tree:\n{}\n", stepped_goal_5);
        assert_eq!(stepped_goal_5,
                   expected_stepped_goal_5,
                   "Fifth stepped goal");

        // Test step 6
        let expected_stepped_goal_6: Goal<Datum, Datum, Rule<Datum, Datum>> = Goal {
            pattern: goal.pattern.clone(),
            unification_index: UnificationIndex::Actor(0),
            subgoals: vec![Goal {
                               pattern: Datum::from_sexp_str("((current-state 2) ((time ?t1::2)))").expect("SubGoal 1 datum"),
                               unification_index: UnificationIndex::Actor(0),
                               subgoals: vec![Goal {
                                                  pattern: Datum::from_sexp_str("((current-state 0) ((time 0)))")
                                                      .expect("SubGoal 1.1 datum"),
                                                  unification_index: UnificationIndex::Datum(0),
                                                  subgoals: Vec::new(),
                                                  _a_marker: PhantomData,
                                                  _t_marker: PhantomData,
                                              },
                                              Goal {
                                                  pattern: Datum::from_sexp_str("((action 2) ((time ?t0::2)))")
                                                      .expect("SubGoal 1.2 datum"),
                                                  unification_index: UnificationIndex::Actor(1),
                                                  subgoals: Vec::new(),
                                                  _a_marker: PhantomData,
                                                  _t_marker: PhantomData,
                                              }],
                               _a_marker: PhantomData,
                               _t_marker: PhantomData,
                           },
                           Goal {
                               pattern: Datum::from_sexp_str("((action 2) ((time ?t1::2)))").expect("SubGoal 2 datum"),
                               unification_index: UnificationIndex::Actor(1),
                               subgoals: Vec::new(),
                               _a_marker: PhantomData,
                               _t_marker: PhantomData,
                           }],
            _a_marker: PhantomData,
            _t_marker: PhantomData,
        };
        println!("STEPPING AGAIN\n\n");
        let stepped_goal_6 = stepped_goal_5.step(&data_refs, &rule_refs, 2).expect("Third plan");

        print!("\nPrevious Goal tree:\n{}\n", stepped_goal_5);
        print!("\nExpected Goal tree:\n{}\n", expected_stepped_goal_6);
        print!("\nStepped Goal tree:\n{}\n", stepped_goal_6);
        assert_eq!(stepped_goal_6,
                   expected_stepped_goal_6,
                   "Second stepped goal");

        assert_eq!(true, false);
    }
}
