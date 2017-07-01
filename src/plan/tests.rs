use plan::*;

#[cfg(test)]
mod fplan_tests {
    use core::ToSexp;
    use datum::Datum;
    use infer::Rule;
    use super::*;

    /// Recursively compares the 'spines' of two goal trees
    /// where the 'spine' is the goal pattern and the unification indexes, as well as the number of subgoals
    fn assert_goal_spines_match(actual_goal: &Goal<Datum, Datum, Rule<Datum, Datum>>, expected_goal: &Goal<Datum, Datum, Rule<Datum, Datum>>) {
        if actual_goal.pattern != expected_goal.pattern {
            panic!("Patterns don't match - expected:\n\t{}\nbut found:\n\t{}\n",
                   actual_goal.pattern,
                   expected_goal.pattern);
        }
        if actual_goal.unification_index != expected_goal.unification_index {
            panic!("Unification indexes don't match - expected:\n\t{}\nbut found:\n\t{}\n",
                   actual_goal.unification_index,
                   expected_goal.unification_index);
        }
        assert_eq!(actual_goal.subgoals.len(),
                   expected_goal.subgoals.len(),
                   "Differing numbers of subgoals");

        for (expected_subgoal, actual_subgoal) in expected_goal.subgoals.iter().zip(actual_goal.subgoals.iter()) {
            assert_goal_spines_match(actual_subgoal, expected_subgoal)
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

    #[cfg(test)]
    mod create_subgoals_tests {
        use super::*;

        #[test]
        fn test_create_subgoals() {
            let rules = setup_rules();
            let rule_refs: Vec<&Rule<Datum, Datum>> = rules.iter().collect();

            let goal_pattern = Datum::from_sexp_str("((current-state 2) ((time ?t)))").expect("Goal pattern datum");
            let expected_bindings: Bindings<Datum> = vec![("?s2".to_string(), Datum::Float(2.0)),
                                                          ("?diff1".to_string(), Datum::Float(1.0)),
                                                          ("?diff2".to_string(), Datum::Float(2.0)),
                                                          ("?s1".to_string(), Datum::Float(0.0))]
                .into_iter()
                .collect();
            let expected_bindings = expected_bindings.set_binding(&"?t".to_string(), Datum::Variable("?t2".to_string()));

            let expected_subgoals = vec![Goal::new(Datum::from_sexp_str("((current-state 0) ((time ?t1)))").expect("SubGoal 1 datum"),
                                                   Vec::new(),
                                                   vec![Constraint::from_sexp_str("(constraint-set (?diff1 1))").expect("Constraint"),
                                                        Constraint::from_sexp_str("(constraint-set (?diff2 2))").expect("Constraint"),
                                                        Constraint::from_sexp_str(r#"(constraint-sum (?s1 ?diff2 ?s2))"#).expect("Constraint"),
                                                        Constraint::from_sexp_str(r#"(constraint-sum (?t1 ?diff1 ?t2))"#).expect("Constraint")],
                                                   expected_bindings.clone(),
                                                   UnificationIndex::Init,
                                                   Vec::new()),
                                         Goal::new(Datum::from_sexp_str("((action 2) ((time ?t1)))").expect("SubGoal 1 datum"),
                                                   Vec::new(),
                                                   vec![Constraint::from_sexp_str("(constraint-set (?diff1 1))").expect("Constraint"),
                                                        Constraint::from_sexp_str("(constraint-set (?diff2 2))").expect("Constraint"),
                                                        Constraint::from_sexp_str(r#"(constraint-sum (?s1 ?diff2 ?s2))"#).expect("Constraint"),
                                                        Constraint::from_sexp_str(r#"(constraint-sum (?t1 ?diff1 ?t2))"#).expect("Constraint")],
                                                   expected_bindings,
                                                   UnificationIndex::Init,
                                                   Vec::new())];
            let actual_subgoals = Goal::create_subgoals(&goal_pattern, rule_refs[0], &Vec::new());
            assert_eq!(actual_subgoals.as_ref().unwrap()[0],
                       expected_subgoals[0],
                       "First subgoal not as expected");
            assert_eq!(actual_subgoals.as_ref().unwrap()[1],
                       expected_subgoals[1],
                       "Second subgoal not as expected");
        }
    }

    #[cfg(test)]
    mod first_subgoal_to_increment_tests {
        use super::*;

        #[test]
        fn test_first_subgoal_to_increment_with_all_init() {
            let subgoals = vec![UnificationIndex::Init, UnificationIndex::Init];
            assert_eq!(first_subgoal_to_increment(&subgoals), Some(0));
        }

        #[test]
        fn test_first_subgoal_to_increment_with_partial_init() {
            let subgoals = vec![UnificationIndex::Datum(0), UnificationIndex::Init];
            assert_eq!(first_subgoal_to_increment(&subgoals), Some(1));
        }

        #[test]
        fn test_first_subgoal_to_increment_with_no_init() {
            let subgoals = vec![UnificationIndex::Datum(0), UnificationIndex::Datum(1)];
            assert_eq!(first_subgoal_to_increment(&subgoals), Some(1));
        }

        #[test]
        fn test_first_subgoal_to_increment_with_no_exhausted_subgoals() {
            let subgoals = vec![UnificationIndex::Datum(0), UnificationIndex::Actor(0)];
            assert_eq!(first_subgoal_to_increment(&subgoals), Some(1));
        }

        #[test]
        fn test_first_subgoal_to_increment_with_exhausted_subgoal() {
            let subgoals = vec![UnificationIndex::Datum(0), UnificationIndex::Exhausted];
            assert_eq!(first_subgoal_to_increment(&subgoals), Some(0));
        }

        #[test]
        fn test_first_subgoal_to_increment_with_all_exhausted() {
            let subgoals = vec![UnificationIndex::Exhausted, UnificationIndex::Exhausted];
            assert_eq!(first_subgoal_to_increment(&subgoals), None);
        }
    }

    #[cfg(test)]
    mod increment_subgoals_tests {
        use super::*;

        #[test]
        fn test_increment_subgoals_with_backtracking() {
            let data = vec![Datum::from_sexp_str("((current-state 0) ((time 0)))").expect("Test datum")];
            let rules = setup_rules();
            let data_refs: Vec<&Datum> = data.iter().collect();
            let rule_refs: Vec<&Rule<Datum, Datum>> = rules.iter().collect();

            let goal_pattern = Datum::from_sexp_str("((current-state 4) ((time ?t)))").expect("Goal datum");
            let initial_goal: Goal<Datum, Datum, Rule<Datum, Datum>> =
                Goal::new(goal_pattern.clone(),
                          Vec::new(),
                          Vec::new(),
                          Bindings::new(),
                          UnificationIndex::Actor(0),
                          vec![Goal::new(Datum::from_sexp_str("((current-state 2) ((time ?t1::0)))").expect("SubGoal 1 datum"),
                                         Vec::new(),
                                         Vec::new(),
                                         Bindings::new(),
                                         UnificationIndex::Datum(0),
                                         vec![]),
                               Goal::new(Datum::from_sexp_str("((action 2) ((time ?t1::0)))").expect("SubGoal 2 datum"),
                                         Vec::new(),
                                         Vec::new(),
                                         Bindings::new(),
                                         UnificationIndex::Actor(1),
                                         Vec::new())]);
            let expected_incremented_subgoals: Vec<Goal<Datum, Datum, Rule<Datum, Datum>>> =
                vec![Goal::new(Datum::from_sexp_str("((current-state 2) ((time ?t1::0)))").expect("SubGoal 1 datum"),
                               Vec::new(),
                               Vec::new(),
                               Bindings::new(),
                               UnificationIndex::Actor(0),
                               vec![Goal::new(Datum::from_sexp_str("((current-state 0) ((time ?t1::0)))").expect("SubGoal 1 datum"),
                                              Vec::new(),
                                              Vec::new(),
                                              Bindings::new(),
                                              UnificationIndex::Init,
                                              Vec::new()),
                                    Goal::new(Datum::from_sexp_str("((action 2) ((time ?t1::0)))").expect("SubGoal 1 datum"),
                                              Vec::new(),
                                              Vec::new(),
                                              Bindings::new(),
                                              UnificationIndex::Init,
                                              Vec::new())]),
                     Goal::new(Datum::from_sexp_str("((action 2) ((time ?t1::0)))").expect("SubGoal 2 datum"),
                               Vec::new(),
                               Vec::new(),
                               Bindings::new(),
                               UnificationIndex::Datum(0),
                               Vec::new())];
            let incremented_subgoals = initial_goal.increment_subgoals(&data_refs, &rule_refs, 0, 3);
            assert_eq!(incremented_subgoals.is_some(), true);
            let incremented_subgoals = incremented_subgoals.unwrap();

            for (incremented_subgoal, expected_incremented_goal) in incremented_subgoals.iter().zip(expected_incremented_subgoals.iter()) {
                assert_goal_spines_match(incremented_subgoal, expected_incremented_goal);
            }
        }
    }

    #[cfg(test)]
    mod increment_tests {
        use super::*;

        #[test]
        fn test_initial_increment_of_root_goal() {
            let data = vec![Datum::from_sexp_str("((current-state 0) ((time 0)))").expect("Test datum")];
            let rules = setup_rules();
            let data_refs: Vec<&Datum> = data.iter().collect();
            let rule_refs: Vec<&Rule<Datum, Datum>> = rules.iter().collect();

            let goal = Goal::new(Datum::from_sexp_str("((current-state 4) ((time ?t)))").expect("Goal datum"),
                                 Vec::new(),
                                 Vec::new(),
                                 Bindings::new(),
                                 UnificationIndex::Init,
                                 Vec::new());

            let expected_increments: Vec<Goal<Datum, Datum, Rule<Datum, Datum>>> = vec![// First goal
                     Goal::new(goal.pattern.clone(),
                               Vec::new(),
                               Vec::new(),
                               Bindings::new(),
                               UnificationIndex::Datum(0),
                               Vec::new()),
                     // Second goal
                     Goal::new(goal.pattern.clone(),
                               Vec::new(),
                               Vec::new(),
                               Bindings::new(),
                               UnificationIndex::Actor(0),
                               vec![Goal::new(Datum::from_sexp_str("((current-state 2) ((time ?t1::0)))").expect("SubGoal 1 datum"),
                                              Vec::new(),
                                              Vec::new(),
                                              Bindings::new(),
                                              UnificationIndex::Init,
                                              Vec::new()),
                                    Goal::new(Datum::from_sexp_str("((action 2) ((time ?t1::0)))").expect("SubGoal 2 datum"),
                                              Vec::new(),
                                              Vec::new(),
                                              Bindings::new(),
                                              UnificationIndex::Init,
                                              Vec::new())]),
                     // Third goal
                     Goal::new(goal.pattern.clone(),
                               Vec::new(),
                               Vec::new(),
                               Bindings::new(),
                               UnificationIndex::Actor(0),
                               vec![Goal::new(Datum::from_sexp_str("((current-state 2) ((time ?t1::0)))").expect("SubGoal 1 datum"),
                                              Vec::new(),
                                              Vec::new(),
                                              Bindings::new(),
                                              UnificationIndex::Datum(0),
                                              vec![]),
                                    Goal::new(Datum::from_sexp_str("((action 2) ((time ?t1::0)))").expect("SubGoal 2 datum"),
                                              Vec::new(),
                                              Vec::new(),
                                              Bindings::new(),
                                              UnificationIndex::Datum(0),
                                              Vec::new())]),
                     // Fourth goal
                     Goal::new(goal.pattern.clone(),
                               Vec::new(),
                               Vec::new(),
                               Bindings::new(),
                               UnificationIndex::Actor(0),
                               vec![Goal::new(Datum::from_sexp_str("((current-state 2) ((time ?t1::0)))").expect("SubGoal 1 datum"),
                                              Vec::new(),
                                              Vec::new(),
                                              Bindings::new(),
                                              UnificationIndex::Datum(0),
                                              Vec::new()),
                                    Goal::new(Datum::from_sexp_str("((action 2) ((time ?t1::0)))").expect("SubGoal 2 datum"),
                                              Vec::new(),
                                              Vec::new(),
                                              Bindings::new(),
                                              UnificationIndex::Actor(1),
                                              Vec::new())]),
                     // Fifth goal
                     Goal::new(goal.pattern.clone(),
                               Vec::new(),
                               Vec::new(),
                               Bindings::new(),
                               UnificationIndex::Actor(0),
                               vec![Goal::new(Datum::from_sexp_str("((current-state 2) ((time ?t1::0)))").expect("SubGoal 1 datum"),
                                              Vec::new(),
                                              Vec::new(),
                                              Bindings::new(),
                                              UnificationIndex::Actor(0),
                                              vec![Goal::new(Datum::from_sexp_str("((current-state 0) ((time ?t1::3)))")
                                                                 .expect("SubGoal 1 datum"),
                                                             Vec::new(),
                                                             Vec::new(),
                                                             Bindings::new(),
                                                             UnificationIndex::Init,
                                                             Vec::new()),
                                                   Goal::new(Datum::from_sexp_str("((action 2) ((time ?t1::3)))").expect("SubGoal 1 datum"),
                                                             Vec::new(),
                                                             Vec::new(),
                                                             Bindings::new(),
                                                             UnificationIndex::Init,
                                                             Vec::new())]),
                                    Goal::new(Datum::from_sexp_str("((action 2) ((time ?t1::0)))").expect("SubGoal 2 datum"),
                                              Vec::new(),
                                              Vec::new(),
                                              Bindings::new(),
                                              UnificationIndex::Datum(0),
                                              Vec::new())]),
                     // Sixth goal
                     Goal::new(goal.pattern.clone(),
                               Vec::new(),
                               Vec::new(),
                               Bindings::new(),
                               UnificationIndex::Actor(0),
                               vec![Goal::new(Datum::from_sexp_str("((current-state 2) ((time ?t1::0)))").expect("SubGoal 1 datum"),
                                              Vec::new(),
                                              Vec::new(),
                                              Bindings::new(),
                                              UnificationIndex::Actor(0),
                                              vec![Goal::new(Datum::from_sexp_str("((current-state 0) ((time ?t1::3)))")
                                                                 .expect("SubGoal 1 datum"),
                                                             Vec::new(),
                                                             Vec::new(),
                                                             Bindings::new(),
                                                             UnificationIndex::Init,
                                                             Vec::new()),
                                                   Goal::new(Datum::from_sexp_str("((action 2) ((time ?t1::3)))").expect("SubGoal 1 datum"),
                                                             Vec::new(),
                                                             Vec::new(),
                                                             Bindings::new(),
                                                             UnificationIndex::Init,
                                                             Vec::new())]),
                                    Goal::new(Datum::from_sexp_str("((action 2) ((time ?t1::0)))").expect("SubGoal 2 datum"),
                                              Vec::new(),
                                              Vec::new(),
                                              Bindings::new(),
                                              UnificationIndex::Actor(1),
                                              Vec::new())])];
            let mut incremented_goal = goal.increment(&data_refs, &rule_refs, 2, 3).expect("Initial plan");

            for (idx, expected_incremented_goal) in expected_increments.into_iter().enumerate() {
                println!("Step {}\n----Expected:\n{}\n\n----Actual:\n{}\n\n",
                         idx + 1,
                         expected_incremented_goal,
                         incremented_goal);
                assert_goal_spines_match(&incremented_goal, &expected_incremented_goal);
                incremented_goal = incremented_goal.increment(&data_refs, &rule_refs, idx, 3).expect("Initial plan");
            }
        }

        #[test]
        fn test_initial_increment_weird_case() {
            let data = vec![Datum::from_sexp_str("((current-state 0) ((time 0)))").expect("Test datum")];
            let rules = setup_rules();
            let data_refs: Vec<&Datum> = data.iter().collect();
            let rule_refs: Vec<&Rule<Datum, Datum>> = rules.iter().collect();

            let goal_pattern = Datum::from_sexp_str("((current-state 4) ((time ?t)))").expect("Goal datum");
            let initial_goal = Goal::new(goal_pattern.clone(),
                                         Vec::new(),
                                         Vec::new(),
                                         Bindings::new(),
                                         UnificationIndex::Actor(0),
                                         vec![Goal::new(Datum::from_sexp_str("((current-state 2) ((time ?t1::0)))").expect("SubGoal 1 datum"),
                                                        Vec::new(),
                                                        Vec::new(),
                                                        Bindings::new(),
                                                        UnificationIndex::Datum(0),
                                                        vec![]),
                                              Goal::new(Datum::from_sexp_str("((action 2) ((time ?t1::0)))").expect("SubGoal 2 datum"),
                                                        Vec::new(),
                                                        Vec::new(),
                                                        Bindings::new(),
                                                        UnificationIndex::Actor(1),
                                                        Vec::new())]);
            let expected_increment =
                Goal::new(goal_pattern.clone(),
                          Vec::new(),
                          Vec::new(),
                          Bindings::new(),
                          UnificationIndex::Actor(0),
                          vec![Goal::new(Datum::from_sexp_str("((current-state 2) ((time ?t1::0)))").expect("SubGoal 1 datum"),
                                         Vec::new(),
                                         Vec::new(),
                                         Bindings::new(),
                                         UnificationIndex::Actor(0),
                                         vec![Goal::new(Datum::from_sexp_str("((current-state 0) ((time ?t1::2)))").expect("SubGoal 1 datum"),
                                                        Vec::new(),
                                                        Vec::new(),
                                                        Bindings::new(),
                                                        UnificationIndex::Init,
                                                        Vec::new()),
                                              Goal::new(Datum::from_sexp_str("((action 2) ((time ?t1::2)))").expect("SubGoal 1 datum"),
                                                        Vec::new(),
                                                        Vec::new(),
                                                        Bindings::new(),
                                                        UnificationIndex::Init,
                                                        Vec::new())]),
                               Goal::new(Datum::from_sexp_str("((action 2) ((time ?t1::0)))").expect("SubGoal 2 datum"),
                                         Vec::new(),
                                         Vec::new(),
                                         Bindings::new(),
                                         UnificationIndex::Datum(0),
                                         Vec::new())]);
            let incremented_goal = initial_goal.increment(&data_refs, &rule_refs, 2, 3).expect("Initial plan");
            assert_goal_spines_match(&incremented_goal, &expected_increment);
        }
    }

    #[cfg(test)]
    mod satisfaction_tests {
        use super::*;

        #[test]
        fn test_goal_satisfied_returns_true_for_satisfying_datum() {
            let data = vec![Datum::from_sexp_str("((current-state 0) ((time 0)))").expect("Test datum")];
            let data_refs: Vec<&Datum> = data.iter().collect();

            let goal: Goal<Datum, Datum, Rule<Datum, Datum>> = Goal::new(Datum::from_sexp_str("((current-state 0) ((time ?t)))")
                                                                             .expect("Goal datum"),
                                                                         Vec::new(),
                                                                         Vec::new(),
                                                                         Bindings::new(),
                                                                         UnificationIndex::Datum(0),
                                                                         Vec::new());
            let expected_bindings: Bindings<Datum> = vec![("?t".to_string(), Datum::Float(0.0))].into_iter().collect();
            assert_eq!(goal.satisified(&data_refs, &Vec::new(), &Bindings::new()),
                       Some(expected_bindings));
        }

        #[test]
        fn test_goal_satisfied_returns_false_for_non_satisfying_datum() {
            let data = vec![Datum::from_sexp_str("((current-state 0) ((time 0)))").expect("Test datum")];
            let data_refs: Vec<&Datum> = data.iter().collect();

            let goal: Goal<Datum, Datum, Rule<Datum, Datum>> = Goal::new(Datum::from_sexp_str("((current-state 2) ((time ?t)))")
                                                                             .expect("Goal datum"),
                                                                         Vec::new(),
                                                                         Vec::new(),
                                                                         Bindings::new(),
                                                                         UnificationIndex::Datum(0),
                                                                         Vec::new());
            assert_eq!(goal.satisified(&data_refs, &Vec::new(), &Bindings::new()),
                       None);
        }

        #[test]
        fn test_goal_satisfied_returns_true_for_shallowly_nested_plan() {
            let data = vec![Datum::from_sexp_str("((current-state 0) ((time 0)))").expect("Test datum")];
            let data_refs: Vec<&Datum> = data.iter().collect();
            let rules = setup_rules();
            let rule_refs: Vec<&Rule<Datum, Datum>> = rules.iter().collect();

            let goal: Goal<Datum, Datum, Rule<Datum, Datum>> =
                Goal::new(Datum::from_sexp_str("((current-state 2) ((time ?t2::1)))").expect("SubGoal 1 datum"),
                          Vec::new(),
                          vec![Constraint::from_sexp_str("(constraint-set (?diff1::1 1))").expect("Constraint"),
                               Constraint::from_sexp_str("(constraint-set (?diff2::1 2))").expect("Constraint"),
                               Constraint::from_sexp_str("(constraint-sum (?s1::1 ?diff2::1 ?s2::1))").expect("Constraint"),
                               Constraint::from_sexp_str("(constraint-sum (?t1::1 ?diff1::1 ?t2::1))").expect("Constraint")],
                          Bindings::new(),
                          UnificationIndex::Actor(0),
                          vec![Goal::new(Datum::from_sexp_str("((current-state 0) ((time ?t1::1)))").expect("SubGoal 1 datum"),
                                         vec![Constraint::from_sexp_str(r#"(constraint-set (?diff1::1 1))"#).expect("Constraint"),
                                              Constraint::from_sexp_str(r#"(constraint-set (?diff2::1 2))"#).expect("Constraint"),
                                              Constraint::from_sexp_str(r#"(constraint-sum (?s1::1 ?diff2::1 ?s2::1))"#).expect("Constraint"),
                                              Constraint::from_sexp_str(r#"(constraint-sum (?t1::1 ?diff1::1 ?t2::1))"#).expect("Constraint"),
                                              Constraint::from_sexp_str(r#"(constraint-set (?diff1::2 1))"#).expect("Constraint"),
                                              Constraint::from_sexp_str(r#"(constraint-set (?diff2::2 2))"#).expect("Constraint"),
                                              Constraint::from_sexp_str(r#"(constraint-sum (?s1::2 ?diff2::2 ?s2::2))"#).expect("Constraint"),
                                              Constraint::from_sexp_str(r#"(constraint-sum (?t1::2 ?diff1::2 ?t2::2))"#).expect("Constraint")],
                                         Vec::new(),
                                         Bindings::new(),
                                         UnificationIndex::Datum(0),
                                         Vec::new()),
                               Goal::new(Datum::from_sexp_str("((action 2) ((time ?t1::1)))").expect("SubGoal 1 datum"),
                                         vec![Constraint::from_sexp_str(r#"(constraint-set (?diff1::1 1))"#).expect("Constraint"),
                                              Constraint::from_sexp_str(r#"(constraint-set (?diff2::1 2))"#).expect("Constraint"),
                                              Constraint::from_sexp_str(r#"(constraint-sum (?s1::1 ?diff2::1 ?s2::1))"#).expect("Constraint"),
                                              Constraint::from_sexp_str(r#"(constraint-sum (?t1::1 ?diff1::1 ?t2::1))"#).expect("Constraint"),
                                              Constraint::from_sexp_str(r#"(constraint-set (?diff1::2 1))"#).expect("Constraint"),
                                              Constraint::from_sexp_str(r#"(constraint-set (?diff2::2 2))"#).expect("Constraint"),
                                              Constraint::from_sexp_str(r#"(constraint-sum (?s1::2 ?diff2::2 ?s2::2))"#).expect("Constraint"),
                                              Constraint::from_sexp_str(r#"(constraint-sum (?t1::2 ?diff1::2 ?t2::2))"#).expect("Constraint")],
                                         Vec::new(),
                                         Bindings::new(),
                                         UnificationIndex::Actor(1),
                                         Vec::new())]);
            let result = goal.satisified(&data_refs, &rule_refs, &Bindings::new());
            assert_eq!(result.is_some(),
                       true,
                       "satisfied should have returned bindings");

            let bindings = result.unwrap();
            assert_eq!(bindings.get_binding(&"?t1::1".to_string()),
                       Some(Datum::Float(0.0)));
            assert_eq!(bindings.get_binding(&"?t2::1".to_string()),
                       Some(Datum::Float(1.0)));
        }

        #[test]
        fn test_goal_satisfied_returns_true_for_deeply_nested_plan() {
            let data = vec![Datum::from_sexp_str("((current-state 0) ((time 0)))").expect("Test datum")];
            let data_refs: Vec<&Datum> = data.iter().collect();
            let rules = setup_rules();
            let rule_refs: Vec<&Rule<Datum, Datum>> = rules.iter().collect();

            let goal: Goal<Datum, Datum, Rule<Datum, Datum>> =
                Goal::new(Datum::from_sexp_str("((current-state 4) ((time ?t)))").expect("Goal datum"),
                          Vec::new(),
                          vec![Constraint::from_sexp_str("(constraint-set (?diff1 1))").expect("Constraint"),
                               Constraint::from_sexp_str("(constraint-set (?diff2 2))").expect("Constraint"),
                               Constraint::from_sexp_str("(constraint-sum (?s2::1 ?diff2 ?s))").expect("Constraint"),
                               Constraint::from_sexp_str("(constraint-sum (?t2::1 ?diff1 ?t))").expect("Constraint")],
                          Bindings::new(),
                          UnificationIndex::Actor(0),
                          vec![Goal::new(Datum::from_sexp_str("((current-state 2) ((time ?t2::1)))").expect("SubGoal 1 datum"),
                                         Vec::new(),
                                         vec![Constraint::from_sexp_str("(constraint-set (?diff1::1 1))").expect("Constraint"),
                                              Constraint::from_sexp_str("(constraint-set (?diff2::1 2))").expect("Constraint"),
                                              Constraint::from_sexp_str("(constraint-sum (?s1::1 ?diff2::1 ?s2::1))").expect("Constraint"),
                                              Constraint::from_sexp_str("(constraint-sum (?t1::1 ?diff1::1 ?t2::1))").expect("Constraint")],
                                         Bindings::new(),
                                         UnificationIndex::Actor(0),
                                         vec![Goal::new(Datum::from_sexp_str("((current-state 0) ((time ?t1::1)))").expect("SubGoal 1 datum"),
                                                        vec![Constraint::from_sexp_str(r#"(constraint-set (?diff1::1 1))"#)
                                                                 .expect("Constraint"),
                                                             Constraint::from_sexp_str(r#"(constraint-set (?diff2::1 2))"#)
                                                                 .expect("Constraint"),
                                                             Constraint::from_sexp_str(r#"(constraint-sum (?s1::1 ?diff2::1 ?s2::1))"#)
                                                                 .expect("Constraint"),
                                                             Constraint::from_sexp_str(r#"(constraint-sum (?t1::1 ?diff1::1 ?t2::1))"#)
                                                                 .expect("Constraint"),
                                                             Constraint::from_sexp_str(r#"(constraint-set (?diff1::2 1))"#)
                                                                 .expect("Constraint"),
                                                             Constraint::from_sexp_str(r#"(constraint-set (?diff2::2 2))"#)
                                                                 .expect("Constraint"),
                                                             Constraint::from_sexp_str(r#"(constraint-sum (?s1::2 ?diff2::2 ?s2::2))"#)
                                                                 .expect("Constraint"),
                                                             Constraint::from_sexp_str(r#"(constraint-sum (?t1::2 ?diff1::2 ?t2::2))"#)
                                                                 .expect("Constraint"),
                                                             Constraint::from_sexp_str("(constraint-set (?diff1 1))").expect("Constraint"),
                                                             Constraint::from_sexp_str("(constraint-set (?diff2 2))").expect("Constraint"),
                                                             Constraint::from_sexp_str("(constraint-sum (?s2::1 ?diff2 ?s))")
                                                                 .expect("Constraint"),
                                                             Constraint::from_sexp_str("(constraint-sum (?t2::1 ?diff1 ?t))")
                                                                 .expect("Constraint")],
                                                        Vec::new(),
                                                        Bindings::new(),
                                                        UnificationIndex::Datum(0),
                                                        Vec::new()),
                                              Goal::new(Datum::from_sexp_str("((action 2) ((time ?t1::1)))").expect("SubGoal 1 datum"),
                                                        vec![Constraint::from_sexp_str(r#"(constraint-set (?diff1::1 1))"#)
                                                                 .expect("Constraint"),
                                                             Constraint::from_sexp_str(r#"(constraint-set (?diff2::1 2))"#)
                                                                 .expect("Constraint"),
                                                             Constraint::from_sexp_str(r#"(constraint-sum (?s1::1 ?diff2::1 ?s2::1))"#)
                                                                 .expect("Constraint"),
                                                             Constraint::from_sexp_str(r#"(constraint-sum (?t1::1 ?diff1::1 ?t2::1))"#)
                                                                 .expect("Constraint"),
                                                             Constraint::from_sexp_str(r#"(constraint-set (?diff1::2 1))"#)
                                                                 .expect("Constraint"),
                                                             Constraint::from_sexp_str(r#"(constraint-set (?diff2::2 2))"#)
                                                                 .expect("Constraint"),
                                                             Constraint::from_sexp_str(r#"(constraint-sum (?s1::2 ?diff2::2 ?s2::2))"#)
                                                                 .expect("Constraint"),
                                                             Constraint::from_sexp_str(r#"(constraint-sum (?t1::2 ?diff1::2 ?t2::2))"#)
                                                                 .expect("Constraint"),
                                                             Constraint::from_sexp_str("(constraint-set (?diff1 1))").expect("Constraint"),
                                                             Constraint::from_sexp_str("(constraint-set (?diff2 2))").expect("Constraint"),
                                                             Constraint::from_sexp_str("(constraint-sum (?s2::1 ?diff2 ?s))")
                                                                 .expect("Constraint"),
                                                             Constraint::from_sexp_str("(constraint-sum (?t2::1 ?diff1 ?t))")
                                                                 .expect("Constraint")],
                                                        Vec::new(),
                                                        Bindings::new(),
                                                        UnificationIndex::Actor(1),
                                                        Vec::new())]),
                               Goal::new(Datum::from_sexp_str("((action 2) ((time ?t2::1)))").expect("SubGoal 2 datum"),
                                         Vec::new(),
                                         vec![Constraint::from_sexp_str("(constraint-set (?diff1::1 1))").expect("Constraint"),
                                              Constraint::from_sexp_str("(constraint-set (?diff2::1 2))").expect("Constraint"),
                                              Constraint::from_sexp_str("(constraint-sum (?s1::1 ?diff2::1 ?s2::1))").expect("Constraint"),
                                              Constraint::from_sexp_str("(constraint-sum (?t1::1 ?diff1::1 ?t2::1))").expect("Constraint")],
                                         Bindings::new(),
                                         UnificationIndex::Actor(1),
                                         Vec::new())]);
            let result = goal.satisified(&data_refs, &rule_refs, &Bindings::new());
            assert_eq!(result.is_some(),
                       true,
                       "satisfied should have returned bindings");

            let bindings = result.unwrap();
            assert_eq!(bindings.get_binding(&"?t1::1".to_string()),
                       Some(Datum::Float(0.0)));
            assert_eq!(bindings.get_binding(&"?t2::1".to_string()),
                       Some(Datum::Float(1.0)));
            assert_eq!(bindings.get_binding(&"?t".to_string()),
                       Some(Datum::Float(2.0)));
        }

        #[test]
        fn test_goal_satisfied_returns_false_for_incomplete_nested_plan() {
            let data = vec![Datum::from_sexp_str("((current-state 0) ((time 0)))").expect("Test datum")];
            let data_refs: Vec<&Datum> = data.iter().collect();
            let rules = setup_rules();
            let rule_refs: Vec<&Rule<Datum, Datum>> = rules.iter().collect();

            let goal: Goal<Datum, Datum, Rule<Datum, Datum>> =
                Goal::new(Datum::from_sexp_str("((current-state 4) ((time ?t)))").expect("Goal datum"),
                          Vec::new(),
                          vec![Constraint::from_sexp_str("(constraint-set (?diff1::1 1))").expect("Constraint"),
                               Constraint::from_sexp_str("(constraint-set (?diff2::1 2))").expect("Constraint"),
                               Constraint::from_sexp_str("(constraint-sum (?s1::1 ?diff2::1 ?s2::1))").expect("Constraint"),
                               Constraint::from_sexp_str("(constraint-sum (?t1::1 ?diff1::1 ?t2::1))").expect("Constraint")],
                          Bindings::new(),
                          UnificationIndex::Actor(0),
                          vec![Goal::new(Datum::from_sexp_str("((current-state 2) ((time ?t1::1)))").expect("SubGoal 1 datum"),
                                         vec![Constraint::from_sexp_str("(constraint-set (?diff1::1 1))").expect("Constraint"),
                                              Constraint::from_sexp_str("(constraint-set (?diff2::1 2))").expect("Constraint"),
                                              Constraint::from_sexp_str("(constraint-sum (?s1::1 ?diff2::1 ?s2::1))").expect("Constraint"),
                                              Constraint::from_sexp_str("(constraint-sum (?t1::1 ?diff1::1 ?t2::1))").expect("Constraint")],
                                         vec![Constraint::from_sexp_str("(constraint-set (?diff1::4 1))").expect("Constraint"),
                                              Constraint::from_sexp_str("(constraint-set (?diff2::4 2))").expect("Constraint"),
                                              Constraint::from_sexp_str("(constraint-sum (?s1::4 ?diff2::4 ?s2::4))").expect("Constraint"),
                                              Constraint::from_sexp_str("(constraint-sum (?t1::4 ?diff1::4 ?t2::4))").expect("Constraint")],
                                         Bindings::new(),
                                         UnificationIndex::Init,
                                         Vec::new()),
                               Goal::new(Datum::from_sexp_str("((action 2) ((time ?t1::1)))").expect("SubGoal 2 datum"),
                                         vec![Constraint::from_sexp_str("(constraint-set (?diff1::1 1))").expect("Constraint"),
                                              Constraint::from_sexp_str("(constraint-set (?diff2::1 2))").expect("Constraint"),
                                              Constraint::from_sexp_str("(constraint-sum (?s1::1 ?diff2::1 ?s2::1))").expect("Constraint"),
                                              Constraint::from_sexp_str("(constraint-sum (?t1::1 ?diff1::1 ?t2::1))").expect("Constraint")],
                                         Vec::new(),
                                         Bindings::new(),
                                         UnificationIndex::Init,
                                         Vec::new())]);
            assert_eq!(goal.satisified(&data_refs, &rule_refs, &Bindings::new()),
                       None);
        }
    }

    #[cfg(test)]
    mod plan_iterator_tests {
        use super::*;
        use core::Bindings;

        #[test]
        fn test_plan_iterator() {
            let data = vec![Datum::from_sexp_str("((current-state 0) ((time 0)))").expect("Test datum")];
            let rules = setup_rules();
            let initial_goal = Goal::new(Datum::from_sexp_str("((current-state 4) ((time ?t)))").expect("Goal datum"),
                                         Vec::new(),
                                         Vec::new(),
                                         Bindings::new(),
                                         UnificationIndex::Init,
                                         Vec::new());

            let mut planner = Planner::new(&initial_goal,
                                           &Bindings::new(),
                                           &PlanningConfig::default(),
                                           data.iter().collect(),
                                           rules.iter().collect(),
                                           100);

            let expected_final_goal: Goal<Datum, Datum, Rule<Datum, Datum>> =
                Goal::new(Datum::from_sexp_str("((current-state 4) ((time ?t)))").expect("Goal datum"),
                          Vec::new(),
                          Vec::new(),
                          Bindings::new(),
                          UnificationIndex::Actor(0),
                          vec![Goal::new(Datum::from_sexp_str("((current-state 2) ((time ?t1::1)))").expect("SubGoal 1 datum"),
                                         Vec::new(),
                                         Vec::new(),
                                         Bindings::new(),
                                         UnificationIndex::Actor(0),
                                         vec![Goal::new(Datum::from_sexp_str("((current-state 0) ((time ?t1::4)))").expect("SubGoal 1 datum"),
                                                        Vec::new(),
                                                        Vec::new(),
                                                        Bindings::new(),
                                                        UnificationIndex::Datum(0),
                                                        Vec::new()),
                                              Goal::new(Datum::from_sexp_str("((action 2) ((time ?t1::4)))").expect("SubGoal 1 datum"),
                                                        Vec::new(),
                                                        Vec::new(),
                                                        Bindings::new(),
                                                        UnificationIndex::Actor(1),
                                                        Vec::new())]),
                               Goal::new(Datum::from_sexp_str("((action 2) ((time ?t1::1)))").expect("SubGoal 2 datum"),
                                         Vec::new(),
                                         Vec::new(),
                                         Bindings::new(),
                                         UnificationIndex::Actor(1),
                                         Vec::new())]);
            let result = planner.next();
            assert_eq!(result.is_some(), true);
            let (final_goal, _bindings) = result.unwrap();
            assert_goal_spines_match(&final_goal, &expected_final_goal);
        }
    }
}
