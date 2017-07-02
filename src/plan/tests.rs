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
    mod misc_tests {
        use super::*;

        #[test]
        fn test_find_reused_datum_returns_reused_datum() {
            let goal: Goal<Datum, Datum, Rule<Datum, Datum>> = Goal::new(Datum::Nil,
                                                                         Vec::new(),
                                                                         Vec::new(),
                                                                         Bindings::new(),
                                                                         UnificationIndex::Actor(0),
                                                                         vec![Goal::new(Datum::Nil,
                                                                                        Vec::new(),
                                                                                        Vec::new(),
                                                                                        Bindings::new(),
                                                                                        UnificationIndex::Datum(0),
                                                                                        Vec::new()),
                                                                              Goal::new(Datum::Nil,
                                                                                        Vec::new(),
                                                                                        Vec::new(),
                                                                                        Bindings::new(),
                                                                                        UnificationIndex::Datum(0),
                                                                                        Vec::new())]);
            assert_eq!(goal.find_reused_datum(&mut HashSet::new()), Some(0));
        }

        #[test]
        fn test_find_reused_datum_returns_nothing_when_no_reuse() {
            let goal: Goal<Datum, Datum, Rule<Datum, Datum>> = Goal::new(Datum::Nil,
                                                                         Vec::new(),
                                                                         Vec::new(),
                                                                         Bindings::new(),
                                                                         UnificationIndex::Actor(0),
                                                                         vec![Goal::new(Datum::Nil,
                                                                                        Vec::new(),
                                                                                        Vec::new(),
                                                                                        Bindings::new(),
                                                                                        UnificationIndex::Datum(0),
                                                                                        vec![]),
                                                                              Goal::new(Datum::Nil,
                                                                                        Vec::new(),
                                                                                        Vec::new(),
                                                                                        Bindings::new(),
                                                                                        UnificationIndex::Datum(1),
                                                                                        Vec::new())]);
            assert_eq!(goal.find_reused_datum(&mut HashSet::new()), None);
        }
    }

    #[cfg(test)]
    mod create_subgoals_tests {
        use super::*;

        #[test]
        fn test_create_subgoals() {
            let data = vec![Datum::from_sexp_str("((current-state 0) ((time 0)))").expect("Test datum")];
            let data_refs: Vec<&Datum> = data.iter().collect();
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
                                                   UnificationIndex::Datum(0),
                                                   Vec::new()),
                                         Goal::new(Datum::from_sexp_str("((action 2) ((time ?t1)))").expect("SubGoal 1 datum"),
                                                   Vec::new(),
                                                   vec![Constraint::from_sexp_str("(constraint-set (?diff1 1))").expect("Constraint"),
                                                        Constraint::from_sexp_str("(constraint-set (?diff2 2))").expect("Constraint"),
                                                        Constraint::from_sexp_str(r#"(constraint-sum (?s1 ?diff2 ?s2))"#).expect("Constraint"),
                                                        Constraint::from_sexp_str(r#"(constraint-sum (?t1 ?diff1 ?t2))"#).expect("Constraint")],
                                                   expected_bindings,
                                                   UnificationIndex::Actor(1),
                                                   Vec::new())];
            let actual_subgoals = Goal::create_subgoals(&goal_pattern,
                                                        rule_refs[0],
                                                        &Vec::new(),
                                                        &data_refs,
                                                        &rule_refs,
                                                        0,
                                                        5);
            assert_eq!(actual_subgoals.is_some(), true);
            for (idx, (actual_subgoal, expected_subgoal)) in actual_subgoals.unwrap().iter().zip(expected_subgoals.iter()).enumerate() {
                println!("Subgoal[{}] not as expected.\n:Expected:\n{}\nActual:\n{}\n",
                         idx,
                         expected_subgoal,
                         actual_subgoal);
                assert_eq!(actual_subgoal, expected_subgoal);
            }
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

            let expected_incremented_goal: Goal<Datum, Datum, Rule<Datum, Datum>> =
                Goal::new(goal.pattern.clone(),
                          Vec::new(),
                          Vec::new(),
                          Bindings::new(),
                          UnificationIndex::Actor(0),
                          vec![Goal::new(Datum::from_sexp_str("((current-state 2) ((time ?t1::2)))").expect("SubGoal 1 datum"),
                                         Vec::new(),
                                         Vec::new(),
                                         Bindings::new(),
                                         UnificationIndex::Actor(0),
                                         vec![Goal::new(Datum::from_sexp_str("((current-state 0) ((time ?t1::3)))").expect("SubGoal 1 datum"),
                                                        Vec::new(),
                                                        Vec::new(),
                                                        Bindings::new(),
                                                        UnificationIndex::Datum(0),
                                                        Vec::new()),
                                              Goal::new(Datum::from_sexp_str("((action 2) ((time ?t1::3)))").expect("SubGoal 1 datum"),
                                                        Vec::new(),
                                                        Vec::new(),
                                                        Bindings::new(),
                                                        UnificationIndex::Actor(1),
                                                        Vec::new())]),
                               Goal::new(Datum::from_sexp_str("((action 2) ((time ?t1::2)))").expect("SubGoal 2 datum"),
                                         Vec::new(),
                                         Vec::new(),
                                         Bindings::new(),
                                         UnificationIndex::Actor(1),
                                         Vec::new())]);
            let incremented_goal = goal.increment(&data_refs, &rule_refs, 2, 3).expect("Initial plan");
            assert_goal_spines_match(&incremented_goal, &expected_incremented_goal);
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
                Goal::new(Datum::from_sexp_str("((current-state 2) ((time ?t2)))").expect("SubGoal 1 datum"),
                          Vec::new(),
                          vec![Constraint::from_sexp_str("(constraint-set (?diff1 1))").expect("Constraint"),
                               Constraint::from_sexp_str("(constraint-set (?diff2 2))").expect("Constraint"),
                               Constraint::from_sexp_str("(constraint-sum (?s1 ?diff2 ?s2))").expect("Constraint"),
                               Constraint::from_sexp_str("(constraint-sum (?t1 ?diff1 ?t2))").expect("Constraint")],
                          Bindings::new(),
                          UnificationIndex::Actor(0),
                          vec![Goal::new(Datum::from_sexp_str("((current-state 0) ((time ?t1)))").expect("SubGoal 1 datum"),
                                         vec![Constraint::from_sexp_str(r#"(constraint-set (?diff1 1))"#).expect("Constraint"),
                                              Constraint::from_sexp_str(r#"(constraint-set (?diff2 2))"#).expect("Constraint"),
                                              Constraint::from_sexp_str(r#"(constraint-sum (?s1 ?diff2 ?s2))"#).expect("Constraint"),
                                              Constraint::from_sexp_str(r#"(constraint-sum (?t1 ?diff1 ?t2))"#).expect("Constraint")],
                                         Vec::new(),
                                         Bindings::new(),
                                         UnificationIndex::Datum(0),
                                         Vec::new()),
                               Goal::new(Datum::from_sexp_str("((action 2) ((time ?t1)))").expect("SubGoal 1 datum"),
                                         vec![Constraint::from_sexp_str(r#"(constraint-set (?diff1 1))"#).expect("Constraint"),
                                              Constraint::from_sexp_str(r#"(constraint-set (?diff2 2))"#).expect("Constraint"),
                                              Constraint::from_sexp_str(r#"(constraint-sum (?s1 ?diff2 ?s2))"#).expect("Constraint"),
                                              Constraint::from_sexp_str(r#"(constraint-sum (?t1 ?diff1 ?t2))"#).expect("Constraint")],
                                         Vec::new(),
                                         Bindings::new(),
                                         UnificationIndex::Actor(1),
                                         Vec::new())]);
            let result = goal.satisified(&data_refs, &rule_refs, &Bindings::new());
            assert_eq!(result.is_some(),
                       true,
                       "satisfied should have returned bindings");

            let bindings = result.unwrap();
            assert_eq!(bindings.get_binding(&"?t1".to_string()),
                       Some(Datum::Float(0.0)));
            assert_eq!(bindings.get_binding(&"?t2".to_string()),
                       Some(Datum::Float(1.0)));
        }

        #[test]
        fn test_goal_satisfied_returns_true_for_deeply_nested_plan() {
            let data = vec![Datum::from_sexp_str("((current-state 0) ((time 0)))").expect("Test datum")];
            let data_refs: Vec<&Datum> = data.iter().collect();
            let rules = setup_rules();
            let rule_refs: Vec<&Rule<Datum, Datum>> = rules.iter().collect();

            let goal: Goal<Datum, Datum, Rule<Datum, Datum>> =
                Goal::new(Datum::from_sexp_str("((current-state 4) ((time ?t2)))").expect("Goal datum"),
                          Vec::new(),
                          vec![Constraint::from_sexp_str("(constraint-set (?diff1 1))").expect("Constraint"),
                               Constraint::from_sexp_str("(constraint-set (?diff2 2))").expect("Constraint"),
                               Constraint::from_sexp_str("(constraint-sum (?s2 ?diff2 ?s1))").expect("Constraint"),
                               Constraint::from_sexp_str("(constraint-sum (?t2 ?diff1 ?t1))").expect("Constraint")],
                          Bindings::new(),
                          UnificationIndex::Actor(0),
                          vec![Goal::new(Datum::from_sexp_str("((current-state 2) ((time ?t1)))").expect("SubGoal 1 datum"),
                                         vec![Constraint::from_sexp_str("(constraint-set (?diff1 1))").expect("Constraint"),
                                              Constraint::from_sexp_str("(constraint-set (?diff2 2))").expect("Constraint"),
                                              Constraint::from_sexp_str("(constraint-sum (?s0 ?diff2 ?s1))").expect("Constraint"),
                                              Constraint::from_sexp_str("(constraint-sum (?t0 ?diff1 ?t1))").expect("Constraint")],
                                         vec![Constraint::from_sexp_str("(constraint-set (?diff1 1))").expect("Constraint"),
                                              Constraint::from_sexp_str("(constraint-set (?diff2 2))").expect("Constraint"),
                                              Constraint::from_sexp_str("(constraint-sum (?s1 ?diff2 ?s2))").expect("Constraint"),
                                              Constraint::from_sexp_str("(constraint-sum (?t1 ?diff1 ?t2))").expect("Constraint")],
                                         Bindings::new(),
                                         UnificationIndex::Actor(0),
                                         vec![Goal::new(Datum::from_sexp_str("((current-state 0) ((time ?t0)))").expect("SubGoal 1 datum"),
                                                        vec![Constraint::from_sexp_str("(constraint-set (?diff1 1))").expect("Constraint"),
                                                             Constraint::from_sexp_str("(constraint-set (?diff2 2))").expect("Constraint"),
                                                             Constraint::from_sexp_str("(constraint-sum (?s0 ?diff2 ?s1))")
                                                                 .expect("Constraint"),
                                                             Constraint::from_sexp_str("(constraint-sum (?t0 ?diff1 ?t1))")
                                                                 .expect("Constraint"),
                                                             Constraint::from_sexp_str("(constraint-set (?diff1 1))").expect("Constraint"),
                                                             Constraint::from_sexp_str("(constraint-set (?diff2 2))").expect("Constraint"),
                                                             Constraint::from_sexp_str("(constraint-sum (?s1 ?diff2 ?s2))")
                                                                 .expect("Constraint"),
                                                             Constraint::from_sexp_str("(constraint-sum (?t1 ?diff1 ?t2))")
                                                                 .expect("Constraint")],
                                                        Vec::new(),
                                                        Bindings::new(),
                                                        UnificationIndex::Datum(0),
                                                        Vec::new()),
                                              Goal::new(Datum::from_sexp_str("((action 2) ((time ?t1::1)))").expect("SubGoal 1 datum"),
                                                        vec![Constraint::from_sexp_str("(constraint-set (?diff1 1))").expect("Constraint"),
                                                             Constraint::from_sexp_str("(constraint-set (?diff2 2))").expect("Constraint"),
                                                             Constraint::from_sexp_str("(constraint-sum (?s0 ?diff2 ?s1))")
                                                                 .expect("Constraint"),
                                                             Constraint::from_sexp_str("(constraint-sum (?t0 ?diff1 ?t1))")
                                                                 .expect("Constraint"),
                                                             Constraint::from_sexp_str("(constraint-set (?diff1 1))").expect("Constraint"),
                                                             Constraint::from_sexp_str("(constraint-set (?diff2 2))").expect("Constraint"),
                                                             Constraint::from_sexp_str("(constraint-sum (?s1 ?diff2 ?s2))")
                                                                 .expect("Constraint"),
                                                             Constraint::from_sexp_str("(constraint-sum (?t1 ?diff1 ?t2))")
                                                                 .expect("Constraint")],
                                                        Vec::new(),
                                                        Bindings::new(),
                                                        UnificationIndex::Actor(1),
                                                        Vec::new())]),
                               Goal::new(Datum::from_sexp_str("((action 2) ((time ?t2::1)))").expect("SubGoal 2 datum"),
                                         vec![Constraint::from_sexp_str("(constraint-set (?diff1 1))").expect("Constraint"),
                                              Constraint::from_sexp_str("(constraint-set (?diff2 2))").expect("Constraint"),
                                              Constraint::from_sexp_str("(constraint-sum (?s0 ?diff2 ?s1))").expect("Constraint"),
                                              Constraint::from_sexp_str("(constraint-sum (?t0 ?diff1 ?t1))").expect("Constraint")],
                                         vec![Constraint::from_sexp_str("(constraint-set (?diff1 1))").expect("Constraint"),
                                              Constraint::from_sexp_str("(constraint-set (?diff2 2))").expect("Constraint"),
                                              Constraint::from_sexp_str("(constraint-sum (?s1 ?diff2 ?s2))").expect("Constraint"),
                                              Constraint::from_sexp_str("(constraint-sum (?t1 ?diff1 ?t2))").expect("Constraint")],
                                         Bindings::new(),
                                         UnificationIndex::Actor(1),
                                         Vec::new())]);
            let result = goal.satisified(&data_refs, &rule_refs, &Bindings::new());
            assert_eq!(result.is_some(),
                       true,
                       "satisfied should have returned bindings");

            let bindings = result.unwrap();
            assert_eq!(bindings.get_binding(&"?t0".to_string()),
                       Some(Datum::Float(0.0)));
            assert_eq!(bindings.get_binding(&"?t1".to_string()),
                       Some(Datum::Float(1.0)));
            assert_eq!(bindings.get_binding(&"?t2".to_string()),
                       Some(Datum::Float(2.0)));
        }

        #[test]
        fn test_goal_satisfied_returns_false_for_incomplete_nested_plan() {
            let data = vec![Datum::from_sexp_str("((current-state 0) ((time 0)))").expect("Test datum")];
            let data_refs: Vec<&Datum> = data.iter().collect();
            let rules = setup_rules();
            let rule_refs: Vec<&Rule<Datum, Datum>> = rules.iter().collect();

            let goal: Goal<Datum, Datum, Rule<Datum, Datum>> =
                Goal::new(Datum::from_sexp_str("((current-state 4) ((time ?t2)))").expect("Goal datum"),
                          Vec::new(),
                          vec![Constraint::from_sexp_str("(constraint-set (?diff1 1))").expect("Constraint"),
                               Constraint::from_sexp_str("(constraint-set (?diff2 2))").expect("Constraint"),
                               Constraint::from_sexp_str("(constraint-sum (?s2 ?diff2 ?s1))").expect("Constraint"),
                               Constraint::from_sexp_str("(constraint-sum (?t2 ?diff1 ?t1))").expect("Constraint")],
                          Bindings::new(),
                          UnificationIndex::Actor(0),
                          vec![Goal::new(Datum::from_sexp_str("((current-state 2) ((time ?t1)))").expect("SubGoal 1 datum"),
                                         vec![Constraint::from_sexp_str("(constraint-set (?diff1 1))").expect("Constraint"),
                                              Constraint::from_sexp_str("(constraint-set (?diff2 2))").expect("Constraint"),
                                              Constraint::from_sexp_str("(constraint-sum (?s0 ?diff2 ?s1))").expect("Constraint"),
                                              Constraint::from_sexp_str("(constraint-sum (?t0 ?diff1 ?t1))").expect("Constraint")],
                                         vec![Constraint::from_sexp_str("(constraint-set (?diff1 1))").expect("Constraint"),
                                              Constraint::from_sexp_str("(constraint-set (?diff2 2))").expect("Constraint"),
                                              Constraint::from_sexp_str("(constraint-sum (?s1 ?diff2 ?s2))").expect("Constraint"),
                                              Constraint::from_sexp_str("(constraint-sum (?t1 ?diff1 ?t2))").expect("Constraint")],
                                         Bindings::new(),
                                         UnificationIndex::Init,
                                         Vec::new()),
                               Goal::new(Datum::from_sexp_str("((action 2) ((time ?t2::1)))").expect("SubGoal 2 datum"),
                                         vec![Constraint::from_sexp_str("(constraint-set (?diff1 1))").expect("Constraint"),
                                              Constraint::from_sexp_str("(constraint-set (?diff2 2))").expect("Constraint"),
                                              Constraint::from_sexp_str("(constraint-sum (?s0 ?diff2 ?s1))").expect("Constraint"),
                                              Constraint::from_sexp_str("(constraint-sum (?t0 ?diff1 ?t1))").expect("Constraint")],
                                         vec![Constraint::from_sexp_str("(constraint-set (?diff1 1))").expect("Constraint"),
                                              Constraint::from_sexp_str("(constraint-set (?diff2 2))").expect("Constraint"),
                                              Constraint::from_sexp_str("(constraint-sum (?s1 ?diff2 ?s2))").expect("Constraint"),
                                              Constraint::from_sexp_str("(constraint-sum (?t1 ?diff1 ?t2))").expect("Constraint")],
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
                Goal::new(Datum::from_sexp_str("((current-state 4) ((time 2)))").expect("Goal datum"),
                          Vec::new(),
                          Vec::new(),
                          Bindings::new(),
                          UnificationIndex::Actor(0),
                          vec![Goal::new(Datum::from_sexp_str("((current-state 2) ((time 1)))").expect("SubGoal 1 datum"),
                                         Vec::new(),
                                         Vec::new(),
                                         Bindings::new(),
                                         UnificationIndex::Actor(0),
                                         vec![Goal::new(Datum::from_sexp_str("((current-state 0) ((time 0)))").expect("SubGoal 1 datum"),
                                                        Vec::new(),
                                                        Vec::new(),
                                                        Bindings::new(),
                                                        UnificationIndex::Datum(0),
                                                        Vec::new()),
                                              Goal::new(Datum::from_sexp_str("((action 2) ((time 0)))").expect("SubGoal 1 datum"),
                                                        Vec::new(),
                                                        Vec::new(),
                                                        Bindings::new(),
                                                        UnificationIndex::Actor(1),
                                                        Vec::new())]),
                               Goal::new(Datum::from_sexp_str("((action 2) ((time 1)))").expect("SubGoal 2 datum"),
                                         Vec::new(),
                                         Vec::new(),
                                         Bindings::new(),
                                         UnificationIndex::Actor(1),
                                         Vec::new())]);
            let result = planner.next();
            assert_eq!(result.is_some(), true);
            let (final_goal, bindings) = result.unwrap();
            assert_goal_spines_match(&final_goal.apply_bindings(&bindings).unwrap(),
                                     &expected_final_goal);
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

            let initial_goal = Goal::with_pattern(Datum::from_sexp_str("(sen)").unwrap());
            let mut planner = Planner::new(&initial_goal,
                                           &Bindings::new(),
                                           &PlanningConfig {
                                               max_depth: 5,
                                               reuse_data: false,
                                           },
                                           data.iter().collect(),
                                           rules.iter().collect(),
                                           50);

            let result = planner.next();
            assert_eq!(result.is_some(), true);
            let (final_goal, bindings) = result.unwrap();

            let expected_leaves: Vec<Datum> = vec!["a".to_string(), "dog".to_string(), "chased".to_string(), "the".to_string(), "cat".to_string()]
                .into_iter()
                .map(|s| Datum::String(s))
                .collect();
            assert_eq!(final_goal.gather_leaves(&bindings), expected_leaves);
        }
    }
}
