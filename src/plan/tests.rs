use plan::*;

#[cfg(test)]
mod fplan_tests {
    use core::ToSexp;
    use datum::Datum;
    use infer::Rule;
    use super::*;

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
    mod increment_tests {
        use super::*;

        #[test]
        fn test_initial_increment_of_root_goal() {
            let data = vec![Datum::from_sexp_str("((current-state 0) ((time 0)))").expect("Test datum")];
            let rules = setup_rules();
            let data_refs: Vec<&Datum> = data.iter().collect();
            let rule_refs: Vec<&Rule<Datum, Datum>> = rules.iter().collect();

            let goal = Goal::new(Datum::from_sexp_str("((current-state 4) ((time ?t)))").expect("Goal datum"),
                                 UnificationIndex::Init,
                                 Vec::new());

            let expected_increments: Vec<Goal<Datum, Datum, Rule<Datum, Datum>>> = vec![// First goal
                 Goal::new(goal.pattern.clone(), UnificationIndex::Datum(0), Vec::new()),
                 // Second goal
                 Goal::new(goal.pattern.clone(),
                           UnificationIndex::Actor(0),
                           vec![Goal::new(Datum::from_sexp_str("((current-state 2) ((time ?t1::0)))")
                                              .expect("SubGoal 1 datum"),
                                          UnificationIndex::Init,
                                          Vec::new()),
                                Goal::new(Datum::from_sexp_str("((action 2) ((time ?t1::0)))").expect("SubGoal 2 datum"),
                                          UnificationIndex::Init,
                                          Vec::new())]),
                 // Third goal
                 Goal::new(goal.pattern.clone(),
                           UnificationIndex::Actor(0),
                           vec![Goal::new(Datum::from_sexp_str("((current-state 2) ((time ?t1::0)))")
                                              .expect("SubGoal 1 datum"),
                                          UnificationIndex::Datum(0),
                                          vec![]),
                                Goal::new(Datum::from_sexp_str("((action 2) ((time ?t1::0)))").expect("SubGoal 2 datum"),
                                          UnificationIndex::Init,
                                          Vec::new())]),
                 // Fourth goal
                 Goal::new(goal.pattern.clone(),
                           UnificationIndex::Actor(0),
                           vec![Goal::new(Datum::from_sexp_str("((current-state 2) ((time ?t1::0)))")
                                              .expect("SubGoal 1 datum"),
                                          UnificationIndex::Datum(0),
                                          vec![]),
                                Goal::new(Datum::from_sexp_str("((action 2) ((time ?t1::0)))").expect("SubGoal 2 datum"),
                                          UnificationIndex::Datum(0),
                                          Vec::new())]),
                 // Fifth goal
                 Goal::new(goal.pattern.clone(),
                           UnificationIndex::Actor(0),
                           vec![Goal::new(Datum::from_sexp_str("((current-state 2) ((time ?t1::0)))")
                                              .expect("SubGoal 1 datum"),
                                          UnificationIndex::Datum(0),
                                          vec![]),
                                Goal::new(Datum::from_sexp_str("((action 2) ((time ?t1::0)))").expect("SubGoal 2 datum"),
                                          UnificationIndex::Actor(1),
                                          Vec::new())]),
                 // Sixth goal
                 Goal::new(goal.pattern.clone(),
                           UnificationIndex::Actor(2),
                           vec![Goal::new(Datum::from_sexp_str("((current-state 3) ((time ?t1::4)))")
                                              .expect("SubGoal 1 datum"),
                                          UnificationIndex::Init,
                                          vec![]),
                                Goal::new(Datum::from_sexp_str("((action 1) ((time ?t1::4)))").expect("SubGoal 2 datum"),
                                          UnificationIndex::Init,
                                          Vec::new())])];
            let mut incremented_goal = goal.increment(&data_refs, &rule_refs, 2).expect("Initial plan");

            for (idx, expected_incremented_goal) in expected_increments.into_iter().enumerate() {
                println!("Step {}\n----Expected:\n{}\n\n----Actual:\n{}\n\n",
                         idx,
                         expected_incremented_goal,
                         incremented_goal);
                assert_eq!(incremented_goal,
                           expected_incremented_goal,
                           "Stepped goal not as expected");
                incremented_goal = incremented_goal.increment(&data_refs, &rule_refs, idx).expect("Initial plan");
            }
        }
    }

    #[cfg(test)]
    mod satisfaction_tests {
        use super::*;

        #[test]
        fn test_goal_satisfied_returns_true_for_satisfying_datum() {
            let data = vec![Datum::from_sexp_str("((current-state 0) ((time 0)))").expect("Test datum")];
            let data_refs: Vec<&Datum> = data.iter().collect();

            let goal: Goal<Datum, Datum, Rule<Datum, Datum>> =
                Goal::new(Datum::from_sexp_str("((current-state 0) ((time ?t)))").expect("Goal datum"),
                          UnificationIndex::Datum(0),
                          Vec::new());
            let expected_bindings: Bindings<Datum> = vec![("?t".to_string(), Datum::Float(0.0))].into_iter().collect();
            assert_eq!(goal.satisifed(&data_refs, &Bindings::new()),
                       Some(expected_bindings));
        }

        #[test]
        fn test_goal_satisfied_returns_false_for_non_satisfying_datum() {
            let data = vec![Datum::from_sexp_str("((current-state 0) ((time 0)))").expect("Test datum")];
            let data_refs: Vec<&Datum> = data.iter().collect();

            let goal: Goal<Datum, Datum, Rule<Datum, Datum>> =
                Goal::new(Datum::from_sexp_str("((current-state 2) ((time ?t)))").expect("Goal datum"),
                          UnificationIndex::Datum(0),
                          Vec::new());
            assert_eq!(goal.satisifed(&data_refs, &Bindings::new()), None);
        }

        #[test]
        fn test_goal_satisfied_returns_true_for_satisfied_nested_plan() {
            let data = vec![Datum::from_sexp_str("((current-state 0) ((time 0)))").expect("Test datum")];
            let data_refs: Vec<&Datum> = data.iter().collect();

            let goal: Goal<Datum, Datum, Rule<Datum, Datum>> =
                Goal::new(Datum::from_sexp_str("((current-state 4) ((time ?t)))").expect("Goal datum"),
                          UnificationIndex::Actor(0),
                          vec![Goal::new(Datum::from_sexp_str("((current-state 2) ((time ?t1::0)))")
                                             .expect("SubGoal 1 datum"),
                                         UnificationIndex::Actor(0),
                                         vec![Goal::new(Datum::from_sexp_str("((current-state 0) ((time ?t1::2)))")
                                                            .expect("SubGoal 1 datum"),
                                                        UnificationIndex::Datum(0),
                                                        Vec::new()),
                                              Goal::new(Datum::from_sexp_str("((action 2) ((time ?t1::2)))")
                                                            .expect("SubGoal 1 datum"),
                                                        UnificationIndex::Actor(1),
                                                        Vec::new())]),
                               Goal::new(Datum::from_sexp_str("((action 2) ((time ?t1::0)))").expect("SubGoal 2 datum"),
                                         UnificationIndex::Actor(1),
                                         Vec::new())]);
            let expected_bindings: Bindings<Datum> = vec![("?t1::2".to_string(), Datum::Float(0.0))].into_iter().collect();
            assert_eq!(goal.satisifed(&data_refs, &Bindings::new()),
                       Some(expected_bindings));
        }
    }
}
