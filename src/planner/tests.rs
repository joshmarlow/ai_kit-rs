use planner::*;

#[cfg(test)]
mod planner_tests {
    use datum::Datum;
    use infer::Rule;
    use super::*;

    /// Recursively compares the 'spines' of two goal trees
    /// where the 'spine' is the goal pattern and the unification indexes, as well as the number of subgoals
    fn assert_goal_spines_match(actual_goal: &Goal<Datum, Datum, Rule<Datum, Datum>>, expected_goal: &Goal<Datum, Datum, Rule<Datum, Datum>>) {
        if actual_goal.pattern != expected_goal.pattern {
            panic!("Patterns don't match - actual:\n\t{}\nbut expected:\n\t{}\n",
                   actual_goal.pattern,
                   expected_goal.pattern);
        }
        if actual_goal.unification_index != expected_goal.unification_index {
            panic!("Unification indexes don't match - actual:\n\t{}\nbut expected:\n\t{}\n",
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
        let physics_rule_1 = from_json!(Rule<Datum, Datum>, {
            "lhs": [
              {"vec": [{"vec": [{"str": "current-state"}, {"var": "?s1"}]},
                       {"vec": [{"str": "time"}, {"var": "?t1"}]}]},
              {"vec": [{"vec": [{"str": "action"}, {"int": 1}]},
                       {"vec": [{"str": "time"}, {"var": "?t1"}]}]}],
            "rhs": {"vec": [{"vec": [{"str": "current-state"}, {"var": "?s2"}]},
                            {"vec": [{"str": "time"}, {"var": "?t2"}]}]},
            "constraints": [{"numerical": {"set": {"variable": "?diff", "constant": 1}}},
                            {"numerical": {"sum": {"first": "?s1", "second": "?diff", "third": "?s2"}}},
                            {"numerical": {"sum": {"first": "?t1", "second": "?diff", "third": "?t2"}}}]
      });
        let physics_rule_2 = from_json!(Rule<Datum, Datum>, {
            "lhs": [
              {"vec": [{"vec": [{"str": "current-state"}, {"var": "?s1"}]},
                       {"vec": [{"str": "time"}, {"var": "?t1"}]}]},
              {"vec": [{"vec": [{"str": "action"}, {"int": 2}]},
                       {"vec": [{"str": "time"}, {"var": "?t1"}]}]}],
            "rhs": {"vec": [{"vec": [{"str": "current-state"}, {"var": "?s2"}]},
                            {"vec": [{"str": "time"}, {"var": "?t2"}]}]},
            "constraints": [{"numerical": {"set": {"variable": "?diff", "constant": 1}}},
                            {"numerical": {"set": {"variable": "?diff2", "constant": 2}}},
                            {"numerical": {"sum": {"first": "?s1", "second": "?diff2", "third": "?s2"}}},
                            {"numerical": {"sum": {"first": "?t1", "second": "?diff", "third": "?t2"}}}]
          });
        let interaction_model_add_1 = from_json!(Rule<Datum, Datum>, {
            "rhs": {"vec": [{"vec": [{"str": "action"}, {"int": 1}]},
                            {"vec": [{"str": "time"}, {"var": "?t"}]}]}});
        let interaction_model_add_2 = from_json!(Rule<Datum, Datum>, {
            "rhs": {"vec": [{"vec": [{"str": "action"}, {"int": 2}]},
                            {"vec": [{"str": "time"}, {"var": "?t"}]}]}});

        vec![physics_rule_2, interaction_model_add_2, physics_rule_1, interaction_model_add_1]
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
            let data = vec![datum_json!({"vec": [
                {"vec":[{"str": "current-state"}, {"float": 0}]},
                {"vec":[{"str": "time"}, {"float": 0}]},
            ]})];
            let data_refs: Vec<&Datum> = data.iter().collect();
            let rules = setup_rules();
            let rule_refs: Vec<&Rule<Datum, Datum>> = rules.iter().collect();

            let goal_pattern = datum_json!({"vec": [
                {"vec":[{"str": "current-state"}, {"float": 2}]},
                {"vec":[{"str": "time"}, {"var": "?t"}]},
            ]});

            let expected_subgoals = from_json!(Vec<Goal<Datum, Datum, Rule<Datum, Datum>>>, [
                  {
                    "pattern": {"vec": [
                        {"vec":[{"str": "current-state"}, {"float": 0}]},
                        {"vec":[{"str": "time"}, {"var": "?t1"}]},
                    ]},
                    "unification_index": {"datum": 0},
                    "constraints": [
                      {"numerical": {"set": {"variable": "?diff1", "constant": 1}}},
                      {"numerical": {"set": {"variable": "?diff2", "constant": 2}}},
                      {"numerical": {"sum": {"first": "?s1", "second": "?diff2", "third": "?s2"}}},
                      {"numerical": {"sum": {"first": "?t1", "second": "?diff1", "third": "?t2"}}}
                    ]
                 },
                 {
                    "pattern": {"vec": [
                        {"vec":[{"str": "action"}, {"int": 2}]},
                        {"vec":[{"str": "time"}, {"var": "?t1"}]},
                    ]},
                    "unification_index": {"actor": 1},
                    "constraints": [
                      {"numerical": {"set": {"variable": "?diff1", "constant": 1}}},
                      {"numerical": {"set": {"variable": "?diff2", "constant": 2}}},
                      {"numerical": {"sum": {"first": "?s1", "second": "?diff2", "third": "?s2"}}},
                      {"numerical": {"sum": {"first": "?t1", "second": "?diff1", "third": "?t2"}}}
                    ]
                 }
            ]);

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
                assert_goal_spines_match(&actual_subgoal, &expected_subgoal);
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
    mod increment_tests {
        use super::*;

        #[test]
        fn test_initial_increment_of_root_goal() {
            let data = vec![datum_json!({"vec": [
                {"vec":[{"str": "current-state"}, {"float": 0}]},
                {"vec":[{"str": "time"}, {"float": 0}]},
            ]})];
            let rules = setup_rules();
            let data_refs: Vec<&Datum> = data.iter().collect();
            let rule_refs: Vec<&Rule<Datum, Datum>> = rules.iter().collect();

            let goal: Goal<Datum, Datum, Rule<Datum, Datum>> = from_json!(Goal<Datum, Datum, Rule<Datum, Datum>>, {
                "pattern": {
                  "vec": [
                    {"vec": [{"str": "current-state"}, {"float": 4}]},
                    {"vec": [{"str": "time"}, {"var": "?t"}]}
                  ]
                }
            });

            let expected_incremented_goal = from_json!(Goal<Datum, Datum, Rule<Datum, Datum>>, {
                "pattern": {
                  "vec": [
                    {"vec": [{"str": "current-state"}, {"float": 4}]},
                    {"vec": [{"str": "time"}, {"var": "?t"}]},
                  ]
                },
                "unification_index": {"actor": 0},
                "subgoals": [
                  {
                      "pattern": {
                        "vec": [
                          {"vec": [{"str": "current-state"}, {"float": 2}]},
                          {"vec": [{"str": "time"}, {"var": "?t1::2"}]},
                        ]
                      },
                      "unification_index": {"actor": 0},
                      "subgoals": [
                        {
                            "pattern": {
                              "vec": [
                                {"vec": [{"str": "current-state"}, {"float": 0}]},
                                {"vec": [{"str": "time"}, {"var": "?t1::3"}]},
                              ]
                            },
                            "unification_index": {"datum": 0}
                        },
                        {
                            "pattern": {
                              "vec": [
                                {"vec": [{"str": "action"}, {"int": 2}]},
                                {"vec": [{"str": "time"}, {"var": "?t1::3"}]},
                              ]
                            },
                            "unification_index": {"actor": 1}
                        }
                      ]
                  },
                  {
                      "pattern": {
                        "vec": [
                          {"vec": [{"str": "action"}, {"int": 2}]},
                          {"vec": [{"str": "time"}, {"var": "?t1::2"}]}
                        ]
                      },
                      "unification_index": {"actor": 1}
                  }
                ]
            });

            let incremented_goal = goal.increment(&data_refs, &rule_refs, 2, 3).expect("Initial plan");
            assert_goal_spines_match(&incremented_goal, &expected_incremented_goal);
        }
    }

    #[cfg(test)]
    mod satisfaction_tests {
        use super::*;

        #[test]
        fn test_goal_satisfied_returns_true_for_satisfying_datum() {
            let data = vec![datum_json!({"vec": [
                {"vec":[ {"str": "current-state"}, {"float": 0}]},
                {"vec":[ {"str": "time"}, {"float": 0}]},
            ]})];
            let data_refs: Vec<&Datum> = data.iter().collect();

            let goal = from_json!(Goal<Datum, Datum, Rule<Datum, Datum>>, {
              "pattern": {"vec": [
                  {"vec":[ {"str": "current-state"}, {"float": 0}]},
                  {"vec":[ {"str": "time"}, {"var": "?t"}]},
              ]},
              "unification_index": {"datum": 0}
            });
            let expected_bindings = Bindings::new().set_binding(&"?t".to_string(), Datum::Float(0.0));
            assert_eq!(goal.satisified(&data_refs, &Vec::new(), &Bindings::new()),
                       Some(expected_bindings));
        }

        #[test]
        fn test_goal_satisfied_returns_false_for_non_satisfying_datum() {
            let data = vec![datum_json!({"vec": [
                {"vec":[ {"str": "current-state"}, {"float": 0}]},
                {"vec":[ {"str": "time"}, {"float": 0}]},
            ]})];
            let data_refs: Vec<&Datum> = data.iter().collect();

            let goal = from_json!(Goal<Datum, Datum, Rule<Datum, Datum>>, {
              "pattern": {"vec": [
                  {"vec":[ {"str": "current-state"}, {"float": 2}]},
                  {"vec":[ {"str": "time"}, {"var": "?t"}]},
              ]},
              "unification_index": {"datum": 0}
            });
            assert_eq!(goal.satisified(&data_refs, &Vec::new(), &Bindings::new()),
                       None);
        }

        #[test]
        fn test_goal_satisfied_returns_true_for_shallowly_nested_plan() {
            let data = vec![datum_json!({"vec": [
                {"vec":[ {"str": "current-state"}, {"float": 0}]},
                {"vec":[ {"str": "time"}, {"float": 0}]},
            ]})];
            let data_refs: Vec<&Datum> = data.iter().collect();
            let rules = setup_rules();
            let rule_refs: Vec<&Rule<Datum, Datum>> = rules.iter().collect();

            let goal = goal_json!(Goal<Datum, Datum, Rule<Datum, Datum>>, {
              "pattern": {
                "vec": [
                  {"vec": [{"str": "current-state"}, {"float": 2}]},
                  {"vec": [{"str": "time"}, {"var": "?t2"}]}
                ]
              },
              "unification_index": {"actor": 0},
              "constraints": [
                {"numerical": {"set": {"variable": "?diff1", "constant": 1.0}}},
                {"numerical": {"set": {"variable": "?diff2", "constant": 2.0}}},
                {"numerical": {"sum": {"first": "?s1", "second": "?diff2", "third": "?s2"}}},
                {"numerical": {"sum": {"first": "?t1", "second": "?diff1", "third": "?t2"}}},
              ],
              "subgoals": [
                {
                    "pattern": {
                      "vec": [
                          {"vec": [{"str": "current-state"}, {"float": 0}]},
                          {"vec": [{"str": "time"}, {"var": "?t1"}]}
                      ],
                    },
                    "unification_index": {"datum": 0},
                    "parental_constraints": [
                        {"numerical": {"set": {"variable": "?diff1", "constant": 1.0}}},
                        {"numerical": {"set": {"variable": "?diff2", "constant": 2.0}}},
                        {"numerical": {"sum": {"first": "?s1", "second": "?diff2", "third": "?s2"}}},
                        {"numerical": {"sum": {"first": "?t1", "second": "?diff1", "third": "?t2"}}},
                     ],
                },
                {
                    "pattern": {
                      "vec": [
                        {"vec": [{ "str": "action" }, { "int": 2 }]},
                        {"vec": [{ "str": "time" }, { "var": "?t1" }]},
                      ],
                    },
                    "unification_index": {"actor": 1},
                    "parental_constraints": [
                        {"numerical": {"set": {"variable": "?diff1", "constant": 1.0}}},
                        {"numerical": {"set": {"variable": "?diff2", "constant": 2.0}}},
                        {"numerical": {"sum": {"first": "?s1", "second": "?diff2", "third": "?s2"}}},
                        {"numerical": {"sum": {"first": "?t1", "second": "?diff1", "third": "?t2"}}},
                     ]
                }
              ]
            });
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
            let data = vec![datum_json!({"vec": [
                {"vec":[ {"str": "current-state"}, {"float": 0}]},
                {"vec":[ {"str": "time"}, {"float": 0}]},
            ]})];
            let data_refs: Vec<&Datum> = data.iter().collect();
            let rules = setup_rules();
            let rule_refs: Vec<&Rule<Datum, Datum>> = rules.iter().collect();

            let goal = goal_json!(Goal<Datum, Datum, Rule<Datum, Datum>>, {
              "pattern": {
                "vec": [
                  {"vec": [{"str": "current-state"}, {"float": 4}]},
                  {"vec": [{"str": "time"}, {"var": "?t3"}]}
                ]
              },
              "unification_index": {"actor": 0},
              "constraints": [
                {"numerical": {"set": {"variable": "?diff1", "constant": 1.0}}},
                {"numerical": {"set": {"variable": "?diff2", "constant": 2.0}}},
                {"numerical": {"sum": {"first": "?s2", "second": "?diff2", "third": "?s3"}}},
                {"numerical": {"sum": {"first": "?t2", "second": "?diff1", "third": "?t3"}}},
              ],
              "subgoals": [
                {
                  "pattern": {
                    "vec": [
                      {"vec": [{"str": "current-state"}, {"float": 2}]},
                      {"vec": [{"str": "time"}, {"var": "?t2"}]}
                    ]
                  },
                  "unification_index": {"actor": 0},
                  "parental_constraints": [
                      {"numerical": {"set": {"variable": "?diff1", "constant": 1.0}}},
                      {"numerical": {"set": {"variable": "?diff2", "constant": 2.0}}},
                      {"numerical": {"sum": {"first": "?s2", "second": "?diff2", "third": "?s3"}}},
                      {"numerical": {"sum": {"first": "?t2", "second": "?diff1", "third": "?t3"}}},
                   ],
                  "constraints": [
                    {"numerical": {"set": {"variable": "?diff1", "constant": 1.0}}},
                    {"numerical": {"set": {"variable": "?diff2", "constant": 2.0}}},
                    {"numerical": {"sum": {"first": "?s1", "second": "?diff2", "third": "?s2"}}},
                    {"numerical": {"sum": {"first": "?t1", "second": "?diff1", "third": "?t2"}}},
                  ],
                  "subgoals": [
                    {
                        "pattern": {
                          "vec": [
                              {"vec": [{"str": "current-state"}, {"float": 0}]},
                              {"vec": [{"str": "time"}, {"var": "?t1"}]}
                          ],
                        },
                        "unification_index": {"datum": 0},
                        "parental_constraints": [
                            {"numerical": {"set": {"variable": "?diff1", "constant": 1.0}}},
                            {"numerical": {"set": {"variable": "?diff2", "constant": 2.0}}},
                            {"numerical": {"sum": {"first": "?s2", "second": "?diff2", "third": "?s3"}}},
                            {"numerical": {"sum": {"first": "?t2", "second": "?diff1", "third": "?t3"}}},
                            {"numerical": {"set": {"variable": "?diff1", "constant": 1.0}}},
                            {"numerical": {"set": {"variable": "?diff2", "constant": 2.0}}},
                            {"numerical": {"sum": {"first": "?s1", "second": "?diff2", "third": "?s2"}}},
                            {"numerical": {"sum": {"first": "?t1", "second": "?diff1", "third": "?t2"}}},
                         ],
                    },
                    {
                        "pattern": {
                          "vec": [
                            {"vec": [{ "str": "action" }, { "int": 2 }]},
                            {"vec": [{ "str": "time" }, { "var": "?t1" }]},
                          ],
                        },
                        "unification_index": {"actor": 1},
                        "parental_constraints": [
                            {"numerical": {"set": {"variable": "?diff1", "constant": 1.0}}},
                            {"numerical": {"set": {"variable": "?diff2", "constant": 2.0}}},
                            {"numerical": {"sum": {"first": "?s2", "second": "?diff2", "third": "?s3"}}},
                            {"numerical": {"sum": {"first": "?t2", "second": "?diff1", "third": "?t3"}}},
                            {"numerical": {"set": {"variable": "?diff1", "constant": 1.0}}},
                            {"numerical": {"set": {"variable": "?diff2", "constant": 2.0}}},
                            {"numerical": {"sum": {"first": "?s1", "second": "?diff2", "third": "?s2"}}},
                            {"numerical": {"sum": {"first": "?t1", "second": "?diff1", "third": "?t2"}}},
                         ]
                    }
                  ]
                },
                {
                    "pattern": {
                      "vec": [
                        {"vec": [{ "str": "action" }, { "int": 2 }]},
                        {"vec": [{ "str": "time" }, { "var": "?t2" }]},
                      ],
                    },
                    "unification_index": {"actor": 1},
                    "parental_constraints": [
                        {"numerical": {"set": {"variable": "?diff1", "constant": 1.0}}},
                        {"numerical": {"set": {"variable": "?diff2", "constant": 2.0}}},
                        {"numerical": {"sum": {"first": "?s2", "second": "?diff2", "third": "?s3"}}},
                        {"numerical": {"sum": {"first": "?t2", "second": "?diff1", "third": "?t3"}}},
                     ]
                }
              ]
            });

            let result = goal.satisified(&data_refs, &rule_refs, &Bindings::new());
            assert_eq!(result.is_some(),
                       true,
                       "satisfied should have returned bindings");

            let bindings = result.unwrap();
            assert_eq!(bindings.get_binding(&"?t1".to_string()),
                       Some(Datum::Float(0.0)));
            assert_eq!(bindings.get_binding(&"?t2".to_string()),
                       Some(Datum::Float(1.0)));
            assert_eq!(bindings.get_binding(&"?t3".to_string()),
                       Some(Datum::Float(2.0)));
        }

        #[test]
        fn test_goal_satisfied_returns_false_for_incomplete_nested_plan() {
            let data = vec![datum_json!({"vec": [
                {"vec":[ {"str": "current-state"}, {"float": 0}]},
                {"vec":[ {"str": "time"}, {"float": 0}]},
            ]})];
            let data_refs: Vec<&Datum> = data.iter().collect();
            let rules = setup_rules();
            let rule_refs: Vec<&Rule<Datum, Datum>> = rules.iter().collect();

            let goal = goal_json!(Goal<Datum, Datum, Rule<Datum, Datum>>, {
              "pattern": {
                "vec": [
                  {"vec": [{"str": "current-state"}, {"float": 4}]},
                  {"vec": [{"str": "time"}, {"var": "?t2"}]}
                ]
              },
              "constraints": [
                {"numerical": {"set": {"variable": "?diff1", "constant": 1.0}}},
                {"numerical": {"set": {"variable": "?diff2", "constant": 2.0}}},
                {"numerical": {"sum": {"first": "?s1", "second": "?diff2", "third": "?s2"}}},
                {"numerical": {"sum": {"first": "?t1", "second": "?diff1", "third": "?t2"}}},
              ],
              "unification_index": {"actor": 0},
              "subgoals": [
                {
                    "pattern": {
                      "vec": [
                          {"vec": [{"str": "current-state"}, {"float": 2}]},
                          {"vec": [{"str": "time"}, {"var": "?t1"}]}
                      ],
                    },
                    "parental_constraints": [
                        {"numerical": {"set": {"variable": "?diff1", "constant": 1.0}}},
                        {"numerical": {"set": {"variable": "?diff2", "constant": 2.0}}},
                        {"numerical": {"sum": {"first": "?s0", "second": "?diff2", "third": "?s1"}}},
                        {"numerical": {"sum": {"first": "?t0", "second": "?diff1", "third": "?t1"}}},
                     ],
                    "constraints": [
                        {"numerical": {"set": {"variable": "?diff1", "constant": 1.0}}},
                        {"numerical": {"set": {"variable": "?diff2", "constant": 2.0}}},
                        {"numerical": {"sum": {"first": "?s1", "second": "?diff2", "third": "?s2"}}},
                        {"numerical": {"sum": {"first": "?t1", "second": "?diff1", "third": "?t2"}}},
                     ],
                },
                {
                    "pattern": {
                      "vec": [
                        {"vec": [{ "str": "action" }, { "int": 2 }]},
                        {"vec": [{ "str": "time" }, { "var": "?t2::1" }]},
                      ],
                    },
                    "parental_constraints": [
                        {"numerical": {"set": {"variable": "?diff1", "constant": 1.0}}},
                        {"numerical": {"set": {"variable": "?diff2", "constant": 2.0}}},
                        {"numerical": {"sum": {"first": "?s0", "second": "?diff2", "third": "?s1"}}},
                        {"numerical": {"sum": {"first": "?t0", "second": "?diff1", "third": "?t1"}}},
                     ],
                     "constraints": [
                        {"numerical": {"set": {"variable": "?diff1", "constant": 1.0}}},
                        {"numerical": {"set": {"variable": "?diff2", "constant": 2.0}}},
                        {"numerical": {"sum": {"first": "?s1", "second": "?diff2", "third": "?s2"}}},
                        {"numerical": {"sum": {"first": "?t1", "second": "?diff1", "third": "?t2"}}},
                     ]
                }
              ]
            });
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
            let data = vec![datum_json!({"vec": [
                {"vec":[ {"str": "current-state"}, {"float": 0}]},
                {"vec":[ {"str": "time"}, {"float": 0}]},
            ]})];
            let rules = setup_rules();
            let goal_pattern = datum_json!({"vec": [
                {"vec":[ {"str": "current-state"}, {"float": 4}]},
                {"vec":[ {"str": "time"}, {"var": "?t"}]},
            ]});
            let initial_goal = Goal::new(goal_pattern,
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

            let expected_final_goal = goal_json!(Goal<Datum, Datum, Rule<Datum, Datum>>, {
              "pattern": {
                "vec": [
                  {"vec": [{"str": "current-state"}, {"float": 4}]},
                  {"vec": [{"str": "time"}, {"float": 2}]}
                ]
              },
              "unification_index": {"actor": 0},
              "subgoals": [
                {
                  "pattern": {
                    "vec": [
                      {"vec": [{"str": "current-state"}, {"float": 2}]},
                      {"vec": [{"str": "time"}, {"float": 1}]}
                    ]
                  },
                  "unification_index": {"actor": 0},
                  "subgoals": [
                    {
                        "pattern": {
                          "vec": [
                              {"vec": [{"str": "current-state"}, {"float": 0}]},
                              {"vec": [{"str": "time"}, {"float": 0}]}
                          ],
                        },
                        "unification_index": {"datum": 0},
                    },
                    {
                        "pattern": {
                          "vec": [
                            {"vec": [{ "str": "action" }, { "int": 2 }]},
                            {"vec": [{ "str": "time" }, { "float": 0 }]},
                          ],
                        },
                        "unification_index": {"actor": 1},
                    }
                  ]
                },
                {
                    "pattern": {
                      "vec": [
                        {"vec": [{ "str": "action" }, { "int": 2 }]},
                        {"vec": [{ "str": "time" }, { "float": 1 }]},
                      ],
                    },
                    "unification_index": {"actor": 1},
                }
              ]
            });
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
            let rules: Vec<Rule<Datum, Datum>> = from_json!(Vec<Rule<Datum, Datum>>, [
              {"lhs": [{"str": "a"}], "rhs": {"str": "det"}},
              {"lhs": [{"str": "the"}], "rhs": {"str": "det"}},
              {"lhs": [{"str": "chased"}], "rhs": {"str": "verb"}},
              {"lhs": [{"str": "chased"}], "rhs": {"str": "verb"}},
              {"lhs": [{"str": "dog"}], "rhs": {"str": "noun"}},
              {"lhs": [{"str": "cat"}], "rhs": {"str": "noun"}},
              {"lhs": [{"str": "det"}, {"str": "noun"}], "rhs": {"str": "np"}},
              {"lhs": [{"str": "verb"}, {"str": "np"}], "rhs": {"str": "vp"}},
              {"lhs": [{"str": "np"}, {"str": "vp"}], "rhs": {"str": "sen"}}
            ]);
            let data: Vec<Datum> = from_json!(Vec<Datum>, [
              {"str": "a"},
              {"str": "the"},
              {"str": "dog"},
              {"str": "cat"},
              {"str": "chased"}
            ]);

            let initial_goal = Goal::with_pattern(datum_json!({"str": "sen"}));
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

        #[test]
        fn test_plan_with_goal_constraint() {
            /*
             * Verify that we backtrack if a constraint is violated.
             * Specifically, we test this by ensuring that the immediate solution
             * (just add 2 once) violates a constraint, forcing the algorithm
             * to add 1 twice.
             */
            let rules = setup_rules();

            let data = vec![datum_json!({"vec": [
                {"vec":[{"str": "current-state"}, {"float": 0}]},
                {"vec":[{"str": "time"}, {"float": 1}]},
            ]})];
            let initial_goal = from_json!(Goal<Datum, Datum, Rule<Datum, Datum>>, {
                "pattern": {"vec": [
                    {"vec":[ {"str": "current-state"}, {"float": 2}]},
                    {"vec":[ {"str": "time"}, {"var": "?t2"}]},
                ]},
                "unification_index": {"datum": 0},
                "constraints": [
                  {
                    "numerical": {
                      "set": {
                        "variable": "?min_time",
                        "constant": 2,
                      },
                    },
                  },
                  {
                    "numerical": {
                      ">": {
                        "left": "?t2",
                        "right": "?min_time",
                      }
                    }
                  }
                ]
            });
            let expected_final_goal = from_json!(Goal<Datum, Datum, Rule<Datum, Datum>>, {
                "pattern": {"vec": [
                    {"vec":[ {"str": "current-state"}, {"float": 2}]},
                    {"vec":[ {"str": "time"}, {"float": 3}]},
                ]},
                "unification_index": {"actor": 2},
                "subgoals": [
                  {
                    "pattern": {"vec": [
                        {"vec":[ {"str": "current-state"}, {"float": 1}]},
                        {"vec":[ {"str": "time"}, {"float": 2}]},
                    ]},
                    "unification_index": {"actor": 2},
                    "subgoals": [
                      {
                        "pattern": {"vec": [
                            {"vec":[ {"str": "current-state"}, {"float": 0}]},
                            {"vec":[ {"str": "time"}, {"float": 1}]},
                        ]},
                        "unification_index": {"datum": 0},
                      },
                      {
                        "pattern": {"vec": [
                            {"vec":[ {"str": "action"}, {"int": 1}]},
                            {"vec":[ {"str": "time"}, {"float": 1}]},
                        ]},
                        "unification_index": {"actor": 3},
                      }
                    ]
                  },
                  {
                    "pattern": {"vec": [
                        {"vec":[ {"str": "action"}, {"int": 1}]},
                        {"vec":[ {"str": "time"}, {"float": 2}]},
                    ]},
                    "unification_index": {"actor": 3},
                  }
                ]
            });
            let mut planner = Planner::new(&initial_goal,
                                           &Bindings::new(),
                                           &PlanningConfig {
                                               max_depth: 5,
                                               reuse_data: true,
                                           },
                                           data.iter().collect(),
                                           rules.iter().collect(),
                                           50);

            let result = planner.next();
            assert_eq!(result.is_some(), true);

            let (final_goal, bindings) = result.unwrap();

            assert_eq!(bindings.get_binding(&"?t2".to_string()),
                       Some(Datum::from_float(3.0)));

            assert_goal_spines_match(&final_goal.apply_bindings(&bindings).unwrap(),
                                     &expected_final_goal);
        }
    }
}
