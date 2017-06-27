#[cfg(test)]
mod bindings_tests {
    use std::collections::HashMap;
    use core::Bindings;
    use datum::Datum;

    #[test]
    fn test_setting_variable_as_value_adds_to_equivalence() {
        let expected_bindings = Bindings {
            data: HashMap::new(),
            equivalences: vec![("?y".to_string(), vec!["?y".to_string(), "?x".to_string()].into_iter().collect()),
                               ("?x".to_string(), vec!["?y".to_string(), "?x".to_string()].into_iter().collect())]
                .into_iter()
                .collect(),
        };
        let bindings = Bindings::new().set_binding(&"?x".to_string(), Datum::from_variable("?y".to_string()));
        assert_eq!(bindings, expected_bindings);
    }

    #[test]
    fn test_setting_variable_sets_value_for_all_equivalents() {
        let expected_bindings = Bindings {
            data: vec![("?x".to_string(), Datum::from_float(5.0)), ("?y".to_string(), Datum::from_float(5.0))]
                .into_iter()
                .collect(),
            equivalences: vec![("?y".to_string(), vec!["?y".to_string(), "?x".to_string()].into_iter().collect()),
                               ("?x".to_string(), vec!["?y".to_string(), "?x".to_string()].into_iter().collect())]
                .into_iter()
                .collect(),
        };
        let bindings = Bindings::new()
            .set_binding(&"?x".to_string(), Datum::from_variable("?y".to_string()))
            .set_binding(&"?x".to_string(), Datum::from_float(5.0));
        assert_eq!(bindings, expected_bindings);
    }

    #[test]
    fn test_merge() {
        let bindings = Bindings::new()
            .set_binding(&"?x".to_string(), Datum::from_variable("?y".to_string()))
            .set_binding(&"?x".to_string(), Datum::from_float(5.0));
        let bindings_2 = Bindings::new()
            .set_binding(&"?a".to_string(), Datum::from_variable("?b".to_string()))
            .set_binding(&"?a".to_string(), Datum::from_float(10.0));

        let expected_bindings = Bindings::new()
            .set_binding(&"?x".to_string(), Datum::from_variable("?y".to_string()))
            .set_binding(&"?x".to_string(), Datum::from_float(5.0))
            .set_binding(&"?a".to_string(), Datum::from_variable("?b".to_string()))
            .set_binding(&"?a".to_string(), Datum::from_float(10.0));
        assert_eq!(bindings.merge(&bindings_2), expected_bindings);
    }
}
