#[cfg(test)]
mod bindings_tests {
    use std::collections::HashMap;
    use core::{Bindings, BindingsValue};

    impl BindingsValue for String {
        fn variable(s: &String) -> Option<Self> {
            Some(s.clone())
        }
        fn to_variable(&self) -> Option<String> {
            if self.starts_with("?") {
                Some(self.clone())
            } else {
                None
            }
        }
    }

    #[test]
    fn test_setting_variable_as_value_adds_to_equivalence() {
        let expected_bindings: Bindings<String> = Bindings {
            data: HashMap::new(),
            equivalences: vec![("?y".to_string(), vec!["?y".to_string(), "?x".to_string()].into_iter().collect()),
                               ("?x".to_string(), vec!["?y".to_string(), "?x".to_string()].into_iter().collect())]
                .into_iter()
                .collect(),
        };
        let bindings: Bindings<String> = Bindings::new().set_binding(&"?x".to_string(), "?y".to_string());
        assert_eq!(bindings.equivalences, expected_bindings.equivalences);
    }

    #[test]
    fn test_setting_variable_sets_value_for_all_equivalents() {
        let expected_bindings = Bindings {
            data: vec![("?x".to_string(), "5.0".to_string()), ("?y".to_string(), "5.0".to_string())]
                .into_iter()
                .collect(),
            equivalences: vec![("?y".to_string(), vec!["?y".to_string(), "?x".to_string()].into_iter().collect()),
                               ("?x".to_string(), vec!["?y".to_string(), "?x".to_string()].into_iter().collect())]
                .into_iter()
                .collect(),
        };
        let bindings = Bindings::new()
            .set_binding(&"?x".to_string(), "?y".to_string())
            .set_binding(&"?x".to_string(), "5.0".to_string());
        assert_eq!(bindings, expected_bindings);
    }

    #[test]
    fn test_merge() {
        let bindings = Bindings::new()
            .set_binding(&"?x".to_string(), "?y".to_string())
            .set_binding(&"?x".to_string(), "5.0".to_string());
        let bindings_2 = Bindings::new()
            .set_binding(&"?a".to_string(), "?b".to_string())
            .set_binding(&"?a".to_string(), "10.0".to_string());

        let expected_bindings = Bindings::new()
            .set_binding(&"?x".to_string(), "?y".to_string())
            .set_binding(&"?x".to_string(), "5.0".to_string())
            .set_binding(&"?a".to_string(), "?b".to_string())
            .set_binding(&"?a".to_string(), "10.0".to_string());
        assert_eq!(bindings.merge(&bindings_2), expected_bindings);
    }
}
