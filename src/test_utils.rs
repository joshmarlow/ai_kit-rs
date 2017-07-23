macro_rules! from_json {
    ($type: ty, $json: tt) => ({
        use serde_json;
        let x: $type = serde_json::from_value(json!($json)).expect("Expected json decoding");
        x
    })
}
