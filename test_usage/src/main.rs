
use json::stringify_pretty;

use decent_serde_json_alternative::{FromJson, ToJson};

#[derive(Debug, FromJson, ToJson)]
enum S<A> {
    X(A),
    Y {},
    Z {x: A, y: Vec<u8> }
}

fn main() {
    let json_input = r#"
    {
        "kind": "Y",
        "payload": {}
    }
    "#;
    let json_value = json::parse(json_input).unwrap();
    let value = S::<u8>::from_json(&json_value).unwrap();
    
    println!("{}", stringify_pretty(value.to_json(), 8));

    let value = S::<Option<bool>>::Z { x: Some(true), y: vec![0, 2] };
    println!("{}", stringify_pretty(value.to_json(), 8));

}
