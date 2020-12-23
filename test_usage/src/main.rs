
use json::stringify_pretty;

use decent_serde_json_derive_alternative::{FromJson, ToJson};
use decent_serde_json_alternative::{FromJson, ToJson};

#[derive(Debug, FromJson, ToJson)]
enum S<A> {
    X(u8),
    Y { x: A, y: String}
}

// impl :: decent_serde_json_alternative :: ToJson for S where
// {
//     fn to_json(& self) -> :: json :: JsonValue
//     {
//         let mut object = :: json :: object :: Object :: new() ; match self
//         {
//             S :: X(_0) =>
//             {
//                 object .
//                 insert("kind", :: json :: JsonValue ::
//                        String("X" . to_string())) ; let mut payload = :: json
//                 :: object :: Object :: new() ; payload .
//                 insert("0", self . 0 . to_json()) ; object .
//                 insert("payload", :: json :: JsonValue :: Object(payload)) ;
//                 :: json :: JsonValue :: Object(object)
//             } S :: Y =>
//             {
//                 object .
//                 insert("kind", :: json :: JsonValue ::
//                        String("Y" . to_string())) ; :: json :: JsonValue ::
//                 Object(object)
//             }
//         }
//     }
// }

// #[derive(Debug, FromJson)]
// enum A {
//     X(u8, Box<A>),
//     Y
// }

fn main() {
    let json_input = r#"
    {
        "kind": "Y",
        "payload": {
            "x": 8,
            "y": "A"
        }
    }
    "#;
    let json_value = json::parse(json_input).unwrap();
    let value = S::<u8>::from_json(&json_value).unwrap();
    
    println!("{}", stringify_pretty(value.to_json(), 8));
}
