extern crate proc_macro;
use proc_macro2::{self, Ident, Literal, Span};

#[macro_use]
extern crate decent_synquote_alternative;

use decent_synquote_alternative::parser;
use decent_synquote_alternative::token_builder;
use decent_synquote_alternative::token_builder::TokenBuilderExtend;

use parser::{TokenParser, WhereClause, WhereClauseItem};
use token_builder::TokenBuilder;

#[proc_macro_derive(FromJson)]
pub fn derive_from_json(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = proc_macro2::TokenStream::from(input);

    let mut parser = TokenParser::new(input);

    let mut tb = TokenBuilder::new();

    if let Some(s) = parser.eat_struct() {
        derive_from_json_struct(s, &mut tb)
    } else if let Some(e) = parser.eat_enumeration() {
        derive_from_json_enum(e, &mut tb)
    } else {
        extend_ts!(&mut tb,
                "compile_error !("
                Literal::string("decent-serde-json-derive-alternative could not parse the structure")
                ") ;"
        )
    }
    let ts = tb.end();
    ts.into()
}

#[proc_macro_derive(ToJson)]
pub fn derive_to_json(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = proc_macro2::TokenStream::from(input);

    let mut parser = TokenParser::new(input);

    let mut tb = TokenBuilder::new();

    if let Some(s) = parser.eat_struct() {
        derive_to_json_struct(s, &mut tb)
    } else if let Some(e) = parser.eat_enumeration() {
        derive_to_json_enum(e, &mut tb)
    } else {
        extend_ts!(&mut tb,
                "compile_error !("
                Literal::string("decent-serde-json-derive-alternative could not parse the structure")
                ") ;"
        )
    }
    let ts = tb.end();
    ts.into()
}

fn derive_to_json_struct(parsed_struct: parser::Struct, tb: &mut TokenBuilder) {
    let to_json = ts!("::decent_serde_json_alternative::ToJson");
    let json_value = ts!("::json::JsonValue ");
    let json_object = ts!("::json::object::Object");

    let mut where_clause_in_impl_from = parsed_struct
        .where_clause
        .clone()
        .unwrap_or_else(|| WhereClause::default());

    for ty in &parsed_struct.generics.type_params {
        where_clause_in_impl_from.items.push(WhereClauseItem {
            for_lifetimes: None,
            lhs: ty.type_ident.clone(),
            rhs: to_json.clone(),
        })
    }

    extend_ts!(tb,
        "impl" parsed_struct.generics.removing_eq_type() to_json "for" parsed_struct.ident parsed_struct.generics.removing_bounds_and_eq_type() where_clause_in_impl_from
        "{
        fn to_json(&self)  ->" json_value " {
            let mut object = " json_object "::new() ;
            "
                join_ts!(parsed_struct.struct_fields, f,
                    "object.insert(" Literal::string(&f.access().to_string()) ", self." f.access() ".to_json()) ;"
            )
                "
            " json_value "::Object(object)
        }
    }"
    )
}

fn derive_to_json_enum(parsed_enum: parser::Enum, tb: &mut TokenBuilder) {
    let to_json = ts!("::decent_serde_json_alternative::ToJson");
    let json_value = ts!("::json::JsonValue ");
    let json_object = ts!("::json::object::Object");

    let mut where_clause_in_impl_from = parsed_enum
        .where_clause
        .clone()
        .unwrap_or_else(|| WhereClause::default());

    for ty in &parsed_enum.generics.type_params {
        where_clause_in_impl_from.items.push(WhereClauseItem {
            for_lifetimes: None,
            lhs: ty.type_ident.clone(),
            rhs: to_json.clone(),
        })
    }

    extend_ts!(tb,
    "# [ allow(non_shorthand_field_patterns) ]"
    "impl" parsed_enum.generics.removing_eq_type() to_json "for" parsed_enum.ident parsed_enum.generics.removing_bounds_and_eq_type() where_clause_in_impl_from 
    "{
        fn to_json(&self)  -> " json_value " {
            let mut object = " json_object "::new() ;
            match self {"
            join_ts!(parsed_enum.items, item,
                item.pattern_match(&parsed_enum.ident, None) "=> {"
                    "object.insert(" Literal::string("kind") ", " json_value "::String ("  Literal::string(&item.ident.to_string()) ".to_string())) ;"
                    if let Some(fields) = item.get_struct_data().map(|d| d.1) {
                        ts!("
                            let mut payload = " json_object "::new() ;
                            "
                            join_ts!(fields, f, 
                                "payload.insert(" Literal::string(&f.access().to_string()) "," f.safe_ident() ".to_json()) ;"
                        )
                            "object.insert(" Literal::string("payload") " , " json_value "::Object(payload)) ; "
                    )
                    } else {
                        ts!()
                    }
                    "
                    " json_value "::Object(object)
                }"
        )
            "}
        }
    }")
}

fn derive_from_json_struct(parsed_struct: parser::Struct, tb: &mut TokenBuilder) {
    let from_json = ts!("::decent_serde_json_alternative::FromJson");
    let json_value = ts!("::json::JsonValue ");
    let option = ts!("::std::option::Option");
    let some = ts!(option "::Some");
    let none = ts!(option "::None");

    let mut where_clause_in_impl_from = parsed_struct
        .where_clause
        .clone()
        .unwrap_or_else(|| WhereClause::default());

    for ty in &parsed_struct.generics.type_params {
        where_clause_in_impl_from.items.push(WhereClauseItem {
            for_lifetimes: None,
            lhs: ty.type_ident.clone(),
            rhs: from_json.clone(),
        })
    }

    extend_ts!(tb,
    "impl" parsed_struct.generics.removing_eq_type() from_json "for" parsed_struct.ident parsed_struct.generics.removing_bounds_and_eq_type() where_clause_in_impl_from
    "{
        fn from_json(fromjson : & " json_value ") -> " option " <Self> {
            if let " json_value "::Object(o) = fromjson {"
                join_ts!(&parsed_struct.struct_fields, f,
                    "let mut" f.safe_ident() "= " none " ;"
                )
                "for(key , value) in o.iter() {
                    match key {"
                        join_ts!(&parsed_struct.struct_fields, f,
                            Literal::string(&f.access().to_string()) " => {"
                                f.safe_ident() " = " some "(<_>::from_json(value) ?) ;"
                            "}"
                        )
                        "_ => { }
                    }
                }"
                join_ts!(&parsed_struct.struct_fields, f,
                    "let" f.safe_ident() "=" f.safe_ident() "? ;"
                )
                some "(Self {"
                    join_ts!(&parsed_struct.struct_fields, f,
                        f.access() ":" f.safe_ident()
                    , separator: ",")
                "})"
            "} else {"
                none
            "}
        }
    }"
    )
}

fn derive_from_json_enum(parsed_enum: parser::Enum, tb: &mut TokenBuilder) {
    let from_json = ts!("::decent_serde_json_alternative::FromJson");
    let json_value = ts!("::json::JsonValue ");
    let option = ts!("::std::option::Option");
    let some = ts!(option "::Some");
    let none = ts!(option "::None");

    let mut where_clause_in_impl_from = parsed_enum
        .where_clause
        .clone()
        .unwrap_or_else(|| WhereClause::default());

    for ty in &parsed_enum.generics.type_params {
        where_clause_in_impl_from.items.push(WhereClauseItem {
            for_lifetimes: None,
            lhs: ty.type_ident.clone(),
            rhs: from_json.clone(),
        })
    }

    let kind_literal = Literal::string("kind");
    let payload_literal = Literal::string("payload");

    extend_ts!(tb,
            "impl" parsed_enum.generics.removing_eq_type() from_json "for" parsed_enum.ident parsed_enum.generics.removing_bounds_and_eq_type() where_clause_in_impl_from
            "{
            fn from_json(fromjson : & " json_value ") -> " option " <Self> {
                if let " json_value "::Object(o) = fromjson {
                    if !fromjson.has_key(" kind_literal ") { return " none " }
                    let kind = match & o [ " kind_literal " ] {
                        " json_value "::String(x) => {
                            x.clone()
                        }
                        " json_value "::Short(x) => {
                            x.to_string()
                        }
                        _ => {
                            return " none "
                        }
                    } ;
                    match kind.as_str() {"
                            join_ts!(&parsed_enum.items, item, {
                                let variant_literal = Literal::string(&item.ident.to_string());
                                ts!(
                                variant_literal "=> {"
                                    if let Some(fields) = item.get_struct_data().map(|d| d.1) {
                                        ts!(
                                        "if !fromjson.has_key(" payload_literal ") {
                                        return " none "
                                    }
                                    if let " json_value "::Object(o) = & o [ " payload_literal " ] {"
                                            // TODO: deduplicate this, it is the same logic as for structs
                                            join_ts!(fields, f,
                                                "let mut" f.safe_ident() "= " none " ;"
                                        )
                                            "for(key , value) in o.iter() {
                                            match key {
                                            "
                                                join_ts!(fields, f, {
                                                    let literal_key = Literal::string(&f.access().to_string());
                                                    ts!(
                                                    literal_key " => {"
                                                        f.safe_ident() " = " some "(<_>::from_json(value) ?) ;"
                                                    "
                                                    }"
                                                )
                                                })
                                                    "_ => { }"
                                            "   }
                                        }
                                        "
                                            join_ts!(fields, f,
                                                "let" f.safe_ident() "=" f.safe_ident() "? ;"
                                        )
                                            some "(Self::" item.ident " {"
                                                join_ts!(fields, f,
                                                    f.access() ":" f.safe_ident()
                                                , separator: ",")
                                            "})"
                                        "} else {
                                        return " none "
                                    }"
                                    )
                                    } else {
                                        ts!(some "( Self::" item.ident ")")
                                    }
                                "}"
                            )
                            })

                            "_ => { return " none " }"
                        "}
                } else {
                    return " none "
                }
            }
        }"
    )
}

#[proc_macro]
pub fn derive_from_json_to_json_for_tuple(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let mut tb = TokenBuilder::new();

    let mut parser = TokenParser::new(item.into());
    if let Some(l) = parser.eat_literal() {
        if let Ok(nbr_elements) = l.to_string().parse::<usize>() {
            impl_from_json_to_json_for_tuple(&mut tb, nbr_elements);
            return tb.end().into();
        }
    }
    panic!()
}

fn impl_from_json_to_json_for_tuple(tb: &mut TokenBuilder, nbr_elements: usize) {
    #[allow(non_snake_case)]
    let Ti = |i: usize| ident!("T" i);
    let ti = |i: usize| ident!("t" i);
    let type_params = join_ts!(0..nbr_elements, i, Ti(i), separator: ",");
    extend_ts!(tb,
        "impl<" type_params "> FromJson for (" type_params ") where"
        join_ts!(0..nbr_elements, i, Ti(i) ": FromJson", separator: ",")
        "{
            fn from_json(json: &JsonValue) -> Option<Self> {
                if let JsonValue::Object(o) = json {"
                join_ts!(0..nbr_elements, i,
                    "let mut" ti(i) "= None;"
                )
                    "for (key, value) in o.iter() {
                        match key {"
                        join_ts!(0..nbr_elements, i,
                            Literal::string(&format!("{}", i)) "=> {"
                                ti(i) "= Some(<_>::from_json(value)?);"
                            "}"
                        )"
                            _ => {}
                        }
                    }"
                    join_ts!(0..nbr_elements, i,
                        "let" ti(i) "=" ti(i) "?;"
                    )
                    "Some( ("
                        join_ts!(0..nbr_elements, i,
                            ti(i)
                        , separator: ",")
                    ") )
                } else {
                    None
                }
            }
        }
        impl<" type_params "> ToJson for (" type_params ") where"
            join_ts!(0..nbr_elements, i, Ti(i) ": ToJson", separator: ",")
        "{
            fn to_json(&self) -> JsonValue {
                let mut object = ::json::object::Object::new();"
                join_ts!(0..nbr_elements, i,
                    "object.insert(" Literal::string(&format!("{}", i)) ", self." i ".to_json());"
                )
                "JsonValue::Object(object)"
            "}
        }"
    );
}

#[cfg(test)]
mod test {
    use decent_synquote_alternative::token_builder::TokenBuilder;
    use proc_macro2::TokenStream;

    use crate::impl_from_json_to_json_for_tuple;

    #[test]
    fn test_impl_from_json_to_json_for_tuple() {
        let mut tb = TokenBuilder::new();
        impl_from_json_to_json_for_tuple(&mut tb, 2);
        let generated = tb.end().to_string();
        let expected = "

impl<T0, T1> FromJson for (T0, T1)
where
    T0: FromJson,
    T1: FromJson
{
    fn from_json(json: &JsonValue) -> Option<Self> {
        if let JsonValue::Object(o) = json {
            let mut t0 = None;
            let mut t1 = None;
            for (key, value) in o.iter() {
                match key {
                    \"0\" => {
                        t0 = Some(<_>::from_json(value)?);
                    }
                    \"1\" => {
                        t1 = Some(<_>::from_json(value)?);
                    }
                    _ => {}
                }
            }
            let t0 = t0?;
            let t1 = t1?;
            Some((t0, t1))
        } else {
            None
        }
    }
}
        impl<T0, T1> ToJson for (T0, T1) where T0: ToJson, T1: ToJson {
            fn to_json(&self) -> JsonValue {
                let mut object = ::json::object::Object::new();
                object.insert(\"0\", self.0.to_json());
                object.insert(\"1\", self.1.to_json());
                JsonValue::Object(object)
            }
        }
        "
        .parse::<TokenStream>()
        .unwrap()
        .to_string();
        assert_eq!(generated, expected, "\n\n{}\n\n{}", generated, expected);
    }
}
