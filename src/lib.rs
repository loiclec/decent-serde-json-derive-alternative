
extern crate proc_macro;
use proc_macro2::{self, Literal};

#[macro_use]
extern crate decent_synquote_alternative;

use decent_synquote_alternative::token_builder::TokenBuilderExtend;
use decent_synquote_alternative::parser as parser;
use decent_synquote_alternative::token_builder as token_builder;

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
            "compile_error ! ("
            Literal::string("decent-serde-json-derive-alternative could not parse the structure")
            ") ;"
        )
    }
    let ts = tb.end();
    eprintln!("{}", ts);
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
            "compile_error ! ("
            Literal::string("decent-serde-json-derive-alternative could not parse the structure")
            ") ;"
        )
    }
    let ts = tb.end();
    ts.into()
}

fn derive_to_json_struct(parsed_struct: parser::Struct, tb: &mut TokenBuilder) {
    let to_json = ts!(":: decent_serde_json_alternative :: ToJson");

    let mut where_clause_in_impl_from = parsed_struct.where_clause.clone().unwrap_or_else(|| WhereClause::default());
    
    for ty in &parsed_struct.generics.type_params {
        where_clause_in_impl_from.items.push(WhereClauseItem {
            for_lifetimes: None,
            lhs: ty.type_ident.clone(),
            rhs: to_json.clone(),
        })
    }
    /*
        let mut object = :: json :: object :: Object :: new () ;
        object . insert ( "field1" , self . field1 . to_json ( ) ) ;
        object . insert ( "field2" , self . field2 . to_json ( ) ) ;
        :: json :: JsonValue :: Object ( object )
    */
    extend_ts!(tb,
    "impl" parsed_struct.generics.removing_eq_type() to_json "for" parsed_struct.ident parsed_struct.generics.removing_bounds_and_eq_type() where_clause_in_impl_from 
    "{
        fn to_json ( & self )  -> :: json :: JsonValue {
            let mut object = :: json :: object :: Object :: new ( ) ;
            "
            join_ts!(parsed_struct.struct_fields, f, 
                "object . insert ( " Literal::string(&f.access().to_string()) ", self . " f.access() " . to_json ( ) ) ;"
            )
            "
            :: json :: JsonValue :: Object ( object )
        }
    }"
    )
}

fn derive_to_json_enum(parsed_enum: parser::Enum, tb: &mut TokenBuilder) {
    let to_json = ts!(":: decent_serde_json_alternative :: ToJson");

    let mut where_clause_in_impl_from = parsed_enum.where_clause.clone().unwrap_or_else(|| WhereClause::default());
    
    for ty in &parsed_enum.generics.type_params {
        where_clause_in_impl_from.items.push(WhereClauseItem {
            for_lifetimes: None,
            lhs: ty.type_ident.clone(),
            rhs: to_json.clone(),
        })
    }
    /*
        let mut object = :: json :: object :: Object :: new () ;
        object . insert ( "field1" , self . field1 . to_json ( ) ) ;
        object . insert ( "field2" , self . field2 . to_json ( ) ) ;
        :: json :: JsonValue :: Object ( object )
    */
    extend_ts!(tb,
    "# [ allow ( non_shorthand_field_patterns ) ]"
    "impl" parsed_enum.generics.removing_eq_type() to_json "for" parsed_enum.ident parsed_enum.generics.removing_bounds_and_eq_type() where_clause_in_impl_from 
    "{
        fn to_json ( & self )  -> :: json :: JsonValue {
            let mut object = :: json :: object :: Object :: new ( ) ;
            match self {"
            join_ts!(parsed_enum.items, item,
                item.pattern_match(&parsed_enum.ident, None) "=> {"
                    "object . insert ( " Literal::string("kind") ", :: json :: JsonValue :: String ("  Literal::string(&item.ident.to_string()) " . to_string ( ) ) ) ;"
                    if let Some(fields) = item.get_struct_data().map(|d| d.1) {
                        ts!("
                            let mut payload = :: json :: object :: Object :: new ( ) ;
                            "
                            join_ts!(fields, f, 
                                "payload . insert ( " Literal::string(&f.access().to_string()) "," f.safe_ident() " . to_json ( ) ) ;"
                            )
                            "object . insert ( " Literal::string("payload") " , :: json :: JsonValue :: Object ( payload ) ) ; "
                        )
                    } else {
                        ts!()
                    }
                    "
                    :: json :: JsonValue :: Object ( object )
                }"
            )
            "}
        }
    }")
}

fn derive_from_json_struct(parsed_struct: parser::Struct, tb: &mut TokenBuilder) {
    let from_json = ts!(":: decent_serde_json_alternative :: FromJson");

    let mut where_clause_in_impl_from = parsed_struct.where_clause.clone().unwrap_or_else(|| WhereClause::default());
    
    for ty in &parsed_struct.generics.type_params {
        where_clause_in_impl_from.items.push(WhereClauseItem {
            for_lifetimes: None,
            lhs: ty.type_ident.clone(),
            rhs: from_json.clone(),
        })
    }

    /*
    let mut field1 = None;
    let mut field2 = None;
    for (key, value) in o.iter() {
        match key {
            "field1" => {
                field1 = Some(<_>::from_json(value)?);
            }
            "field2" => {
                field2 = Some(<_>::from_json(value)?);
            }
            _ => { }
        }
    }
    let field1 = field1?;
    let field2 = field2?;

    Some ( Self {
        field1: field1,
        field2: field2,
    } )
    */
    extend_ts!(tb,
        "impl" parsed_struct.generics.removing_eq_type() from_json "for" parsed_struct.ident parsed_struct.generics.removing_bounds_and_eq_type() where_clause_in_impl_from 
        "{
            fn from_json ( fromjson : & json :: JsonValue ) -> Option < Self > {
                if let :: json :: JsonValue :: Object ( o ) = fromjson {"
                    join_ts!(&parsed_struct.struct_fields, f, 
                        "let mut" f.safe_ident() "= None ;"
                    )
                    "for ( key , value ) in o . iter ( ) {
                        match key {
                        "
                        join_ts!(&parsed_struct.struct_fields, f, {
                            let literal_key = Literal::string(&f.access().to_string());
                            ts!(
                            literal_key " => {"
                                f.safe_ident() " = Some ( < _ > :: from_json ( value ) ? ) ;" 
                            "
                            }"
                            )
                        })
                            "_ => { }"
                    "   }
                    }
                    "
                    join_ts!(&parsed_struct.struct_fields, f, 
                        "let" f.safe_ident() "=" f.safe_ident() "? ;"
                    )

                    "Some ( Self {"
                        join_ts!(&parsed_struct.struct_fields, f, 
                            f.access() ":" f.safe_ident()
                        , separator: ",")
                    "} )"
                "} else {
                    None
                }
                "
            "}
        }"
    )
}

/*
{
    kind: "VariantA",
    payload: {
        ...
    }
}
*/

fn derive_from_json_enum(parsed_enum: parser::Enum, tb: &mut TokenBuilder) {
    let from_json = ts!(":: decent_serde_json_alternative :: FromJson");

    let mut where_clause_in_impl_from = parsed_enum.where_clause.clone().unwrap_or_else(|| WhereClause::default());

    for ty in &parsed_enum.generics.type_params {
        where_clause_in_impl_from.items.push(WhereClauseItem {
            for_lifetimes: None,
            lhs: ty.type_ident.clone(),
            rhs: from_json.clone(),
        })
    }

    /*
    if let ::json::JsonValue(Object(o)) = fromjson {
        if ! o . has_key ( "kind" ) { return None }
        let kind = match o["kind"] {
            JsonValue::String(x) => {
                x
            }
            JsonValue::Short(x) =>{
                x.to_string()
            }
        };
        match kind {
            "Variant1" => {
                if !o.has_key("payload") {
                    return None
                }
                if let JsonValue::Object(payload) = o["payload"] {
                    let mut field1 = None;
                    let mut field2 = None;
                    for (key, value) in payload.iter() {
                        match key {
                            "field1" => {
                                field1 = Some(<_>::from_json(value)?);
                            }
                            "field2" => {
                                field2 = Some(<_>::from_json(value)?);
                            }
                            _ => { }
                        }
                    }
                    let field1 = field1?;
                    let field2 = field2?;

                    Some ( Self::Variant1 {
                        field1: field1,
                        field2: field2,
                    } )
                } else {
                    return None
                }
            }
            "Variant2" => {
                Some(Self::Variant2)
            }
            _ => {
                return None
            }
        }
    } else {
        return None
    }
    */

    let kind_literal = Literal::string("kind");
    let payload_literal = Literal::string("payload");

    extend_ts!(tb,
        "impl" parsed_enum.generics.removing_eq_type() from_json "for" parsed_enum.ident parsed_enum.generics.removing_bounds_and_eq_type() where_clause_in_impl_from 
        "{
            fn from_json ( fromjson : & json :: JsonValue ) -> Option < Self > {
                if let :: json :: JsonValue :: Object ( o ) = fromjson {
                    if ! fromjson . has_key ( " kind_literal " ) { return None }
                    let kind = match & o [ " kind_literal " ] {
                        :: json :: JsonValue :: String ( x ) => {
                            x . clone ( )
                        }
                        :: json :: JsonValue :: Short ( x ) => {
                            x . to_string ( )
                        }
                        _ => {
                            return None
                        }
                    } ;
                    match kind . as_str ( ) {"
                        join_ts!(&parsed_enum.items, item, {
                            let variant_literal = Literal::string(&item.ident.to_string());
                            ts!(
                            variant_literal "=> {"
                                if let Some(fields) = item.get_struct_data().map(|d| d.1) {
                                    ts!(
                                    "if ! fromjson . has_key ( " payload_literal ") {
                                        return None
                                    }
                                    if let :: json :: JsonValue :: Object ( o ) = & o [ " payload_literal " ] {"
                                        // TODO: deduplicate this, it is the same logic as for structs
                                        join_ts!(fields, f, 
                                            "let mut" f.safe_ident() "= None ;"
                                        )
                                        "for ( key , value ) in o . iter ( ) {
                                            match key {
                                            "
                                            join_ts!(fields, f, {
                                                let literal_key = Literal::string(&f.access().to_string());
                                                ts!(
                                                literal_key " => {"
                                                    f.safe_ident() " = Some ( < _ > :: from_json ( value ) ? ) ;" 
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
                                        "Some ( Self :: " item.ident " {"
                                            join_ts!(fields, f, 
                                                f.access() ":" f.safe_ident()
                                            , separator: ",")
                                        "} )"
                                    "} else {
                                        return None
                                    }"
                                    )
                                } else {
                                    ts!(
                                    "Some ( Self :: " item.ident " )"
                                    )
                                }
                            "}"
                            )
                        })
                        
                        "_ => { return None }"
                        // "Variant1" => {
                        //     if !o.has_key("payload") {
                        //         return None
                        //     }
                        //     if let JsonValue::Object(payload) = o["payload"] {
                        //         let mut field1 = None;
                        //         let mut field2 = None;
                        //         for (key, value) in payload.iter() {
                        //             match key {
                        //                 "field1" => {
                        //                     field1 = Some(<_>::from_json(value)?);
                        //                 }
                        //                 "field2" => {
                        //                     field2 = Some(<_>::from_json(value)?);
                        //                 }
                        //                 _ => { }
                        //             }
                        //         }
                        //         let field1 = field1?;
                        //         let field2 = field2?;
            
                        //         Some ( Self::Variant1 {
                        //             field1: field1,
                        //             field2: field2,
                        //         } )
                        //     } else {
                        //         return None
                        //     }
                        // }
                        // "Variant2" => {
                        //     Some(Self::Variant2)
                        // }
                        // _ => {
                        //     return None
                        // }
                    "}
                } else {
                    return None
                }
            }
        }"
    )
}