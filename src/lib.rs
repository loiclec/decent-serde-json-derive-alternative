
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
    let json_value = ts!(":: json :: JsonValue ");
    let json_object = ts!(":: json :: object :: Object");

    let mut where_clause_in_impl_from = parsed_struct.where_clause.clone().unwrap_or_else(|| WhereClause::default());
    
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
        fn to_json ( & self )  ->" json_value " {
            let mut object = " json_object " :: new ( ) ;
            "
            join_ts!(parsed_struct.struct_fields, f, 
                "object . insert ( " Literal::string(&f.access().to_string()) ", self . " f.access() " . to_json ( ) ) ;"
            )
            "
            " json_value " :: Object ( object )
        }
    }"
    )
}

fn derive_to_json_enum(parsed_enum: parser::Enum, tb: &mut TokenBuilder) {
    let to_json = ts!(":: decent_serde_json_alternative :: ToJson");
    let json_value = ts!(":: json :: JsonValue ");
    let json_object = ts!(":: json :: object :: Object");

    let mut where_clause_in_impl_from = parsed_enum.where_clause.clone().unwrap_or_else(|| WhereClause::default());
    
    for ty in &parsed_enum.generics.type_params {
        where_clause_in_impl_from.items.push(WhereClauseItem {
            for_lifetimes: None,
            lhs: ty.type_ident.clone(),
            rhs: to_json.clone(),
        })
    }

    extend_ts!(tb,
    "# [ allow ( non_shorthand_field_patterns ) ]"
    "impl" parsed_enum.generics.removing_eq_type() to_json "for" parsed_enum.ident parsed_enum.generics.removing_bounds_and_eq_type() where_clause_in_impl_from 
    "{
        fn to_json ( & self )  -> " json_value " {
            let mut object = " json_object " :: new ( ) ;
            match self {"
            join_ts!(parsed_enum.items, item,
                item.pattern_match(&parsed_enum.ident, None) "=> {"
                    "object . insert ( " Literal::string("kind") ", " json_value " :: String ("  Literal::string(&item.ident.to_string()) " . to_string ( ) ) ) ;"
                    if let Some(fields) = item.get_struct_data().map(|d| d.1) {
                        ts!("
                            let mut payload = " json_object " :: new ( ) ;
                            "
                            join_ts!(fields, f, 
                                "payload . insert ( " Literal::string(&f.access().to_string()) "," f.safe_ident() " . to_json ( ) ) ;"
                            )
                            "object . insert ( " Literal::string("payload") " , " json_value " :: Object ( payload ) ) ; "
                        )
                    } else {
                        ts!()
                    }
                    "
                    " json_value " :: Object ( object )
                }"
            )
            "}
        }
    }")
}

fn derive_from_json_struct(parsed_struct: parser::Struct, tb: &mut TokenBuilder) {
    let from_json = ts!(":: decent_serde_json_alternative :: FromJson");
    let json_value = ts!(":: json :: JsonValue ");
    let option = ts!(":: std :: option :: Option");
    let some = ts!(option ":: Some");
    let none = ts!(option ":: None");

    let mut where_clause_in_impl_from = parsed_struct.where_clause.clone().unwrap_or_else(|| WhereClause::default());
    
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
            fn from_json ( fromjson : & " json_value " ) -> " option " < Self > {
                if let " json_value " :: Object ( o ) = fromjson {"
                    join_ts!(&parsed_struct.struct_fields, f, 
                        "let mut" f.safe_ident() "= " none " ;"
                    )
                    "for ( key , value ) in o . iter ( ) {
                        match key {
                        "
                        join_ts!(&parsed_struct.struct_fields, f, {
                            let literal_key = Literal::string(&f.access().to_string());
                            ts!(
                            literal_key " => {"
                                f.safe_ident() " = " some " ( < _ > :: from_json ( value ) ? ) ;" 
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
                    some " ( Self {"
                        join_ts!(&parsed_struct.struct_fields, f, 
                            f.access() ":" f.safe_ident()
                        , separator: ",")
                    "} )"
                "} else {
                    " none "
                }
                "
            "}
        }"
    )
}

fn derive_from_json_enum(parsed_enum: parser::Enum, tb: &mut TokenBuilder) {
    let from_json = ts!(":: decent_serde_json_alternative :: FromJson");
    let json_value = ts!(":: json :: JsonValue ");
    let option = ts!(":: std :: option :: Option");
    let some = ts!(option ":: Some");
    let none = ts!(option ":: None");

    let mut where_clause_in_impl_from = parsed_enum.where_clause.clone().unwrap_or_else(|| WhereClause::default());

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
            fn from_json ( fromjson : & " json_value " ) -> " option " < Self > {
                if let " json_value " :: Object ( o ) = fromjson {
                    if ! fromjson . has_key ( " kind_literal " ) { return " none " }
                    let kind = match & o [ " kind_literal " ] {
                        " json_value " :: String ( x ) => {
                            x . clone ( )
                        }
                        " json_value " :: Short ( x ) => {
                            x . to_string ( )
                        }
                        _ => {
                            return " none "
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
                                        return " none "
                                    }
                                    if let " json_value " :: Object ( o ) = & o [ " payload_literal " ] {"
                                        // TODO: deduplicate this, it is the same logic as for structs
                                        join_ts!(fields, f, 
                                            "let mut" f.safe_ident() "= " none " ;"
                                        )
                                        "for ( key , value ) in o . iter ( ) {
                                            match key {
                                            "
                                            join_ts!(fields, f, {
                                                let literal_key = Literal::string(&f.access().to_string());
                                                ts!(
                                                literal_key " => {"
                                                    f.safe_ident() " = " some " ( < _ > :: from_json ( value ) ? ) ;" 
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
                                        some " ( Self :: " item.ident " {"
                                            join_ts!(fields, f, 
                                                f.access() ":" f.safe_ident()
                                            , separator: ",")
                                        "} )"
                                    "} else {
                                        return " none "
                                    }"
                                    )
                                } else {
                                    ts!(
                                    some "( Self :: " item.ident " )"
                                    )
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