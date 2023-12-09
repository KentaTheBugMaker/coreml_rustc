//! TopLevel compilation.
//! this module call functions.

use std::collections::{BTreeMap, HashMap};

use crate::{
    alpha_unique::alpha_conv_decls,
    closureconversion::closure_conversion_decls,
    knormalize::{knormalize_decls, knormalize_functions},
    parser_libchumsky,
    typeinf::type_inf,
};
use ariadne::{sources, Color, Label, Report, ReportKind};
use chumsky::{input::Input, Parser};
#[derive(Debug, PartialEq)]
pub enum StopAt {
    Syntax,
    TypeInf,
    AlphaConversion,
    ClosureConversion,
    KNormalize,
}
#[derive(Debug, Clone, Copy)]
pub struct Control {
    pub print_syntax: bool,
    pub print_typeinf: bool,
    pub print_alpha_conversion: bool,
    pub print_closure_conversion: bool,
    pub print_knormalize: bool,
}

pub fn compile(stop: StopAt, control: Control, src: String, filename: String) -> () {
    let mut gamma = HashMap::new();
    let tokenizer = parser_libchumsky::lexer();
    let (tokens, errs) = tokenizer.parse(src.as_str()).into_output_errors();
    let (ast, parse_errs) = if let Some(tokens) = &tokens {
        parser_libchumsky::decl_parser()
            .parse(tokens.as_slice().spanned((src.len()..src.len()).into()))
            .into_output_errors()
    } else {
        (None, vec![])
    };
    let (typed_decls, _typeinf_errors) = if stop == StopAt::Syntax {
        (vec![], vec![])
    } else {
        if let Some(mut decl) = ast {
            if control.print_syntax {
                println!("{:?}", decl);
            }
            let mut typed_decls = vec![];
            let errors = decl
                .drain(..)
                .map(|decl| match type_inf(&gamma, decl.into()) {
                    Ok((ty_env, typed_decl)) => {
                        gamma = ty_env;
                        typed_decls.push(typed_decl);
                        vec![]
                    }
                    Err(err) => {
                        vec![err]
                    }
                })
                .flatten()
                .collect();

            (typed_decls, errors)
        } else {
            (vec![], vec![])
        }
    };
    if control.print_typeinf {
        println!("{:?}", typed_decls);
    }
    let au_decls = if stop == StopAt::TypeInf {
        vec![]
    } else {
        alpha_conv_decls(typed_decls)
    };
    if control.print_alpha_conversion {
        println!("{:?}", au_decls);
    }
    let (nc_decls, functions) = if stop == StopAt::AlphaConversion {
        (vec![], BTreeMap::new())
    } else {
        closure_conversion_decls(au_decls)
    };

    if control.print_closure_conversion {
        for nc_decl in nc_decls.clone() {
            println!("{}", nc_decl);
        }
        for (fid, function) in functions.clone() {
            println!("fn {} {}", fid, function);
        }
    }
    let (kn_decls, functions) = if stop == StopAt::ClosureConversion {
        (vec![], BTreeMap::new())
    } else {
        (knormalize_decls(nc_decls), knormalize_functions(functions))
    };

    if control.print_knormalize {
        for nc_decl in kn_decls {
            println!("{}", nc_decl);
        }
        for (fid, function) in functions {
            println!("fn {} {}", fid, function);
        }
    }

    errs.into_iter()
        .map(|e| e.map_token(|c| c.to_string()))
        .chain(
            parse_errs
                .into_iter()
                .map(|e| e.map_token(|tok| tok.to_string())),
        )
        .for_each(|e| {
            Report::build(ReportKind::Error, filename.clone(), e.span().start)
                .with_message(e.to_string())
                .with_label(
                    Label::new((filename.clone(), e.span().into_range()))
                        .with_message(e.reason().to_string())
                        .with_color(Color::Red),
                )
                .with_labels(e.contexts().map(|(label, span)| {
                    Label::new((filename.clone(), span.into_range()))
                        .with_message(format!("while parsing this {}", label))
                        .with_color(Color::Yellow)
                }))
                .finish()
                .print(sources([(filename.to_owned(), src.clone())]))
                .unwrap()
        })
}
