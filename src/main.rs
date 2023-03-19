mod flat_syntax;
mod mincaml_parser;
mod parser_libchumsky;
mod parser_libnom;
mod secd_machine_code;
mod sml_syntax;
mod syntax_tree;
mod transpiler;
mod typeinf;

use chumsky::{prelude::Input, Parser};
use nom_locate::LocatedSpan;
use nom_recursive::RecursiveInfo;

use crate::{syntax_tree::Top, typeinf::TypeEnvironment};
/*
 * this parser contains bug
 * fun fact x = if eq(x,0) then 1 else if eq(x,1) then 1 else mul( x, fact sub(x,1))
 * fun fact x = if eq(x,0) then 1 else (if eq(x,1) then 1 else (mul( x, fact sub(x,1))))
 * fun fact x = if eq(x,0) then 1 else if eq(x,1) then 1 else mul( x, fact sub(x,1))
 */
fn main() {
    let mut program = String::new();
    let mut type_environment = TypeEnvironment::new();

    loop {
        if std::io::stdin().read_line(&mut program).is_ok() {
            println!("try to compile this program");
            {
                let s = LocatedSpan::new_extra(program.as_str(), RecursiveInfo::new());
                let ast = parser_libnom::parse(s);

                if let Ok((_span, ast)) = ast {
                    println!("ast {ast:?}");
                    let Top::Dec(dec) = ast;
                    if let Ok(ty_env) = typeinf::type_inf(&type_environment, dec.into()) {
                        type_environment = ty_env;
                    }
                }
                let (tokens, err) = parser_libchumsky::lexer()
                    .parse(&program)
                    .into_output_errors();
                if let Some(tokens) = tokens {
                    let (ast, errors) = parser_libchumsky::parse()
                        .parse(
                            tokens
                                .as_slice()
                                .spanned((program.len()..program.len()).into()),
                        )
                        .into_output_errors();
                    if let Some(ast) = ast {
                        println!("ast {ast:?}");
                        if let Ok(ty_env) = typeinf::type_inf(&type_environment, ast.into()) {
                            type_environment = ty_env;
                        }
                    }
                }
            }
            program.clear();
        }
    }
}
