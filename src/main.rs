mod flat_syntax;
mod parser_libchumsky;
mod secd_machine_code;
mod sml_syntax;
mod syntax_tree;
mod transpiler;
mod typeinf;
use chumsky::{prelude::Input, Parser};

use crate::typeinf::TypeEnvironment;
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

            let (tokens, _errs) = parser_libchumsky::lexer()
                .parse(program.as_str())
                .into_output_errors();

            println!("tokenized {:?}", tokens);
            if let Some(tokens) = tokens {
                let (ast, errors) = parser_libchumsky::parse()
                    .map_with_span(|ast, span| (ast, span))
                    .parse(
                        tokens
                            .as_slice()
                            .spanned((program.len()..program.len()).into()),
                    )
                    .into_output_errors();
                if let Some((ast, _span)) = ast {
                    println!("ast {ast:?}");
                    if let Ok(ty_env) = typeinf::type_inf(&type_environment, ast.into()) {
                        type_environment = ty_env;
                    }
                } else {
                    println!("failed to parse {:?}", errors)
                }
            }
            program.clear();
        }
    }
}
