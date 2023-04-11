mod ast_transpiler;
mod flat_syntax;
mod parser_libchumsky;
mod secd_machine_code;
mod syntax_tree;
mod transpiler;
mod typeinf;
use chumsky::{prelude::Input, Parser};

use crate::{ast_transpiler::CGU, flat_syntax::Dec, typeinf::TypeEnvironment};
/*
 * this parser contains bug
 * fun fact x = if eq(x,0) then 1 else mul( x, fact sub(x,1))
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
                let (tokens, _err) = parser_libchumsky::lexer()
                    .parse(&program)
                    .into_output_errors();
                if let Some(tokens) = tokens {
                    let (ast, _errors) = parser_libchumsky::parse()
                        .parse(
                            tokens
                                .as_slice()
                                .spanned((program.len()..program.len()).into()),
                        )
                        .into_output_errors();
                    if let Some(ast) = ast {
                        if let Ok(ty_env) = typeinf::type_inf(&type_environment, ast.clone().into())
                        {
                            type_environment = ty_env;
                            //型推論に成功したのでコンパイルを行う.
                            let declaration: Dec = ast.into();
                            let rust_declaration = declaration.generate_rust_declaration();
                            let cgu = CGU::new(&type_environment, rust_declaration);
                            println!(
                                "generated rust code :
                            {}
                            ",
                                cgu.to_string()
                            )
                        } else {
                            eprintln!("Type inference failed ")
                        }
                    }
                }
            }
            program.clear();
        }
    }
}
