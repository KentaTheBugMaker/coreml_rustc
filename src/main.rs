mod flat_syntax;
mod mincaml_parser;
mod parser_libchumsky;
mod secd_machine_code;
mod sml_syntax;
mod syntax_tree;
mod transpiler;
mod typeinf;

use chumsky::{prelude::Input, Parser};

use crate::{flat_syntax::Dec, secd_machine_code::Code, typeinf::TypeEnvironment};
/*
 * this parser contains bug
 * fun fact x = if eq(x,0) then 1 else mul( x, fact sub(x,1))
 * fun fact x = if eq(x,0) then 1 else (if eq(x,1) then 1 else (mul( x, fact sub(x,1))))
 * fun fact x = if eq(x,0) then 1 else if eq(x,1) then 1 else mul( x, fact sub(x,1))
 */
fn main() {
    let mut program = String::new();
    let mut type_environment = TypeEnvironment::new();
    let mut machine = secd_machine_code::Machine::new();
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
                        println!("ast {ast:?}");
                        if let Ok(ty_env) = typeinf::type_inf(&type_environment, ast.clone().into())
                        {
                            type_environment = ty_env;
                            //型推論に成功したのでコンパイルを行う.
                            let Dec::Val(name, exp) = ast.into();
                            let code = secd_machine_code::code_gen(exp, Code::blank());
                            println!(
                                "comipled to 
                            {:?}",
                                code
                            );

                            machine = machine.load_code(code);
                            let mut eval_counter = 0;
                            loop {
                                eval_counter += 1;
                                let result = machine.eval_one();
                                if let Ok(Some(value)) = result {
                                    println!("val {} = {:?}", name, value);
                                    machine.load_value(name, value);
                                    break;
                                } else if let Err(error) = result {
                                    eprintln!("{error:?}");
                                    break;
                                }
                                println!("executed step {eval_counter:}");
                            }
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
