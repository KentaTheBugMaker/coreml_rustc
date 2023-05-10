use coreml_rustc::{
    parser_libchumsky,
    typed_ast::TypedDeclaration,
    typeinf::{self, TypeEnvironment},
    Input, Parser,
};
/*
 * this parser contains bug
 * fun fact x = if eq(x,0) then 1 else mul( x, fact sub(x,1))
 * fun fact x = if eq(x,0) then 1 else (if eq(x,1) then 1 else (mul( x, fact sub(x,1))))
 * fun fact x = if eq(x,0) then 1 else if eq(x,1) then 1 else mul( x, fact sub(x,1))
 */

fn main() {
    let mut program = String::new();
    let mut type_environment = TypeEnvironment::new();
    let mut vm = coreml_rustc::secd_machine_code::Machine::new();

    loop {
        if std::io::stdin().read_line(&mut program).is_ok() {
            println!("try to compile this program");
            {
                let (tokens, _err) = parser_libchumsky::lexer()
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
                        if let Ok((ty_env, typed_dec)) =
                            typeinf::type_inf(&type_environment, ast.clone().into())
                        {
                            println!(
                                "input :
                            {}
                            ",
                                typed_dec
                            );
                            type_environment = ty_env;
                            let TypedDeclaration::Val(x, ast) = typed_dec;
                            let asm = coreml_rustc::secd_machine_code::code_gen(
                                ast,
                                coreml_rustc::secd_machine_code::Code::blank(),
                            );
                            println!("asm {asm:?}");
                            vm = vm.load_code(asm);
                            loop {
                                let result = vm.eval_one();
                                match result {
                                    Ok(Some(value)) => {
                                        println!("val {}  = {:?}", x, value);
                                        vm.load_value(x, value);
                                        break;
                                    }
                                    Err(runtime_err) => {
                                        println!("runtime error {:?} occured ", runtime_err);
                                        break;
                                    }
                                    Ok(_) => {}
                                }
                            }
                        } else {
                            eprintln!("Type inference failed ")
                        }
                    } else {
                        eprintln!("Failed to build AST {:#?}", errors);
                    }
                } else {
                    eprintln!("Tokenizer returned error")
                }
            }
            program.clear();
        }
    }
}
