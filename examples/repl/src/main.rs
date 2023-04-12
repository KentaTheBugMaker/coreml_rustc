use coreml_rustc::{
    ast_transpiler::CGU,
    flat_syntax::Dec,
    parser_libchumsky,
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
                    } else {
                        eprintln!("Failed to build AST {:?}", errors)
                    }
                } else {
                    eprintln!("Tokenizer returned error")
                }
            }
            program.clear();
        }
    }
}
