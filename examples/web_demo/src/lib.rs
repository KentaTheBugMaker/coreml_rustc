use wasm_bindgen::prelude::*;
use coreml_rustc::{
    ast_transpiler::CGU,
    flat_syntax::Dec,
    parser_libchumsky,
    typeinf::{self, TypeEnvironment},
    Input, Parser,
};
#[wasm_bindgen]
pub fn transpile(src:String)->Result<String,JsValue>{
    let mut type_environment = TypeEnvironment::new();
    let mut generated = String::new();
    for dec in src.split(';') {
        let (tokens, _err) = parser_libchumsky::lexer()
            .parse(&dec)
            .into_output_errors();
        if let Some(tokens) = tokens {
            let (ast, errors) = parser_libchumsky::parse()
                .parse(
                    tokens
                        .as_slice()
                        .spanned((dec.len()..dec.len()).into()),
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
                    generated.push_str(&cgu.to_string());
                    generated.push('\n');
                } 
            } 
        } 
    }

    Ok(generated)

}