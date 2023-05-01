use coreml_rustc::{
    ast_transpiler::CGU,
    flat_syntax::Dec,
    parser_libchumsky,
    typed_ast::TypedDeclaration,
    typeinf::{self, TypeEnvironment},
    Input, Parser,
};
use wasm_bindgen::prelude::*;
#[wasm_bindgen]
pub fn transpile(src: String) -> Result<String, JsValue> {
    let mut type_environment = TypeEnvironment::new();
    let mut generated = String::new();
    for dec in src.split(';') {
        let (tokens, _err) = parser_libchumsky::lexer().parse(&dec).into_output_errors();
        if let Some(tokens) = tokens {
            let (ast, _errors) = parser_libchumsky::parse()
                .parse(tokens.as_slice().spanned((dec.len()..dec.len()).into()))
                .into_output_errors();
            if let Some(ast) = ast {
                if let Ok((ty_env, typed_ast)) =
                    typeinf::type_inf(&type_environment, ast.clone().into())
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
#[wasm_bindgen]
pub fn execute(src: String) -> Result<String, JsValue> {
    let mut type_environment = TypeEnvironment::new();
    let mut vm = coreml_rustc::secd_machine_code::Machine::new();
    let mut console = String::new();
    for dec in src.split(';') {
        let (tokens, _err) = parser_libchumsky::lexer().parse(&dec).into_output_errors();
        if let Some(tokens) = tokens {
            let (ast, _errors) = parser_libchumsky::parse()
                .parse(tokens.as_slice().spanned((dec.len()..dec.len()).into()))
                .into_output_errors();
            if let Some(ast) = ast {
                if let Ok((ty_env, typed_ast)) =
                    typeinf::type_inf(&type_environment, ast.clone().into())
                {
                    type_environment = ty_env;
                    //型推論に成功したのでコンパイルを行う.
                    let TypedDeclaration::Val(x, ast) = typed_ast;

                    let asm = coreml_rustc::secd_machine_code::code_gen(
                        ast,
                        coreml_rustc::secd_machine_code::Code::blank(),
                    );
                    vm = vm.load_code(asm);
                    loop {
                        let result = vm.eval_one();
                        match result {
                            Ok(Some(value)) => {
                                console.push_str(&format!("val {}  = {:?} \n", x, value));
                                vm.load_value(x, value);
                                break;
                            }
                            Err(runtime_err) => {
                                console.push_str(&format!(
                                    "runtime error {:?} occured \n",
                                    runtime_err
                                ));
                                break;
                            }
                            Ok(_) => {}
                        }
                    }
                }
            }
        }
    }
    Ok(console)
}
