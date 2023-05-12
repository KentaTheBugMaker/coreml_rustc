use std::collections::HashMap;

use crate::{
    typed_ast::{TypedDeclaration, TypedExp},
    typeinf::Type,
};

impl TypedDeclaration {
    pub fn code_gen(&self) -> String {
        let mut buffer = String::new();
        match self {
            TypedDeclaration::Val(x, code) => {
                buffer.push_str(&format!("let {x:} = {{"));
                buffer.push_str(&code.code_gen("", ""));
                buffer.push_str("};");
            }
        }
        buffer
    }
}
impl TypedExp {
    fn code_gen(&self, recursive_closure_name: &str, e_name: &str) -> String {
        let mut buffer = String::new();
        match self {
            crate::typed_ast::TypedExp::ExpId(x, _ty) => buffer.push_str(x),
            crate::typed_ast::TypedExp::Int(x) => buffer.push_str(&format!("{x:}")),
            crate::typed_ast::TypedExp::String(x) => buffer.push_str(x),
            crate::typed_ast::TypedExp::True => buffer.push_str("true"),
            crate::typed_ast::TypedExp::False => buffer.push_str("false"),
            crate::typed_ast::TypedExp::ExpFn(x, code, ty) => {
                if let Type::Fun(var, result) = ty {
                    buffer.push_str(&format!(
                        "move |{x:}:{}|{{{}}}",
                        var.rustic_name(),
                        code.code_gen(recursive_closure_name, e_name)
                    ))
                }
            }
            crate::typed_ast::TypedExp::ExpApp(a, b, _ty) => {
                // 再帰クロージャへの参照を検出.
                if let TypedExp::ExpId(x, _) = a.as_ref() {
                    if x == recursive_closure_name {
                        buffer += &format!(
                            "body({e_name},{})",
                            b.code_gen(recursive_closure_name, e_name)
                        )
                    } else {
                        buffer += &format!(
                            "{}({})",
                            a.code_gen(recursive_closure_name, e_name),
                            b.code_gen(recursive_closure_name, e_name)
                        );
                    }
                } else {
                    buffer += &format!(
                        "({})({})",
                        a.code_gen(recursive_closure_name, e_name),
                        b.code_gen(recursive_closure_name, e_name)
                    );
                }
            }
            crate::typed_ast::TypedExp::ExpPair(a, b, _) => {
                buffer += &format!(
                    "({},{})",
                    a.code_gen(recursive_closure_name, e_name),
                    b.code_gen(recursive_closure_name, e_name)
                );
            }
            crate::typed_ast::TypedExp::ExpProj1(a, _) => {
                buffer += &format!("{}.0", a.code_gen(recursive_closure_name, e_name))
            }
            crate::typed_ast::TypedExp::ExpProj2(a, _) => {
                buffer += &format!("{}.1", a.code_gen(recursive_closure_name, e_name))
            }
            crate::typed_ast::TypedExp::ExpPrim(prim, a, b, _) => {
                buffer += &format!(
                    "{} {} {}",
                    a.code_gen(recursive_closure_name, e_name),
                    match prim {
                        crate::syntax_tree::Prim::Eq => "==",
                        crate::syntax_tree::Prim::Add => "+",
                        crate::syntax_tree::Prim::Sub => "-",
                        crate::syntax_tree::Prim::Mul => "*",
                        crate::syntax_tree::Prim::Div => "/",
                    },
                    b.code_gen(recursive_closure_name, e_name)
                )
            }
            crate::typed_ast::TypedExp::ExpIf(a, b, c) => {
                buffer += &format!(
                    "if {} {{{}}}else{{{}}}",
                    a.code_gen(recursive_closure_name, e_name),
                    b.code_gen(recursive_closure_name, e_name),
                    c.code_gen(recursive_closure_name, e_name)
                );
            }
            crate::typed_ast::TypedExp::ExpFix(f, x, c, ty) => {
                if let Type::Fun(var, result) = ty {
                    let mut fv_list = c.fv();
                    //環境名は衝突してはならないので,自動的に生成する.
                    let e_name = (0..)
                        .map(|x| format!("env_{x:}"))
                        .find(|e_name| !fv_list.contains_key(e_name))
                        .unwrap();

                    fv_list.remove(f);
                    fv_list.remove(x);
                    // プリアンブルの作成
                    buffer += &format!(
                        "struct Env {{{}}}\n",
                        fv_list
                            .iter()
                            .map(|(var, ty)| { format!("{var}:{},\n", ty.rustic_name()) })
                            .fold(String::new(), |acc, x| { acc + &x })
                    );
                    //クロージャのシグネチャの生成
                    buffer += &format!(
                        "fn body({e_name:}:&Env,{x:}:{})->{}{{",
                        var.rustic_name(),
                        result.rustic_name(),
                    );
                    //環境へのアクセスのためにローカル変数を宣言.
                    fv_list.keys().for_each(|k| {
                        buffer += &format!("let {k:} ={e_name}.{k:};");
                    });
                    //コード生成
                    buffer += &c.code_gen(f, &e_name);
                    //関数を閉じる
                    buffer += "}";
                    //クロージャ生成
                    buffer += &format!(
                        "move |{x:}|{{ body(&Env{{{}}},{x:}) }}",
                        fv_list
                            .keys()
                            .fold(String::new(), |acc, x| { acc + x + "," })
                    );
                }
            }
        }
        buffer
    }
    ///自由変数 型も含む
    fn fv(&self) -> HashMap<String, Type> {
        match self {
            TypedExp::ExpId(x, ty) => {
                let mut set = HashMap::new();
                set.insert(x.clone(), ty.clone());
                set
            }
            TypedExp::ExpFn(x, c, _) => {
                let mut fv = c.fv();
                fv.remove(x);
                fv
            }
            TypedExp::ExpApp(a, b, _)
            | TypedExp::ExpPair(a, b, _)
            | TypedExp::ExpPrim(_, a, b, _) => {
                let mut a = a.fv();
                let b = b.fv();
                a.extend(b);
                a
            }
            TypedExp::ExpProj1(a, _) | TypedExp::ExpProj2(a, _) => a.fv(),
            TypedExp::ExpIf(a, b, c) => {
                let mut a = a.fv();
                a.extend(b.fv());
                a.extend(c.fv());
                a
            }
            TypedExp::ExpFix(f, x, c, _) => {
                let mut fv = c.fv();
                fv.remove(x);
                fv.remove(f);
                fv
            }
            _ => HashMap::new(),
        }
    }
}
