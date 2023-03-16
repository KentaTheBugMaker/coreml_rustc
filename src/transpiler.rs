//! トランスパイラ　モジュール
//!

use crate::{
    flat_syntax::{self, Dec, Exp},
    typeinf::{Type, TypeEnvironment},
};

pub fn transpile(dec: Dec, type_env: &TypeEnvironment) -> String {
    let mut src = String::new();
    match dec {
        flat_syntax::Dec::Val(x, d) => match &d {
            Exp::ExpId(_)
            | Exp::Int(_)
            | Exp::String(_)
            | Exp::True
            | Exp::False
            | Exp::ExpPair(_, _)
            | Exp::ExpApp(_, _)
            | Exp::ExpProj1(_)
            | Exp::ExpProj2(_)
            | Exp::ExpPrim(_, _, _)
            | Exp::ExpIf(_, _, _) => {
                src = format!("let {} = {};", x, transpile_expr(&d, type_env));
            }
            Exp::ExpFn(_, _) | Exp::ExpFix(_, _, _) => {
                src = transpile_expr(&d, type_env);
            }
        },
    }

    src
}

pub fn type_name(ty: Type) -> String {
    match ty {
        Type::TyVar(a) => a,
        Type::Int => "i64".to_owned(),
        Type::String => "String".to_owned(),
        Type::Bool => "bool".to_owned(),
        Type::Fun(_, _) => todo!(),
        Type::Pair(a, b) => format!("({},{})", type_name(*a.clone()), type_name(*b.clone())),
        Type::Poly(_, _) => todo!(),
    }
}

pub fn transpile_expr(exp: &Exp, type_env: &TypeEnvironment) -> String {
    match exp {
        Exp::ExpId(x) => x.to_owned(),
        Exp::Int(x) => x.to_string(),
        Exp::String(x) => format!("\"{}\"", x),
        Exp::True => "true".to_owned(),
        Exp::False => "false".to_owned(),
        Exp::ExpFn(var, body) => format!("|{}|{{{}}}", var, transpile_expr(body, type_env)),
        // 匿名だからなにか適当な名前をつける必要がある.
        // クロージャであれば(|x|{x*x})(exp2) のようにしてコンパイルできる.
        Exp::ExpApp(exp1, exp2) => "".to_owned(),
        Exp::ExpPair(exp1, exp2) => format!(
            "({},{})",
            transpile_expr(exp1, type_env),
            transpile_expr(exp2, type_env)
        ),
        Exp::ExpProj1(exp) => format!("{}.0", transpile_expr(exp, type_env)),
        Exp::ExpProj2(exp) => format!("{}.1", transpile_expr(exp, type_env)),
        Exp::ExpPrim(prim, exp1, exp2) => {
            format!(
                "{} {} {}",
                transpile_expr(exp1, type_env),
                match prim {
                    crate::syntax_tree::Prim::Eq => "==",
                    crate::syntax_tree::Prim::Add => "+",
                    crate::syntax_tree::Prim::Sub => "-",
                    crate::syntax_tree::Prim::Mul => "*",
                    crate::syntax_tree::Prim::Div => "/",
                },
                transpile_expr(exp2, type_env)
            )
        }
        Exp::ExpIf(exp1, exp2, exp3) => {
            format!(
                "if {} {{ {} }} else {{ {} }}",
                transpile_expr(exp1, type_env),
                transpile_expr(exp2, type_env),
                transpile_expr(exp3, type_env)
            )
        }
        Exp::ExpFix(f, x, function_body) => {
            let mut src = String::new();
            if let Some(ty) = type_env.get(f) {
                match ty {
                    Type::Fun(a, b) => {
                        let mut generic_type_vars = vec![];
                        if let Type::TyVar(a) = a.as_ref() {
                            generic_type_vars.push(a.clone());
                        }
                        if let Type::TyVar(b) = b.as_ref() {
                            generic_type_vars.push(b.clone());
                        }
                        src.push_str("fn ");
                        src.push_str(f);
                        // 型引数をセット.
                        if !generic_type_vars.is_empty() {
                            src.push('<');
                            if generic_type_vars.len() == 2 {
                                src.push_str(&generic_type_vars[0]);
                                src.push(',');
                                src.push_str(&generic_type_vars[1]);
                            } else {
                                src.push_str(&generic_type_vars[0]);
                            }
                            src.push('>');
                        }
                        src.push('(');
                        src.push_str(&x);
                        src.push(':');
                        src.push_str(&type_name(*a.clone()));
                        src.push(')');
                        src.push_str("->");
                        src.push_str(&type_name(*b.clone()));
                        src.push('{');
                        src.push_str(&transpile_expr(&function_body, type_env));
                        src.push('}');
                    }
                    Type::Poly(tyvars, function) => {
                        if let Type::Fun(a, b) = *function.clone() {
                            src.push_str("fn ");
                            src.push_str(f);
                            // 型引数をセット.

                            src.push('<');
                            if tyvars.len() == 2 {
                                src.push_str(&tyvars[0]);
                                src.push(',');
                                src.push_str(&tyvars[1]);
                            } else {
                                src.push_str(&tyvars[0]);
                            }
                            src.push('>');

                            src.push('(');
                            src.push_str(&x);
                            src.push(':');
                            src.push_str(&type_name(*a.clone()));
                            src.push(')');
                            src.push_str("->");
                            src.push_str(&type_name(*b.clone()));
                            src.push('{');
                            src.push_str(&transpile_expr(&function_body, type_env));
                            src.push('}');
                        } else {
                            eprintln!("Type check error ")
                        }
                    }
                    _ => {
                        eprintln!("type of {} is not function", f);
                    }
                }
            } else {
                eprintln!("can't get type of {}", f);
            }
            src
        }
    }
}
