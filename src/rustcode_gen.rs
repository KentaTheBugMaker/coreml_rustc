//! convert KNormalized expressions to Rust Program.
//!

use std::collections::{BTreeMap, BTreeSet};

use crate::{
    alpha_unique::Var,
    closureconversion::{CCValue, FunID},
    knormalize::{KNClosureLike, KNDeclaration, KNExp},
    typeinf::Type,
};

/// compile given declaration and functions to rust function.
pub fn compile(decls: Vec<KNDeclaration>, functions: BTreeMap<FunID, KNClosureLike>) -> String {
    let main_bottom = KNExp::Value(crate::closureconversion::CCValue::Int(0));

    let main = KNClosureLike::Function {
        args: vec![],
        inner_exp: KNExp::ExpLet(
            decls
                .into_iter()
                .map(|decl| {
                    let KNDeclaration::Val(var, exp) = decl;
                    (var, exp)
                })
                .collect(),
            Box::new(main_bottom),
            Type::Int,
        ),
        ty: Type::Fun(Box::new(Type::Int), Box::new(Type::Int)),
    };

    let main_source = compile_function("cml_main", main);

    functions.into_iter().fold(main_source, |source, function| {
        let f_name = function.0.to_string();
        source + &compile_function(&f_name, function.1)
    })
}
/*
多相型はどうやったらいいのかわかんない.
*/
fn compile_function(name: &str, f: KNClosureLike) -> String {
    match f {
        KNClosureLike::Closure {
            env,
            args,
            inner_exp,
            ty,
        } => {
            let args = args.iter().fold("".to_owned(), |arglist, (var, ty)| {
                arglist + &format!("v{}:{}", var.id, compile_ty(ty))
            });
            let ret_ty = match &ty {
                Type::Fun(_, ret_ty) => ret_ty,
                Type::Poly(_, ty) => match ty.as_ref() {
                    Type::Fun(_, ret_ty) => ret_ty,
                    _ => unimplemented!("Poly ty is fun only"),
                },
                _ => unimplemented!(""),
            };
            let ret_ty = compile_ty(&ret_ty);
            let tyvars = tyvars(&ty, BTreeSet::new());
            let tyvars: Vec<String> = tyvars.into_iter().collect();
            let tyvars = if let Some((first, last)) = tyvars.split_first() {
                let list = last
                    .iter()
                    .fold(first.to_owned(), |first, last| first + "," + last);
                format!("<{}>", list)
            } else {
                "".to_owned()
            };

            let env_def = env.0.iter().fold("".to_owned(), |env, v| {
                env + &format!("v{}:{},", v.0.id, compile_ty(&v.1))
            });
            let env_def = format!("struct Env_{name}{tyvars}{{{env_def}}}");
            format!(
                "{}
                fn {name}{tyvars}(env:Env_{name}{tyvars},{args})->{ret_ty}{{{}}}",
                env_def,
                compile_exp(inner_exp)
            )
        }
        KNClosureLike::Function {
            args,
            inner_exp,
            ty,
        } => {
            let args = args.iter().fold("".to_owned(), |arglist, (var, ty)| {
                arglist + &format!("v{}:{}", var.id, compile_ty(ty))
            });
            let ret_ty = match &ty {
                Type::Fun(_, ret_ty) => ret_ty,
                Type::Poly(_, ty) => match ty.as_ref() {
                    Type::Fun(_, ret_ty) => ret_ty,
                    _ => unimplemented!("Poly ty is fun only"),
                },
                _ => unimplemented!(""),
            };
            let ret_ty = compile_ty(&ret_ty);
            let tyvars = tyvars(&ty, BTreeSet::new());
            let tyvars: Vec<String> = tyvars.into_iter().collect();
            let tyvars = if let Some((first, last)) = tyvars.split_first() {
                let list = last
                    .iter()
                    .fold(first.to_owned(), |first, last| first + "," + last);
                format!("<{}>", list)
            } else {
                "".to_owned()
            };

            format!(
                "fn {name}{tyvars}({args})->{ret_ty}{{{}}}",
                compile_exp(inner_exp)
            )
        }
    }
}

fn compile_value(v: CCValue) -> String {
    match v {
        CCValue::Var(v, _) => {
            let Var { id, name: _ } = v;
            format!("v{id}")
        }
        CCValue::Int(x) => format!("{x}"),
        CCValue::String(x) => format!("{x:}.to_owned()"),
        CCValue::True => "true".to_owned(),
        CCValue::False => "false".to_owned(),
    }
}

fn tyvars(ty: &Type, mut tyvar_set: BTreeSet<String>) -> BTreeSet<String> {
    match ty {
        Type::TyVar(tyvar) => {
            tyvar_set.insert(tyvar.to_owned());
            tyvar_set
        }
        Type::Int => tyvar_set,
        Type::String => tyvar_set,
        Type::Bool => tyvar_set,
        Type::Fun(arg_ty, ret_ty) => {
            let tyvar_set = tyvars(&arg_ty, tyvar_set);
            tyvars(&ret_ty, tyvar_set)
        }
        Type::Pair(v1, v2) => {
            let tyvar_set = tyvars(&v1, tyvar_set);
            tyvars(&v2, tyvar_set)
        }
        Type::Poly(btyvars, _) => {
            tyvar_set.extend(btyvars.iter().cloned());
            tyvar_set
        }
    }
}

fn compile_ty(ty: &Type) -> String {
    match ty {
        Type::TyVar(tyvar) => tyvar.to_owned(),
        Type::Int => "i32".to_owned(),
        Type::String => "String".to_owned(),
        Type::Bool => "bool".to_owned(),
        Type::Fun(arg_ty, ret_ty) => {
            format!(
                "Box<dyn Fn({})->{}>",
                compile_ty(arg_ty),
                compile_ty(ret_ty)
            )
        }
        Type::Pair(ty1, ty2) => {
            format!("({},{})", compile_ty(ty1), compile_ty(ty2))
        }
        Type::Poly(_, ty) => compile_ty(ty),
    }
}

fn compile_exp(exp: KNExp) -> String {
    match exp {
        KNExp::Value(v) => compile_value(v),
        KNExp::ExpApp(v1, v2, _) => {
            let v1 = compile_value(v1);
            let v2 = compile_value(v2);
            format!("({v1})({v2})")
        }
        KNExp::ExpCall(f, v, _) => {
            format!("{f}({})", compile_value(v))
        }
        KNExp::ExpPair(v1, v2, _) => {
            format!("({},{})", compile_value(v1), compile_value(v2))
        }
        KNExp::ExpProj1(v, _) => {
            format!("{}.0", compile_value(v))
        }
        KNExp::ExpProj2(v, _) => {
            format!("{}.1", compile_value(v))
        }
        KNExp::ExpPrim(prim, v1, v2, _) => {
            let prim = match prim {
                crate::syntax_tree::Prim::Eq => "==",
                crate::syntax_tree::Prim::Add => "+",
                crate::syntax_tree::Prim::Sub => "-",
                crate::syntax_tree::Prim::Mul => "*",
                crate::syntax_tree::Prim::Div => "/",
            };
            format!("{}{prim}{}", compile_value(v1), compile_value(v2))
        }
        KNExp::ExpIf(v, exp1, exp2) => {
            format!(
                "if {} {{{}}}else{{{}}}",
                compile_value(v),
                compile_exp(*exp1),
                compile_exp(*exp2)
            )
        }
        KNExp::ExpMkClosure(env, fid, _) => {
            let env = env.0.iter().fold("".to_owned(), |env, v| {
                env + &format!("v{}:v{},", v.0.id, v.0.id)
            });
            //format!("({fid},Env_{fid}{{{env}}})");
            format!("Box::new(|_var|{{{fid}(Env_{fid}{{{env}}},_var)}})")
        }
        KNExp::ExpLet(decls, exp, _) => {
            let mut source = String::new();
            for (var, exp) in decls {
                source += &format!("let v{} = {};\n", var.id, compile_exp(exp));
            }
            format!("{{{}{{{}}}}}", source, compile_exp(*exp))
        }
        KNExp::ExpSelect(var, _) => format!("env.v{}", var.id),
        KNExp::FPtr(f, _) => format!("Box::new({f})"),
    }
}
