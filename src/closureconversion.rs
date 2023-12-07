//! this module compile TypedExp to   

use std::collections::BTreeMap;

use crate::{syntax_tree::Prim, typed_ast::TypedExp, typeinf::Type};

/// define closure added types.
///
#[derive(Debug, Clone)]
enum ClosureType {
    Environment(Vec<ClosureType>),
    TyVar(String),
    Int,
    Bool,
    String,
    Fun(Box<ClosureType>, Box<ClosureType>),
    Pair(Box<ClosureType>, Box<ClosureType>),
    Poly(Vec<String>, Box<ClosureType>),
}

fn free_vars(exp: &TypedExp) -> BTreeMap<String, Type> {
    match exp {
        TypedExp::ExpId(x, ty) => [(x.to_string(), ty.to_owned())].into_iter().collect(),
        TypedExp::Int(_) => BTreeMap::new(),
        TypedExp::String(_) => BTreeMap::new(),
        TypedExp::True => BTreeMap::new(),
        TypedExp::False => BTreeMap::new(),
        TypedExp::ExpFn(bound, exp, _) => {
            let mut fvs = free_vars(exp);
            fvs.remove(bound);
            fvs
        }
        TypedExp::ExpApp(exp1, exp2, _) => {
            let mut fvs1 = free_vars(exp1);
            let fvs2 = free_vars(exp2);
            fvs1.extend(fvs2);
            fvs1
        }
        TypedExp::ExpPair(exp1, exp2, _) => {
            let mut fvs1 = free_vars(exp1);
            let fvs2 = free_vars(exp2);
            fvs1.extend(fvs2);
            fvs1
        }
        TypedExp::ExpProj1(exp, _) => free_vars(exp),
        TypedExp::ExpProj2(exp, _) => free_vars(exp),
        TypedExp::ExpPrim(_, exp1, exp2, _) => {
            let mut fvs1 = free_vars(exp1);
            let fvs2 = free_vars(exp2);
            fvs1.extend(fvs2);
            fvs1
        }
        TypedExp::ExpIf(exp1, exp2, exp3) => {
            let mut fvs1 = free_vars(exp1);
            let fvs2 = free_vars(exp2);
            let fvs3 = free_vars(exp3);
            fvs1.extend(fvs2);
            fvs1.extend(fvs3);
            fvs1
        }
        TypedExp::ExpFix(bound1, bound2, exp, _) => {
            let mut fvs = free_vars(exp);
            fvs.remove(bound1);
            fvs.remove(bound2);
            fvs
        }
    }
}

/// convert closure to function that take (environment , arg)
/// when closure created enviroment and function ptr will created.
///   
enum NonClosureExp {
    ExpId(String, ClosureType),
    Int(i64),
    String(String),
    True,
    False,
    ExpFn {
        var: String,
        inner: Box<NonClosureExp>,
        fun_type: ClosureType,
    },
    ExpApp(Box<NonClosureExp>, Box<NonClosureExp>, Type),
    ExpPair(Box<NonClosureExp>, Box<NonClosureExp>, Type),
    ExpProj1(Box<NonClosureExp>, Type),
    ExpProj2(Box<NonClosureExp>, Type),
    ExpPrim(Prim, Box<NonClosureExp>, Box<NonClosureExp>, Type),
    ExpIf(Box<NonClosureExp>, Box<NonClosureExp>, Box<NonClosureExp>),
    ExpFix {
        f_name: String,
        var_name: String,
        inner: Box<NonClosureExp>,
        fun_type: Type,
    },
}
