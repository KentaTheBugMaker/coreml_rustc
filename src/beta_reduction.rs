//!  in this phase alpha converted expression to dead code removed ir.
//!
//! use beta reduction and remove unused variables.

use std::collections::BTreeSet;

use crate::alpha_unique::{AUDeclaration, AUExp, Var};

type UsedMap = BTreeSet<Var>;
type BoundingMap = BTreeSet<Var>;

///式の中から使われていないtoplevel 宣言を除去する.
pub fn remove_toplevel(decls: Vec<AUDeclaration>) -> Vec<AUDeclaration> {
    let used_map = used_map(&decls);
    decls
        .into_iter()
        .filter(|AUDeclaration::Val(var, _)| used_map.contains(var))
        .collect()
}

pub fn beta_reduction_toplevel(decls: Vec<AUDeclaration>) -> Vec<AUDeclaration> {
    decls
        .into_iter()
        .map(|AUDeclaration::Val(var, exp)| AUDeclaration::Val(var.clone(), beta_reduction(&exp)))
        .collect()
}

fn used_map(decls: &[AUDeclaration]) -> UsedMap {
    decls
        .iter()
        .rev()
        .fold(UsedMap::new(), |used_map, AUDeclaration::Val(_, exp)| {
            used_map_exp(exp, used_map)
        })
}

/// 変数が使われているか調べる.
/// 自由変数も束縛変数も関係なく調べる.

fn used_map_exp(exp: &AUExp, mut used_map: UsedMap) -> UsedMap {
    match exp {
        AUExp::ExpId(x, _) => {
            used_map.insert(x.clone());
            used_map
        }
        AUExp::Int(_) => used_map,
        AUExp::String(_) => used_map,
        AUExp::True => used_map,
        AUExp::False => used_map,
        AUExp::ExpFn(_, exp, _) => used_map_exp(exp, used_map),
        AUExp::ExpApp(exp1, exp2, _) => {
            let used_map1 = used_map_exp(exp1, used_map);
            used_map_exp(exp2, used_map1)
        }
        AUExp::ExpPair(exp1, exp2, _) => {
            let used_map1 = used_map_exp(exp1, used_map);
            used_map_exp(exp2, used_map1)
        }
        AUExp::ExpProj1(exp, _) => used_map_exp(exp, used_map),
        AUExp::ExpProj2(exp, _) => used_map_exp(exp, used_map),
        AUExp::ExpPrim(_, exp1, exp2, _) => {
            let used_map1 = used_map_exp(exp1, used_map);
            used_map_exp(exp2, used_map1)
        }
        AUExp::ExpIf(exp1, exp2, exp3) => {
            let used_map1 = used_map_exp(exp1, used_map);
            let used_map2 = used_map_exp(exp2, used_map1);
            used_map_exp(exp3, used_map2)
        }
        AUExp::ExpFix(_, _, exp, _) => used_map_exp(exp, used_map),
    }
}

/// 関数式で束縛している変数の集合.
fn boundings(exp: &AUExp) -> BoundingMap {
    let empty = BoundingMap::new();
    match exp {
        AUExp::ExpId(_, _) => empty,
        AUExp::Int(_) => empty,
        AUExp::String(_) => empty,
        AUExp::True => empty,
        AUExp::False => empty,
        AUExp::ExpFn(x, _, _) => [x.to_owned()].into_iter().collect(),
        AUExp::ExpApp(_, _, _) => empty,
        AUExp::ExpPair(_, _, _) => empty,
        AUExp::ExpProj1(_, _) => empty,
        AUExp::ExpProj2(_, _) => empty,
        AUExp::ExpPrim(_, _, _, _) => empty,
        AUExp::ExpIf(_, _, _) => empty,
        AUExp::ExpFix(f, x, _, _) => [f.to_owned(), x.to_owned()].into_iter().collect(),
    }
}
/// もし (fn x => 1) のような式の内部で変数を使わないようなものがあれば,それは内部の式そのものに落とすことができる.
/// #1 (1234,543)のような式があれば、内部の式にする.
/// 一種のβ簡約を行う.
fn beta_reduction(exp: &AUExp) -> AUExp {
    match exp {
        AUExp::ExpFn(var, inner, ty) => {
            AUExp::ExpFn(var.clone(), Box::new(beta_reduction(inner)), ty.clone())
        }
        AUExp::ExpApp(exp1, exp2, ty) => {
            let used_map1 = used_map_exp(exp1, UsedMap::new());
            let bound_map = boundings(exp1);
            if used_map1.is_disjoint(&bound_map) {
                match exp1.as_ref() {
                    AUExp::ExpFn(_, inner, _) => {
                        println!("detected non real function");
                        beta_reduction(inner)
                    }
                    AUExp::ExpFix(_, _, inner, _) => {
                        println!("detected non real function");
                        beta_reduction(inner)
                    }
                    exp1 => AUExp::ExpApp(
                        Box::new(beta_reduction(exp1)),
                        Box::new(beta_reduction(exp2)),
                        ty.clone(),
                    ),
                }
            } else {
                AUExp::ExpApp(exp1.clone(), Box::new(beta_reduction(exp2)), ty.clone())
            }
        }
        AUExp::ExpPair(exp1, exp2, ty) => AUExp::ExpPair(
            Box::new(beta_reduction(exp1)),
            Box::new(beta_reduction(exp2)),
            ty.clone(),
        ),
        AUExp::ExpProj1(exp, ty) => match exp.as_ref() {
            AUExp::ExpPair(exp, _, _) => {
                println!("detected #1 (exp1,exp2)");
                *(exp.clone())
            }
            exp => AUExp::ExpProj1(Box::new(beta_reduction(exp)), ty.clone()),
        },
        AUExp::ExpProj2(exp, ty) => match exp.as_ref() {
            AUExp::ExpPair(_, exp, _) => {
                println!("detected #2 (exp1,exp2)");
                *(exp.clone())
            }
            exp => AUExp::ExpProj2(Box::new(beta_reduction(exp)), ty.clone()),
        },
        AUExp::ExpPrim(prim, exp1, exp2, ty) => AUExp::ExpPrim(
            prim.clone(),
            Box::new(beta_reduction(exp1)),
            Box::new(beta_reduction(exp2)),
            ty.clone(),
        ),
        AUExp::ExpIf(exp1, exp2, exp3) => AUExp::ExpIf(
            Box::new(beta_reduction(exp1)),
            Box::new(beta_reduction(exp2)),
            Box::new(beta_reduction(exp3)),
        ),
        AUExp::ExpFix(f, x, inner, ty) => AUExp::ExpFix(
            f.clone(),
            x.clone(),
            Box::new(beta_reduction(inner)),
            ty.clone(),
        ),
        x => x.clone(),
    }
}
