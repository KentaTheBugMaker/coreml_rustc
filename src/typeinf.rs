//! type inference
//!
//!
//!

use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    sync::atomic::AtomicUsize,
};

use crate::{
    flat_syntax::{self, Exp},
    syntax_tree::Prim,
    typed_ast::{TypedDeclaration, TypedExp},
};
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Type {
    /// 型変数
    TyVar(String),
    /// 整数型
    Int,
    /// 文字列型
    String,
    /// 真理値型
    Bool,
    /// 関数型
    Fun(Box<Type>, Box<Type>),
    /// ペア型
    Pair(Box<Type>, Box<Type>),
    /// 多相型
    Poly(Vec<String>, Box<Type>),
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::TyVar(a) => write!(f, "{a}"),
            Type::Int => write!(f, "int"),
            Type::String => write!(f, "string"),
            Type::Bool => write!(f, "bool"),
            Type::Fun(ty1, ty2) => write!(f, "({ty1:} -> {ty2:})"),
            Type::Pair(ty1, ty2) => write!(f, "({ty1:} * {ty2:})"),
            Type::Poly(type_ids, ty) => write!(
                f,
                "[{}.{ty:}]",
                type_ids
                    .iter()
                    .enumerate()
                    .fold(String::new(), |string, (idx, type_id)| {
                        if idx == 0 {
                            string + type_id
                        } else {
                            string + "," + type_id
                        }
                    }),
            ),
        }
    }
}

impl Type {
    pub fn new_type() -> Self {
        Type::TyVar(new_type_id_name())
    }

    pub fn apply_subst(&self, subst: &Subst) -> Self {
        match self {
            Type::TyVar(x) => {
                if let Some(ty) = subst.get(x) {
                    ty.clone()
                } else {
                    self.clone()
                }
            }
            Type::Fun(a, b) => Type::Fun(
                Box::new(a.apply_subst(subst)),
                Box::new(b.apply_subst(subst)),
            ),
            Type::Pair(a, b) => Type::Pair(
                Box::new(a.apply_subst(subst)),
                Box::new(b.apply_subst(subst)),
            ),
            Type::Poly(a, b) => {
                let mut a: HashSet<_> = a.iter().collect();
                for t in subst.keys() {
                    a.remove(t);
                }
                if a.is_empty() {
                    b.apply_subst(subst)
                } else {
                    Type::Poly(
                        a.iter().cloned().cloned().collect(),
                        Box::new(b.apply_subst(subst)),
                    )
                }
            }
            _ => self.clone(),
        }
    }
}
static NEXT_TYPE_ID: once_cell::sync::OnceCell<AtomicUsize> = once_cell::sync::OnceCell::new();

fn new_type_id() -> usize {
    NEXT_TYPE_ID
        .get_or_init(|| AtomicUsize::new(0))
        .fetch_add(1, std::sync::atomic::Ordering::Relaxed)
}

fn new_type_id_name() -> String {
    fn type_id_name(type_id: usize) -> String {
        fn ord(ch: char, n: u32) -> char {
            unsafe { char::from_u32_unchecked(u32::from(ch) + n) }
        }
        fn numeral(n: usize) -> Vec<char> {
            if n < 26 {
                vec![ord('A', n as u32)]
            } else {
                let (msb, rest) = (n % 26, n / 26 - 1);
                let mut msb = vec![ord('A', msb as u32)];
                msb.extend_from_slice(&numeral(rest));
                msb
            }
        }
        numeral(type_id)
            .iter()
            .rev()
            .fold(String::new(), |mut type_id_name, ch| {
                type_id_name.push(*ch);
                type_id_name
            })
    }
    type_id_name(new_type_id())
}

pub type TypeEnvironment = HashMap<String, Type>;
// TypeUtils.sml より
pub type Subst = HashMap<String, Type>;

fn subst_ty(subst: &Subst, ty: Type) -> Type {
    match ty.clone() {
        Type::TyVar(tyid) => subst.get(&tyid).cloned().unwrap_or(ty),
        Type::Int | Type::String | Type::Bool => ty,
        Type::Fun(ty1, ty2) => Type::Fun(
            Box::new(subst_ty(subst, *ty1)),
            Box::new(subst_ty(subst, *ty2)),
        ),
        Type::Pair(ty1, ty2) => Type::Pair(
            Box::new(subst_ty(subst, *ty1)),
            Box::new(subst_ty(subst, *ty2)),
        ),
        Type::Poly(type_ids, ty) => Type::Poly(type_ids, Box::new(subst_ty(subst, *ty))),
    }
}

fn compose_subst(subst1: &Subst, subst2: &Subst) -> Subst {
    let subst2: Subst = subst2
        .iter()
        .map(|(key, ty)| (key.clone(), subst_ty(subst1, ty.clone())))
        .collect();
    let mut subst1 = subst1.clone();
    subst1.extend(subst2);
    subst1
}
fn subst_tyenv(subst: &Subst, tyenv: &TypeEnvironment) -> TypeEnvironment {
    tyenv
        .iter()
        .map(|(key, ty)| (key.clone(), subst_ty(subst, ty.clone())))
        .collect()
}

fn fresh_inst(ty: Type) -> Type {
    match ty {
        Type::Poly(type_ids, ty) => subst_ty(
            &type_ids.iter().fold(Subst::new(), |mut s, type_id| {
                s.insert(type_id.clone(), Type::new_type());
                s
            }),
            *ty,
        ),
        _ => ty,
    }
}

#[derive(Debug)]
pub enum TypeError {
    Unify,
    Unknown(String),
    OccurCheck,
}

// TypeInf.sml

pub fn type_inf(
    gamma: &TypeEnvironment,
    dec: flat_syntax::Dec,
) -> Result<(TypeEnvironment, TypedDeclaration), TypeError> {
    let flat_syntax::Dec::Val(id, exp) = dec;
    let (subst, ty, exp) = w(gamma, &exp)?;
    let tids = ftv(ty.clone()).iter().cloned().collect::<Vec<String>>();
    let exp = exp.apply_subst(&subst);
    // 型引数がすべて束縛されているならば,
    let new_ty = if tids.is_empty() {
        ty
    } else {
        Type::Poly(tids, Box::new(ty))
    };

    let mut gamma = gamma.clone();
    gamma.insert(id.clone(), new_ty);
    Ok((gamma, TypedDeclaration::Val(id, exp)))
}
// UnifyTy.sml
fn ftv(ty: Type) -> HashSet<String> {
    fn scan(ty: Type, set: &HashSet<String>) -> HashSet<String> {
        match ty {
            Type::TyVar(var) => {
                let mut set = set.clone();
                set.insert(var);
                set
            }
            Type::Fun(dom_ty, ran_ty) => scan(*ran_ty, &scan(*dom_ty, set)),
            Type::Pair(first_ty, second_ty) => scan(*second_ty, &scan(*first_ty, set)),
            _ => set.clone(),
        }
    }
    scan(ty, &HashSet::new())
}

fn rewrite(list: &[(Type, Type)], s: &Subst) -> Result<Subst, TypeError> {
    if list.is_empty() {
        Ok(s.clone())
    } else {
        let ((ty1, ty2), list) = list.split_last().unwrap();
        if ty1 == ty2 {
            rewrite(list, s)
        } else {
            match (ty1, ty2) {
                (Type::TyVar(tv), _) => {
                    if occurs(ty1.clone(), ty2.clone()) {
                        Err(TypeError::OccurCheck)
                    } else {
                        let mut s1 = HashMap::new();
                        s1.insert(tv.clone(), ty2.clone());
                        rewrite(
                            &list
                                .iter()
                                .map(|(ty1, ty2)| {
                                    (subst_ty(&s1, ty1.clone()), subst_ty(&s1, ty2.clone()))
                                })
                                .collect::<Vec<_>>(),
                            &compose_subst(&s1, s),
                        )
                    }
                }
                (_, Type::TyVar(_)) => {
                    let mut list = list.to_vec();
                    list.push((ty2.clone(), ty1.clone()));
                    rewrite(&list, s)
                }
                (Type::Fun(ty11, ty12), Type::Fun(ty21, ty22)) => {
                    let mut list = list.to_vec();
                    list.push((*ty12.clone(), *ty22.clone()));
                    list.push((*ty11.clone(), *ty21.clone()));
                    rewrite(&list, s)
                }
                (Type::Pair(ty11, ty12), Type::Pair(ty21, ty22)) => {
                    let mut list = list.to_vec();
                    list.push((*ty12.clone(), *ty22.clone()));
                    list.push((*ty11.clone(), *ty21.clone()));
                    rewrite(&list, s)
                }
                _ => Err(TypeError::Unify),
            }
        }
    }
}

fn unify(list: &[(Type, Type)]) -> Result<Subst, TypeError> {
    rewrite(list, &Subst::new())
}

fn occurs(ty1: Type, ty2: Type) -> bool {
    match ty1 {
        Type::TyVar(var) => ftv(ty2).contains(&var),
        _ => false,
    }
}

// TypeInf.sml
pub fn w(gamma: &TypeEnvironment, exp: &Exp) -> Result<(Subst, Type, TypedExp), TypeError> {
    match exp {
        Exp::ExpId(var) => gamma
            .get(var)
            .ok_or(TypeError::Unknown(var.to_owned()))
            .map(|ty| {
                (
                    Subst::new(),
                    fresh_inst(ty.clone()),
                    TypedExp::ExpId(var.clone(), ty.clone()),
                )
            }),
        Exp::Int(x) => Ok((Subst::new(), Type::Int, TypedExp::Int(*x))),
        Exp::String(x) => Ok((Subst::new(), Type::String, TypedExp::String(x.clone()))),
        Exp::True => Ok((Subst::new(), Type::Bool, TypedExp::True)),
        Exp::False => Ok((Subst::new(), Type::Bool, TypedExp::False)),
        Exp::ExpFn(x, exp) => {
            let ty1 = Type::new_type();
            let mut new_gamma = gamma.clone();
            new_gamma.insert(x.clone(), ty1.clone());
            let (s, ty2, exp) = w(&new_gamma, exp)?;
            let ty = Type::Fun(Box::new(subst_ty(&s, ty1)), Box::new(ty2));
            Ok((
                s,
                ty.clone(),
                TypedExp::ExpFn(x.to_owned(), Box::new(exp), ty),
            ))
        }
        Exp::ExpApp(exp1, exp2) => {
            let (s1, ty1, exp1) = w(gamma, exp1)?;
            let (s2, ty2, exp2) = w(gamma, exp2)?;
            let ty3 = Type::new_type();
            let s3 = unify(&[(
                Type::Fun(Box::new(ty2), Box::new(ty3.clone())),
                subst_ty(&s2, ty1),
            )])?;
            let s4 = compose_subst(&s3, &compose_subst(&s1, &s2));
            Ok((
                s4.clone(),
                subst_ty(&s4, ty3.clone()),
                TypedExp::ExpApp(Box::new(exp1), Box::new(exp2), subst_ty(&s4, ty3)),
            ))
        }
        Exp::ExpPair(exp1, exp2) => {
            let (s1, ty1, exp1) = w(gamma, exp1)?;
            let (s2, ty2, exp2) = w(&subst_tyenv(&s1, gamma), exp2)?;
            let ty = Type::Pair(Box::new(subst_ty(&s2, ty1)), Box::new(ty2));
            Ok((
                compose_subst(&s2, &s1),
                ty.clone(),
                TypedExp::ExpPair(Box::new(exp1), Box::new(exp2), ty),
            ))
        }
        Exp::ExpProj1(exp) => {
            let (s1, ty, exp) = w(gamma, exp)?;
            let ty1 = Type::new_type();
            let ty2 = Type::new_type();
            let s2 = unify(&[(ty, Type::Pair(Box::new(ty1.clone()), Box::new(ty2)))])?;
            Ok((
                compose_subst(&s2, &s1),
                subst_ty(&s2, ty1.clone()),
                TypedExp::ExpProj1(Box::new(exp), subst_ty(&s2, ty1)),
            ))
        }
        Exp::ExpProj2(exp) => {
            let (s1, ty, exp) = w(gamma, exp)?;
            let ty1 = Type::new_type();
            let ty2 = Type::new_type();
            let s2 = unify(&[(ty, Type::Pair(Box::new(ty1), Box::new(ty2.clone())))])?;
            Ok((
                compose_subst(&s2, &s1),
                subst_ty(&s2, ty2.clone()),
                TypedExp::ExpProj2(Box::new(exp), subst_ty(&s2, ty2)),
            ))
        }
        Exp::ExpPrim(p, exp1, exp2) => {
            let (s1, ty1, exp1) = w(gamma, exp1)?;
            let (s2, ty2, exp2) = w(gamma, exp2)?;
            let s3 = unify(&[(subst_ty(&s2, ty1), Type::Int), (ty2, Type::Int)])?;
            let ty3 = if let Prim::Eq = p {
                Type::Bool
            } else {
                Type::Int
            };
            Ok((
                compose_subst(&s3, &compose_subst(&s2, &s1)),
                ty3.clone(),
                TypedExp::ExpPrim(p.clone(), Box::new(exp1), Box::new(exp2), ty3),
            ))
        }
        Exp::ExpIf(exp1, exp2, exp3) => {
            let (s1, ty1, exp1) = w(gamma, exp1)?;
            let s2 = unify(&[(ty1, Type::Bool)])?;
            let (s3, ty2, exp2) = w(&subst_tyenv(&compose_subst(&s2, &s1), gamma), exp2)?;
            let (s4, ty3, exp3) = w(
                &subst_tyenv(&compose_subst(&s3, &compose_subst(&s2, &s1)), gamma),
                exp3,
            )?;
            let s5 = unify(&[(ty2.clone(), ty3)])?;
            let s = compose_subst(
                &s5,
                &compose_subst(&s4, &compose_subst(&s3, &compose_subst(&s2, &s1))),
            );
            let _new_gamma = subst_tyenv(&s, gamma);
            Ok((
                s,
                subst_ty(&s5, ty2),
                TypedExp::ExpIf(Box::new(exp1), Box::new(exp2), Box::new(exp3)),
            ))
        }
        Exp::ExpFix(fid, xid, exp) => {
            let arg_ty = Type::new_type();
            let body_ty = Type::new_type();
            let fun_ty = Type::Fun(Box::new(arg_ty.clone()), Box::new(body_ty.clone()));
            let mut new_gamma = gamma.clone();
            new_gamma.insert(fid.clone(), fun_ty.clone());
            new_gamma.insert(xid.clone(), arg_ty);
            let (s1, ty, exp) = w(&new_gamma, exp)?;
            let s2 = unify(&[(ty, body_ty)])?;
            let s = compose_subst(&s2, &s1);
            Ok((
                s.clone(),
                subst_ty(&s, fun_ty.clone()),
                TypedExp::ExpFix(
                    fid.clone(),
                    xid.clone(),
                    Box::new(exp),
                    subst_ty(&s, fun_ty),
                ),
            ))
        }
    }
}
