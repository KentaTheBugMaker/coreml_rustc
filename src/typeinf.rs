//! type inference
//!
//!
//!

use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    sync::atomic::AtomicUsize,
};

use chumsky::span::Span;

use crate::{
    flat_syntax::{self, Exp, Prim},
    parser_libchumsky::{MySpan, Spanned},
    typed_ast::{TypedDeclaration, TypedExp},
    typeinf_errors::TypeInfErrors,
};
#[derive(Debug, Eq, Clone)]
pub enum SpannedType {
    /// 型変数
    TyVar(String, MySpan),
    /// 整数型
    Int(MySpan),
    /// 文字列型
    String(MySpan),
    /// 真理値型
    Bool(MySpan),
    /// 関数型
    Fun(Box<SpannedType>, MySpan, Box<SpannedType>, MySpan),
    /// ペア型
    Pair(Box<SpannedType>, MySpan, Box<SpannedType>, MySpan),
    /// 多相型
    Poly(Vec<String>, Box<SpannedType>, MySpan),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    TyVar(String),
    Int,
    String,
    Bool,
    Fun(Box<Type>, Box<Type>),
    Pair(Box<Type>, Box<Type>),
    Poly(Vec<String>, Box<Type>),
}

impl PartialEq for SpannedType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::TyVar(l0, l1), Self::TyVar(r0, r1)) => l0 == r0,
            (Self::Int(l0), Self::Int(r0)) => true,
            (Self::String(l0), Self::String(r0)) => true,
            (Self::Bool(l0), Self::Bool(r0)) => true,
            (Self::Fun(l0, l1, l2, l3), Self::Fun(r0, r1, r2, r3)) => l0 == r0 && l2 == r2,
            (Self::Pair(l0, l1, l2, l3), Self::Pair(r0, r1, r2, r3)) => l0 == r0 && l2 == r2,
            (Self::Poly(l0, l1, l2), Self::Poly(r0, r1, r2)) => l0 == r0 && l1 == r1,
            _ => false,
        }
    }
}

impl Display for SpannedType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SpannedType::TyVar(a, _) => write!(f, "{a}"),
            SpannedType::Int(_) => write!(f, "int"),
            SpannedType::String(_) => write!(f, "string"),
            SpannedType::Bool(_) => write!(f, "bool"),
            SpannedType::Fun(ty1, _, ty2, _) => write!(f, "({ty1:} -> {ty2:})"),
            SpannedType::Pair(ty1, _, ty2, _) => write!(f, "({ty1:} * {ty2:})"),
            SpannedType::Poly(type_ids, ty, _) => write!(
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
    pub fn apply_subst(&self, s: &Subst) -> Self {
        match self {
            Type::TyVar(x) => {
                if let Some(ty) = s.get(x) {
                    ty.clone().remove_span()
                } else {
                    self.clone()
                }
            }
            Type::Int => Type::Int,
            Type::String => Type::String,
            Type::Bool => Type::Bool,
            Type::Fun(a, b) => Type::Fun(Box::new(a.apply_subst(s)), Box::new(b.apply_subst(s))),
            Type::Pair(a, b) => Type::Pair(Box::new(a.apply_subst(s)), Box::new(b.apply_subst(s))),
            Type::Poly(a, b) => {
                let mut a: HashSet<_> = a.iter().collect();
                for t in s.keys() {
                    a.remove(t);
                }
                if a.is_empty() {
                    b.apply_subst(s)
                } else {
                    Type::Poly(
                        a.iter().cloned().cloned().collect(),
                        Box::new(b.apply_subst(s)),
                    )
                }
            }
        }
    }
}

impl SpannedType {
    pub fn new_type(span: MySpan) -> Self {
        SpannedType::TyVar(new_type_id_name(), span)
    }

    pub fn apply_subst(&self, subst: &Subst) -> Self {
        match self {
            SpannedType::TyVar(x, span) => {
                if let Some(ty) = subst.get(x) {
                    ty.clone()
                } else {
                    self.clone()
                }
            }
            SpannedType::Fun(a, span1, b, span2) => SpannedType::Fun(
                Box::new(a.apply_subst(subst)),
                *span1,
                Box::new(b.apply_subst(subst)),
                *span2,
            ),
            SpannedType::Pair(a, span1, b, span2) => SpannedType::Pair(
                Box::new(a.apply_subst(subst)),
                *span1,
                Box::new(b.apply_subst(subst)),
                *span2,
            ),
            SpannedType::Poly(a, b, span) => {
                let mut a: HashSet<_> = a.iter().collect();
                for t in subst.keys() {
                    a.remove(t);
                }
                if a.is_empty() {
                    b.apply_subst(subst)
                } else {
                    SpannedType::Poly(
                        a.iter().cloned().cloned().collect(),
                        Box::new(b.apply_subst(subst)),
                        *span,
                    )
                }
            }
            _ => self.clone(),
        }
    }
    fn span(&self) -> MySpan {
        match self {
            SpannedType::TyVar(_, span)
            | SpannedType::Int(span)
            | SpannedType::String(span)
            | SpannedType::Bool(span)
            | SpannedType::Poly(_, _, span) => *span,
            SpannedType::Fun(_, span1, _, span2) | SpannedType::Pair(_, span1, _, span2) => {
                span1.union(*span2)
            }
        }
    }
    pub fn remove_span(self) -> Type {
        match self {
            SpannedType::TyVar(x, _) => Type::TyVar(x),
            SpannedType::Int(_) => Type::Int,
            SpannedType::String(_) => Type::String,
            SpannedType::Bool(_) => Type::Bool,
            SpannedType::Fun(ty1, _, ty2, _) => {
                Type::Fun(Box::new(ty1.remove_span()), Box::new(ty2.remove_span()))
            }
            SpannedType::Pair(ty1, _, ty2, _) => {
                Type::Pair(Box::new(ty1.remove_span()), Box::new(ty2.remove_span()))
            }
            SpannedType::Poly(tyvars, ty, _) => Type::Poly(tyvars, Box::new(ty.remove_span())),
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

pub type TypeEnvironment = HashMap<String, SpannedType>;
// TypeUtils.sml より
pub type Subst = HashMap<String, SpannedType>;

fn compose_subst(subst1: &Subst, subst2: &Subst) -> Subst {
    let subst2 = subst2
        .iter()
        .map(|(tid, ty)| (tid.to_owned(), ty.apply_subst(subst1)));
    let mut subst1 = subst1.clone();
    subst1.extend(subst2);
    subst1
}
fn subst_tyenv(subst: &Subst, tyenv: &TypeEnvironment) -> TypeEnvironment {
    tyenv
        .iter()
        .map(|(key, ty)| (key.clone(), ty.apply_subst(subst)))
        .collect()
}

fn fresh_inst(ty: SpannedType, span: MySpan) -> SpannedType {
    match ty {
        SpannedType::Poly(tids, ty, _) => {
            let s = tids.iter().rfold(Subst::new(), |mut s, tid| {
                let new_ty = SpannedType::new_type(span);
                s.insert(tid.to_owned(), new_ty);
                s
            });
            ty.apply_subst(&s)
        }
        _ => ty,
    }
}

// TypeInf.sml

pub fn type_inf(
    gamma: &TypeEnvironment,
    dec: flat_syntax::Dec,
) -> Result<(TypeEnvironment, TypedDeclaration), TypeInfErrors> {
    let flat_syntax::Dec::Val(id, exp) = dec;
    let span = exp.1;
    let (subst, ty, exp) = w(gamma, &exp)?;
    let tids = ftv(ty.clone()).iter().cloned().collect::<Vec<String>>();
    let exp = exp.apply_subst(&subst);
    // 型引数がすべて束縛されているならば,
    let new_ty = if tids.is_empty() {
        ty
    } else {
        SpannedType::Poly(tids, Box::new(ty), span)
    };

    let mut gamma = gamma.clone();
    gamma.insert(id.clone(), new_ty);
    Ok((gamma, TypedDeclaration::Val(id, exp)))
}
// UnifyTy.sml
fn ftv(ty: SpannedType) -> HashSet<String> {
    fn scan(ty: SpannedType, mut set: HashSet<String>) -> HashSet<String> {
        match ty {
            SpannedType::TyVar(var, _) => {
                set.insert(var);
                set
            }
            SpannedType::Fun(dom_ty, _, ran_ty, _) => scan(*ran_ty, scan(*dom_ty, set)),
            SpannedType::Pair(first_ty, _, second_ty, _) => scan(*second_ty, scan(*first_ty, set)),
            _ => set.clone(),
        }
    }
    scan(ty, HashSet::new())
}

pub enum UnifyError {
    OccurCheck,
    Mismatch(SpannedType, SpannedType),
}

fn rewrite(e: &[(SpannedType, SpannedType)], s: Subst) -> Result<Subst, UnifyError> {
    if let Some(((ty1, ty2), e)) = e.split_first() {
        if ty1 == ty2 {
            rewrite(e, s)
        } else {
            match (ty1, ty2) {
                (SpannedType::TyVar(tv, span1), _) => {
                    if occurs(ty1.clone(), ty2.clone()) {
                        Err(UnifyError::OccurCheck)
                    } else {
                        let mut s1 = HashMap::new();
                        s1.insert(tv.clone(), ty2.clone());
                        rewrite(
                            &e.iter()
                                .map(|(ty1, ty2)| (ty1.apply_subst(&s1), ty2.apply_subst(&s1)))
                                .collect::<Vec<_>>(),
                            compose_subst(&s1, &s),
                        )
                    }
                }
                (_, SpannedType::TyVar(_, _)) => {
                    let mut list = e.to_vec();
                    list.push((ty2.clone(), ty1.clone()));
                    rewrite(&list, s)
                }
                (
                    SpannedType::Fun(ty11, span11, ty12, span12),
                    SpannedType::Fun(ty21, span21, ty22, span22),
                ) => {
                    let mut list = e.to_vec();
                    list.push((*ty12.clone(), *ty22.clone()));
                    list.push((*ty11.clone(), *ty21.clone()));
                    rewrite(&list, s)
                }
                (
                    SpannedType::Pair(ty11, span11, ty12, span12),
                    SpannedType::Pair(ty21, span21, ty22, span22),
                ) => {
                    let mut list = e.to_vec();
                    list.push((*ty12.clone(), *ty22.clone()));
                    list.push((*ty11.clone(), *ty21.clone()));
                    rewrite(&list, s)
                }
                (ty1, ty2) => Err(UnifyError::Mismatch(ty1.clone(), ty2.clone())),
            }
        }
    } else {
        Ok(s)
    }
}

fn unify(list: &[(SpannedType, SpannedType)]) -> Result<Subst, UnifyError> {
    rewrite(list, Subst::new())
}

fn occurs(ty1: SpannedType, ty2: SpannedType) -> bool {
    match ty1 {
        SpannedType::TyVar(var, _span) => ftv(ty2).contains(&var),
        _ => false,
    }
}

// TypeInf.sml
pub fn w(
    gamma: &TypeEnvironment,
    exp: &Spanned<Exp>,
) -> Result<(Subst, SpannedType, TypedExp), TypeInfErrors> {
    let (exp, span) = exp;
    match exp {
        Exp::ExpId(var) => gamma
            .get(var)
            .ok_or(TypeInfErrors::VariableNotDefined((
                var.clone(),
                span.clone(),
            )))
            .map(|ty| {
                (
                    Subst::new(),
                    fresh_inst(ty.clone(), *span),
                    TypedExp::ExpId(var.clone(), ty.clone().remove_span()),
                )
            }),
        Exp::Int(x) => Ok((Subst::new(), SpannedType::Int(*span), TypedExp::Int(*x))),
        Exp::String(x) => Ok((
            Subst::new(),
            SpannedType::String(*span),
            TypedExp::String(x.clone()),
        )),
        Exp::True => Ok((Subst::new(), SpannedType::Bool(*span), TypedExp::True)),
        Exp::False => Ok((Subst::new(), SpannedType::Bool(*span), TypedExp::False)),
        Exp::ExpFn(x, exp) => {
            let ty1 = SpannedType::new_type(*span);
            let chiled_span = exp.1;
            let mut new_gamma = gamma.clone();
            new_gamma.insert(x.clone(), ty1.clone());
            let (s, ty2, exp) = w(&new_gamma, exp)?;
            let ty = SpannedType::Fun(
                Box::new(ty1.apply_subst(&s)),
                *span,
                Box::new(ty2),
                chiled_span,
            );
            Ok((
                s,
                ty.clone(),
                TypedExp::ExpFn(x.to_owned(), Box::new(exp), ty.remove_span()),
            ))
        }
        /*Fixed */
        Exp::ExpApp(exp1, exp2) => {
            let (s1, ty1, typed_exp1) = w(gamma, exp1)?;
            let (s2, ty2, typed_exp2) = w(&subst_tyenv(&s1, gamma), exp2)?;
            let ty3 = SpannedType::new_type(*span);
            let s3 = unify(&[(
                SpannedType::Fun(Box::new(ty2), exp1.1, Box::new(ty3.clone()), exp2.1),
                ty1.apply_subst(&s2),
            )])
            .map_err(|unify_err| match unify_err {
                UnifyError::OccurCheck => todo!(),
                UnifyError::Mismatch(ty1, ty2) => TypeInfErrors::NotExpectedType {
                    error_exp: *(exp1.clone()),
                    actual_ty: ty2,
                    ideal_ty: ty1,
                },
            })?;
            let s4 = compose_subst(&s3, &compose_subst(&s2, &s1));
            let ty = ty3.apply_subst(&s4);
            Ok((
                s4.clone(),
                ty.clone(),
                TypedExp::ExpApp(Box::new(typed_exp1), Box::new(typed_exp2), ty.remove_span()),
            ))
        }
        /*Ok */
        Exp::ExpPair(exp1, exp2) => {
            let (s1, ty1, typed_exp1) = w(gamma, exp1)?;
            let (s2, ty2, typed_exp2) = w(&subst_tyenv(&s1, gamma), exp2)?;
            let ty = SpannedType::Pair(
                Box::new(ty1.apply_subst(&s2)),
                exp1.1,
                Box::new(ty2),
                exp2.1,
            );
            Ok((
                compose_subst(&s2, &s1),
                ty.clone(),
                TypedExp::ExpPair(Box::new(typed_exp1), Box::new(typed_exp2), ty.remove_span()),
            ))
        }
        /*Ok */
        Exp::ExpProj1(exp) => {
            let (s1, ty, typed_exp) = w(gamma, exp)?;
            let ty1 = SpannedType::new_type(*span);
            let ty2 = SpannedType::new_type(*span);
            let s2 = unify(&[(
                ty,
                SpannedType::Pair(Box::new(ty1.clone()), *span, Box::new(ty2), *span),
            )])
            .map_err(|unify_err| match unify_err {
                UnifyError::OccurCheck => todo!(),
                UnifyError::Mismatch(ty1, ty2) => TypeInfErrors::NotExpectedType {
                    error_exp: *exp.clone(),
                    actual_ty: ty1,
                    ideal_ty: ty2,
                },
            })?;
            Ok((
                compose_subst(&s2, &s1),
                ty1.apply_subst(&s2),
                TypedExp::ExpProj1(Box::new(typed_exp), ty1.apply_subst(&s2).remove_span()),
            ))
        }
        /*Ok */
        Exp::ExpProj2(exp) => {
            let (s1, ty, typed_exp) = w(gamma, exp)?;
            let ty1 = SpannedType::new_type(*span);
            let ty2 = SpannedType::new_type(*span);
            let s2 = unify(&[(
                ty,
                SpannedType::Pair(Box::new(ty1), *span, Box::new(ty2.clone()), *span),
            )])
            .map_err(|unify_err| match unify_err {
                UnifyError::OccurCheck => todo!(),
                UnifyError::Mismatch(ty1, ty2) => TypeInfErrors::NotExpectedType {
                    error_exp: *exp.clone(),
                    actual_ty: ty1,
                    ideal_ty: ty2,
                },
            })?;
            Ok((
                compose_subst(&s2, &s1),
                ty2.apply_subst(&s2),
                TypedExp::ExpProj1(Box::new(typed_exp), ty2.apply_subst(&s2).remove_span()),
            ))
        }
        /*Fixed */
        Exp::ExpPrim(p, exp1, exp2) => {
            let (s1, ty1, typed_exp1) = w(gamma, exp1)?;
            let (s2, ty2, typed_exp2) = w(&subst_tyenv(&s1, gamma), exp2)?;
            let ty1_ = ty1.apply_subst(&s2);
            let s3 = unify(&[
                (ty1_.clone(), SpannedType::Int(exp1.1)),
                (ty2, SpannedType::Int(exp2.1)),
            ])
            .map_err(|unify_err| match unify_err {
                UnifyError::OccurCheck => todo!(),
                UnifyError::Mismatch(ty1, ty2) => TypeInfErrors::NotExpectedType {
                    error_exp: if ty1_ == ty1 {
                        *(exp1.clone())
                    } else {
                        *(exp2.clone())
                    },
                    actual_ty: ty1,
                    ideal_ty: ty2,
                },
            })?;
            let ty3 = if let Prim::Eq = p {
                SpannedType::Bool(*span)
            } else {
                SpannedType::Int(*span)
            };
            Ok((
                compose_subst(&s3, &compose_subst(&s2, &s1)),
                ty3.clone(),
                TypedExp::ExpPrim(
                    p.clone(),
                    Box::new(typed_exp1),
                    Box::new(typed_exp2),
                    ty3.remove_span(),
                ),
            ))
        }
        /*Ok */
        Exp::ExpIf(exp1, exp2, exp3) => {
            let (s1, ty1, typed_exp1) = w(gamma, exp1)?;
            let s2 = unify(&[(ty1, SpannedType::Bool(exp1.1))]).map_err(
                |unify_err| match unify_err {
                    UnifyError::OccurCheck => todo!(),
                    UnifyError::Mismatch(ty1, ty2) => TypeInfErrors::NotExpectedType {
                        error_exp: *(exp1.clone()),
                        actual_ty: ty1,
                        ideal_ty: ty2,
                    },
                },
            )?;
            let (s3, ty2, typed_exp2) = w(&subst_tyenv(&compose_subst(&s2, &s1), gamma), exp2)?;
            let (s4, ty3, typed_exp3) = w(
                &subst_tyenv(&compose_subst(&s3, &compose_subst(&s2, &s1)), gamma),
                exp3,
            )?;
            /*
            IF のどちらのブランチが理想な型を持つかはわからないため,とりあえず then のほうの型を理想とする
             */
            let s5 = unify(&[(ty2.clone(), ty3)]).map_err(|unify_err| match unify_err {
                UnifyError::OccurCheck => todo!(),
                UnifyError::Mismatch(ty1, ty2) => TypeInfErrors::NotExpectedType {
                    error_exp: *(exp3.clone()),
                    actual_ty: ty2,
                    ideal_ty: ty1,
                },
            })?;
            let s = compose_subst(
                &s5,
                &compose_subst(&s4, &compose_subst(&s3, &compose_subst(&s2, &s1))),
            );
            let _new_gamma = subst_tyenv(&s, gamma);
            Ok((
                s,
                ty2.apply_subst(&s5),
                TypedExp::ExpIf(
                    Box::new(typed_exp1),
                    Box::new(typed_exp2),
                    Box::new(typed_exp3),
                ),
            ))
        }
        /*OK */
        Exp::ExpFix(fid, xid, exp) => {
            let arg_ty = SpannedType::new_type(*span);
            let body_ty = SpannedType::new_type(exp.1);
            let fun_ty = SpannedType::Fun(
                Box::new(arg_ty.clone()),
                *span,
                Box::new(body_ty.clone()),
                exp.1,
            );
            let mut new_gamma = gamma.clone();
            new_gamma.insert(fid.clone(), fun_ty.clone());
            new_gamma.insert(xid.clone(), arg_ty);
            let (s1, ty, typed_exp) = w(&new_gamma, exp)?;
            let s2 = unify(&[(ty, body_ty)]).map_err(|unify_err| match unify_err {
                UnifyError::OccurCheck => todo!(),
                UnifyError::Mismatch(ty1, ty2) => TypeInfErrors::NotExpectedType {
                    error_exp: *(exp.clone()),
                    actual_ty: ty1,
                    ideal_ty: ty2,
                },
            })?;
            let s = compose_subst(&s2, &s1);
            Ok((
                s.clone(),
                fun_ty.apply_subst(&s),
                TypedExp::ExpFix(
                    fid.clone(),
                    xid.clone(),
                    Box::new(typed_exp),
                    fun_ty.apply_subst(&s).remove_span(),
                ),
            ))
        }
    }
}
