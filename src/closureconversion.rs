//! this module compile TypedExp to   

use std::{collections::BTreeMap, fmt::Display, sync::atomic::AtomicU64};

use crate::{
    alpha_unique::{AUDeclaration, AUExp, Var},
    syntax_tree::Prim,
    typeinf::Type,
};

#[derive(Debug, Clone)]
pub struct ValueEnvironment(pub Vec<(Var, Type)>);

fn free_vars(exp: &AUExp) -> BTreeMap<Var, Type> {
    match exp {
        AUExp::ExpId(x, ty) => [(x.to_owned(), ty.to_owned())].into_iter().collect(),
        AUExp::Int(_) => BTreeMap::new(),
        AUExp::String(_) => BTreeMap::new(),
        AUExp::True => BTreeMap::new(),
        AUExp::False => BTreeMap::new(),
        AUExp::ExpFn(bound, exp, _) => {
            let mut fvs = free_vars(exp);
            fvs.remove(bound);
            fvs
        }
        AUExp::ExpApp(exp1, exp2, _) => {
            let mut fvs1 = free_vars(exp1);
            let fvs2 = free_vars(exp2);
            fvs1.extend(fvs2);
            fvs1
        }
        AUExp::ExpPair(exp1, exp2, _) => {
            let mut fvs1 = free_vars(exp1);
            let fvs2 = free_vars(exp2);
            fvs1.extend(fvs2);
            fvs1
        }
        AUExp::ExpProj1(exp, _) => free_vars(exp),
        AUExp::ExpProj2(exp, _) => free_vars(exp),
        AUExp::ExpPrim(_, exp1, exp2, _) => {
            let mut fvs1 = free_vars(exp1);
            let fvs2 = free_vars(exp2);
            fvs1.extend(fvs2);
            fvs1
        }
        AUExp::ExpIf(exp1, exp2, exp3) => {
            let mut fvs1 = free_vars(exp1);
            let fvs2 = free_vars(exp2);
            let fvs3 = free_vars(exp3);
            fvs1.extend(fvs2);
            fvs1.extend(fvs3);
            fvs1
        }
        AUExp::ExpFix(_, bound2, exp, _) => {
            let mut fvs = free_vars(exp);
            fvs.remove(bound2);
            fvs
        }
    }
}

#[derive(Debug, Clone)]
pub enum ClosureLike {
    Closure {
        /// this is closure environment.
        env: ValueEnvironment,
        /// this is args.
        args: Vec<(Var, Type)>,
        inner_exp: NCExp,
        ty: Type,
    },
    Function {
        /// this is args.
        args: Vec<(Var, Type)>,
        inner_exp: NCExp,
        ty: Type,
    },
}

impl Display for ClosureLike {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ClosureLike::Closure {
                env,
                args,
                inner_exp,
                ty,
            } => {
                write!(f, "(",).ok();
                for arg in args {
                    write!(f, "{}:{}", arg.0, arg.1).ok();
                }
                writeln!(f, "):{ty}").ok();
                writeln!(f, "{inner_exp}")
            }
            ClosureLike::Function {
                args,
                inner_exp,
                ty,
            } => {
                write!(f, "(",).ok();
                for arg in args {
                    write!(f, "{}:{}", arg.0, arg.1).ok();
                }
                writeln!(f, "):{ty}").ok();
                writeln!(f, "{inner_exp}")
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum CCValue {
    Var(Var, Type),
    Int(i64),
    String(String),
    True,
    False,
}

impl Display for CCValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CCValue::Var(id, ty) => {
                write!(f, "{id:}:{ty:}")
            }
            CCValue::Int(x) => {
                write!(f, "{x:}")
            }
            CCValue::String(x) => {
                write!(f, "{x:}")
            }
            CCValue::True => {
                write!(f, "true")
            }
            CCValue::False => {
                write!(f, "false")
            }
        }
    }
}

/// convert closure to function that take (environment , arg)
/// when closure created enviroment and function ptr will created.
///   
#[derive(Debug, Clone)]
pub enum NCExp {
    Value(CCValue),
    // if exp1 contains free variables.
    ExpApp(Box<NCExp>, Box<NCExp>, Type),
    // if exp1 does not contains free variables.
    ExpCall(FunID, Box<NCExp>, Type),
    ExpPair(Box<NCExp>, Box<NCExp>, Type),
    ExpProj1(Box<NCExp>, Type),
    ExpProj2(Box<NCExp>, Type),
    ExpPrim(Prim, Box<NCExp>, Box<NCExp>, Type),
    ExpIf(Box<NCExp>, Box<NCExp>, Box<NCExp>),
    /// put closure in mem.
    ExpMkClosure(ValueEnvironment, FunID, Type),
    /// create local variable.
    /// and evaluate exp
    ExpLet(Vec<(Var, NCExp)>, Box<NCExp>, Type),
    /// extract and bind from env.
    /// very restricted version of SML #label
    ExpSelect(Var, Type),
}

static INDENT_LEVEL: AtomicU64 = AtomicU64::new(1);

fn indent() -> u64 {
    INDENT_LEVEL.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
    INDENT_LEVEL.load(std::sync::atomic::Ordering::SeqCst)
}

fn dedent() -> u64 {
    INDENT_LEVEL.fetch_sub(1, std::sync::atomic::Ordering::SeqCst);
    INDENT_LEVEL.load(std::sync::atomic::Ordering::SeqCst)
}

impl Display for NCExp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NCExp::Value(c) => write!(f, "{c}"),
            NCExp::ExpApp(exp1, exp2, ty) => write!(f, "{exp1}({exp2}):{ty}"),
            NCExp::ExpCall(fun_id, exp, ty) => write!(f, "{fun_id}({exp}):{ty}"),
            NCExp::ExpPair(exp1, exp2, ty) => write!(f, "({exp1},{exp2}):{ty}"),
            NCExp::ExpProj1(exp, _) => write!(f, "#1 {exp}"),
            NCExp::ExpProj2(exp, _) => write!(f, "#2 {exp}"),
            NCExp::ExpPrim(prim, exp1, exp2, _) => write!(f, "{prim}({exp1},{exp2})"),
            NCExp::ExpIf(exp1, exp2, exp3) => {
                writeln!(f, "if {exp1} then").ok();
                let level = indent();
                for _ in 0..level {
                    write!(f, "    ").ok();
                }
                writeln!(f, "{exp2}").ok();
                let level = dedent();
                for _ in 0..level {
                    write!(f, "    ").ok();
                }
                writeln!(f, "else").ok();
                let level_exp3 = indent();
                for _ in 0..level_exp3 {
                    write!(f, "    ").ok();
                }
                let result = write!(f, "{exp3}");
                dedent();
                result
            }
            NCExp::ExpMkClosure(env, fid, ty) => {
                let env_name = format!("env_{fid}");
                write!(f, "(").ok();
                {
                    let mut debug_struct = f.debug_struct(&env_name);
                    env.0
                        .iter()
                        .fold(&mut debug_struct, |f, (var, ty)| {
                            f.field(&var.to_string(), &ty.to_string())
                        })
                        .finish()
                        .ok();
                }
                write!(f, "):{ty}")
            }
            NCExp::ExpLet(binds, inner, ty) => {
                writeln!(f, "let").ok();
                let level = indent();

                for (var, exp) in binds {
                    for _ in 0..level {
                        write!(f, "    ").ok();
                    }
                    writeln!(f, "val {var} = {exp}").ok();
                }
                let level = dedent();
                for _ in 0..level {
                    write!(f, "    ").ok();
                }
                writeln!(f, "in").ok();
                let level = indent();
                for _ in 0..level {
                    write!(f, "    ").ok();
                }
                writeln!(f, "{inner}").ok();
                let level = dedent();
                for _ in 0..level {
                    write!(f, "    ").ok();
                }
                write!(f, "end:{ty}")
            }
            NCExp::ExpSelect(var, ty) => {
                write!(f, "#{var}")
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct FunID(u64);

impl Display for FunID {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "lambda_{}", self.0)
    }
}

static FUN_ID: AtomicU64 = AtomicU64::new(0);

fn new_fun_id() -> FunID {
    FunID(FUN_ID.fetch_add(1, std::sync::atomic::Ordering::SeqCst))
}

pub fn closure_conversion(
    auexp: &AUExp,                           /*クロージャ変換の対象 */
    functions: BTreeMap<FunID, ClosureLike>, /*クロージャ変換によってできた関数.*/
) -> (NCExp, BTreeMap<FunID, ClosureLike>) {
    let fv = free_vars(&auexp);
    match auexp {
        AUExp::ExpId(var, ty) => (
            NCExp::Value(CCValue::Var(var.clone(), ty.clone())),
            functions,
        ),
        AUExp::Int(x) => (NCExp::Value(CCValue::Int(*x)), functions),
        AUExp::String(x) => (NCExp::Value(CCValue::String(x.clone())), functions),
        AUExp::True => (NCExp::Value(CCValue::True), functions),
        AUExp::False => (NCExp::Value(CCValue::False), functions),
        AUExp::ExpPair(exp1, exp2, ty) => {
            let (ncexp1, functions) = closure_conversion(exp1, functions);
            let (ncexp2, functions) = closure_conversion(exp2, functions);
            (
                NCExp::ExpPair(Box::new(ncexp1), Box::new(ncexp2), ty.clone()),
                functions,
            )
        }
        AUExp::ExpProj1(exp, ty) => {
            let (ncexp, functions) = closure_conversion(exp, functions);
            (NCExp::ExpProj1(Box::new(ncexp), ty.clone()), functions)
        }
        AUExp::ExpProj2(exp, ty) => {
            let (ncexp, functions) = closure_conversion(exp, functions);
            (NCExp::ExpProj2(Box::new(ncexp), ty.clone()), functions)
        }
        AUExp::ExpPrim(prim, exp1, exp2, ty) => {
            let (ncexp1, functions) = closure_conversion(exp1, functions);
            let (ncexp2, functions) = closure_conversion(exp2, functions);
            (
                NCExp::ExpPrim(prim.clone(), Box::new(ncexp1), Box::new(ncexp2), ty.clone()),
                functions,
            )
        }
        AUExp::ExpIf(exp1, exp2, exp3) => {
            let (ncexp1, functions) = closure_conversion(exp1, functions);
            let (ncexp2, functions) = closure_conversion(exp2, functions);
            let (ncexp3, functions) = closure_conversion(exp3, functions);
            (
                NCExp::ExpIf(Box::new(ncexp1), Box::new(ncexp2), Box::new(ncexp3)),
                functions,
            )
        }
        AUExp::ExpFn(x, inner, ty) => {
            let fun_id = new_fun_id();
            let args: Vec<(Var, Type)> = fv
                .iter()
                .map(|(var, ty)| (var.clone(), ty.clone()))
                .collect();
            let bindings: Vec<(Var, NCExp)> = args
                .iter()
                .map(|(var, ty)| (var.clone(), NCExp::ExpSelect(var.clone(), ty.clone())))
                .collect();
            let value_env = ValueEnvironment(args);
            let arg_ty = match ty {
                Type::Fun(arg_ty, _) => arg_ty.clone(),
                Type::Poly(_, ty) => match ty.as_ref() {
                    Type::Fun(arg_ty, _) => arg_ty.clone(),
                    _ => unreachable!("this case will not come here"),
                },
                _ => unreachable!("this case will not come here"),
            };
            let (inner, mut functions) = closure_conversion(&inner, functions);

            let inner_exp = if bindings.is_empty() {
                inner.clone()
            } else {
                NCExp::ExpLet(bindings, Box::new(inner), ty.clone())
            };
            let function = ClosureLike::Closure {
                env: value_env.clone(),
                args: vec![(x.clone(), *arg_ty)],
                inner_exp,
                ty: ty.clone(),
            };
            functions.insert(fun_id.clone(), function);
            (
                NCExp::ExpMkClosure(value_env, fun_id, ty.clone()),
                functions,
            )
        }
        AUExp::ExpApp(exp1, exp2, ty) => {
            let (ncexp1, functions) = closure_conversion(exp1, functions);
            let (ncexp2, functions) = closure_conversion(exp2, functions);
            (
                NCExp::ExpApp(Box::new(ncexp1), Box::new(ncexp2), ty.clone()),
                functions,
            )
        }
        AUExp::ExpFix(_, x, inner, ty) => {
            let fun_id = new_fun_id();
            let args: Vec<(Var, Type)> = fv
                .iter()
                .map(|(var, ty)| (var.clone(), ty.clone()))
                .collect();
            let bindings: Vec<(Var, NCExp)> = args
                .iter()
                .map(|(var, ty)| (var.clone(), NCExp::ExpSelect(var.clone(), ty.clone())))
                .collect();
            let value_env = ValueEnvironment(args);
            let arg_ty = match ty {
                Type::Fun(arg_ty, _) => arg_ty.clone(),
                Type::Poly(_, ty) => match ty.as_ref() {
                    Type::Fun(arg_ty, _) => arg_ty.clone(),
                    _ => unreachable!("this case will not come here"),
                },
                _ => unreachable!("this case will not come here"),
            };
            let (inner, mut functions) = closure_conversion(&inner, functions);
            let inner_exp = if bindings.is_empty() {
                inner.clone()
            } else {
                NCExp::ExpLet(bindings, Box::new(inner), ty.clone())
            };
            let function = ClosureLike::Closure {
                env: value_env.clone(),
                args: vec![(x.clone(), *arg_ty)],
                inner_exp,
                ty: ty.clone(),
            };
            functions.insert(fun_id.clone(), function);
            (
                NCExp::ExpMkClosure(value_env, fun_id, ty.clone()),
                functions,
            )
        }
    }
}

#[derive(Debug, Clone)]
pub enum NCDeclaration {
    Val(Var, NCExp),
}

impl Display for NCDeclaration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let NCDeclaration::Val(var, exp) = self;
        write!(f, "val {var} = {exp}")
    }
}

/*
    Optimize ExpApp that's exp1 is not a closure.
    ExpApp -> ExpCall
*/
fn optimize_closure(
    exp: &NCExp,
    functions: &BTreeMap<FunID, ClosureLike>,
    declarations: &[NCDeclaration],
) -> NCExp {
    match exp {
        NCExp::Value(_) => exp.clone(),
        NCExp::ExpCall(fid, exp, ty) => NCExp::ExpCall(
            fid.clone(),
            Box::new(optimize_closure(exp, functions, declarations)),
            ty.clone(),
        ),
        NCExp::ExpPair(exp1, exp2, ty) => NCExp::ExpPair(
            Box::new(optimize_closure(exp1, functions, declarations)),
            Box::new(optimize_closure(exp2, functions, declarations)),
            ty.clone(),
        ),
        NCExp::ExpProj1(exp, ty) => NCExp::ExpProj1(
            Box::new(optimize_closure(exp, functions, declarations)),
            ty.clone(),
        ),
        NCExp::ExpProj2(exp, ty) => NCExp::ExpProj2(
            Box::new(optimize_closure(exp, functions, declarations)),
            ty.clone(),
        ),
        NCExp::ExpPrim(prim, exp1, exp2, ty) => NCExp::ExpPrim(
            prim.clone(),
            Box::new(optimize_closure(exp1, functions, declarations)),
            Box::new(optimize_closure(exp2, functions, declarations)),
            ty.clone(),
        ),
        NCExp::ExpIf(exp1, exp2, exp3) => NCExp::ExpIf(
            Box::new(optimize_closure(exp1, functions, declarations)),
            Box::new(optimize_closure(exp2, functions, declarations)),
            Box::new(optimize_closure(exp3, functions, declarations)),
        ),
        NCExp::ExpApp(_, _, _) => todo!(),
        NCExp::ExpMkClosure(env, fid, ty) => todo!(),
        NCExp::ExpLet(_, _, _) => todo!(),
        NCExp::ExpSelect(_, _) => todo!(),
    }
}

fn closure_conversion_decl(
    au_decl: AUDeclaration,
    functions: BTreeMap<FunID, ClosureLike>,
) -> (NCDeclaration, BTreeMap<FunID, ClosureLike>) {
    let AUDeclaration::Val(var, auexp) = au_decl;
    let (ncexp, functions) = closure_conversion(&auexp, functions);
    (NCDeclaration::Val(var, ncexp), functions)
}

pub fn closure_conversion_decls(
    mut au_decls: Vec<AUDeclaration>,
) -> (Vec<NCDeclaration>, BTreeMap<FunID, ClosureLike>) {
    au_decls.drain(..).fold(
        (vec![], BTreeMap::new()),
        |(mut nc_decls, functions), au_decl| {
            let (nc_decl, functions) = closure_conversion_decl(au_decl, functions);
            nc_decls.push(nc_decl);
            (nc_decls, functions)
        },
    )
}
