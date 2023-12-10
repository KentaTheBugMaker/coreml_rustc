//! In this phase program 's expression more simpler.
//!
//! K-Normalization.

use std::collections::BTreeMap;
use std::fmt::Display;
use std::sync::atomic::AtomicU64;

use crate::alpha_unique::var;
use crate::alpha_unique::Var;
use crate::closureconversion::CCValue;
use crate::closureconversion::ClosureLike;
use crate::closureconversion::FunID;
use crate::closureconversion::NCDeclaration;
use crate::closureconversion::NCExp;
use crate::closureconversion::ValueEnvironment;
use crate::flat_syntax::Prim;
use crate::typeinf::Type;

/// KNormalized Exp
#[derive(Debug, Clone)]
pub enum KNExp {
    Value(CCValue),
    // if exp1 contains free variables.
    ExpApp(CCValue, CCValue, Type),
    // if exp1 does not contains free variables.
    ExpCall(FunID, CCValue, Type),
    ExpPair(CCValue, CCValue, Type),
    ExpProj1(CCValue, Type),
    ExpProj2(CCValue, Type),
    ExpPrim(Prim, CCValue, CCValue, Type),
    ExpIf(CCValue, Box<KNExp>, Box<KNExp>),
    /// put closure in mem.
    ExpMkClosure(ValueEnvironment, FunID, Type),
    /// create local variable.
    /// and evaluate exp
    ExpLet(Vec<(Var, KNExp)>, Box<KNExp>, Type),
    /// extract and bind from env.
    /// very restricted version of SML #label
    ExpSelect(Var, Type),
    ///
    FPtr(FunID, Type),
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

impl Display for KNExp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            KNExp::Value(c) => write!(f, "{c}"),
            KNExp::ExpApp(exp1, exp2, ty) => write!(f, "app({exp1}({exp2})):{ty}"),
            KNExp::ExpCall(fun_id, exp, ty) => write!(f, "call({fun_id}({exp})):{ty}"),
            KNExp::ExpPair(exp1, exp2, ty) => write!(f, "({exp1},{exp2}):{ty}"),
            KNExp::ExpProj1(exp, _) => write!(f, "#1 {exp}"),
            KNExp::ExpProj2(exp, _) => write!(f, "#2 {exp}"),
            KNExp::ExpPrim(prim, exp1, exp2, _) => write!(f, "{prim}({exp1},{exp2})"),
            KNExp::ExpIf(exp1, exp2, exp3) => {
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
            KNExp::ExpMkClosure(env, fid, ty) => {
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
            KNExp::ExpLet(binds, inner, ty) => {
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
            KNExp::ExpSelect(var, _) => {
                write!(f, "#{var}")
            }
            KNExp::FPtr(f_id, ty) => write!(f, "fptr({f_id}:{ty})"),
        }
    }
}

pub enum KNClosureLike {
    Closure {
        /// this is closure environment.
        env: ValueEnvironment,
        /// this is args.
        args: Vec<(Var, Type)>,
        inner_exp: KNExp,
        ty: Type,
    },
    Function {
        /// this is args.
        args: Vec<(Var, Type)>,
        inner_exp: KNExp,
        ty: Type,
    },
}

impl Display for KNClosureLike {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            KNClosureLike::Closure {
                env: _,
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
            KNClosureLike::Function {
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

impl CCValue {
    fn ty(&self) -> Type {
        match self {
            CCValue::Var(_, ty) => ty.clone(),
            CCValue::Int(_) => Type::Int,
            CCValue::String(_) => Type::String,
            CCValue::True => Type::Bool,
            CCValue::False => Type::Bool,
        }
    }
}

impl KNExp {
    pub fn ty(&self) -> Type {
        match self {
            KNExp::Value(v) => v.ty(),
            KNExp::ExpApp(_, _, ty) => ty.clone(),
            KNExp::ExpCall(_, _, ty) => ty.clone(),
            KNExp::ExpPair(_, _, ty) => ty.clone(),
            KNExp::ExpProj1(_, ty) => ty.clone(),
            KNExp::ExpProj2(_, ty) => ty.clone(),
            KNExp::ExpPrim(_, _, _, ty) => ty.clone(),
            KNExp::ExpIf(_, _, e) => e.ty(),
            KNExp::ExpMkClosure(_, _, ty) => ty.clone(),
            KNExp::ExpLet(_, _, ty) => ty.clone(),
            KNExp::ExpSelect(_, ty) => ty.clone(),
            KNExp::FPtr(_, ty) => ty.clone(),
        }
    }
}

fn insert_let(exp: KNExp, inserter: impl FnOnce(CCValue) -> KNExp) -> KNExp {
    match exp {
        KNExp::Value(value) => inserter(value),
        x => {
            let new_var = var("".to_owned());
            let new_bind = CCValue::Var(new_var.clone(), x.ty());
            let transformed_exp = inserter(new_bind);
            let transformed_exp_ty = transformed_exp.ty();
            KNExp::ExpLet(
                vec![(new_var, x)],
                Box::new(transformed_exp),
                transformed_exp_ty,
            )
        }
    }
}

impl NCExp {
    pub fn knormalize(&self) -> KNExp {
        match self {
            NCExp::Value(v) => KNExp::Value(v.clone()),
            NCExp::ExpApp(exp1, exp2, ty) => {
                let exp1 = exp1.knormalize();
                let exp2 = exp2.knormalize();
                insert_let(exp1, |var1| {
                    insert_let(exp2, |var2| KNExp::ExpApp(var1, var2, ty.clone()))
                })
            }
            NCExp::ExpCall(f_id, exp, ty) => {
                let exp = exp.knormalize();
                insert_let(exp, |var| KNExp::ExpCall(f_id.clone(), var, ty.clone()))
            }
            NCExp::ExpPair(exp1, exp2, ty) => {
                let exp1 = exp1.knormalize();
                let exp2 = exp2.knormalize();
                insert_let(exp1, |var1| {
                    insert_let(exp2, |var2| KNExp::ExpPair(var1, var2, ty.clone()))
                })
            }
            NCExp::ExpProj1(exp, ty) => {
                let exp = exp.knormalize();
                insert_let(exp, |var| KNExp::ExpProj1(var, ty.clone()))
            }
            NCExp::ExpProj2(exp, ty) => {
                let exp = exp.knormalize();

                insert_let(exp, |var| KNExp::ExpProj2(var, ty.clone()))
            }
            NCExp::ExpPrim(prim, exp1, exp2, ty) => {
                let exp1 = exp1.knormalize();
                let exp2 = exp2.knormalize();
                insert_let(exp1, |var1| {
                    insert_let(exp2, |var2| {
                        KNExp::ExpPrim(prim.clone(), var1, var2, ty.clone())
                    })
                })
            }
            NCExp::ExpIf(exp1, exp2, exp3) => {
                let exp1 = exp1.knormalize();
                insert_let(exp1, |v| {
                    let exp2 = exp2.knormalize();
                    let exp3 = exp3.knormalize();
                    KNExp::ExpIf(v, Box::new(exp2), Box::new(exp3))
                })
            }
            NCExp::ExpMkClosure(env, f_id, ty) => {
                KNExp::ExpMkClosure(env.clone(), f_id.clone(), ty.clone())
            }
            NCExp::ExpLet(binds, exp, ty) => {
                let binds = binds
                    .iter()
                    .map(|bind| (bind.0.clone(), bind.1.knormalize()))
                    .collect();
                let exp = exp.knormalize();
                KNExp::ExpLet(binds, Box::new(exp), ty.clone())
            }
            NCExp::ExpSelect(v, ty) => KNExp::ExpSelect(v.clone(), ty.clone()),
            NCExp::FPtr(f_id, ty) => KNExp::FPtr(f_id.clone(), ty.clone()),
        }
    }
}

#[derive(Debug, Clone)]
pub enum KNDeclaration {
    Val(Var, KNExp),
}

impl Display for KNDeclaration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let KNDeclaration::Val(var, exp) = self;
        write!(f, "val {var} = {exp}")
    }
}

pub fn knormalize_decls(decls: Vec<NCDeclaration>) -> Vec<KNDeclaration> {
    decls
        .into_iter()
        .map(|decl| {
            let NCDeclaration::Val(v, exp) = decl;
            KNDeclaration::Val(v, exp.knormalize())
        })
        .collect()
}

pub fn knormalize_functions(
    functions: BTreeMap<FunID, ClosureLike>,
) -> BTreeMap<FunID, KNClosureLike> {
    functions
        .into_iter()
        .map(|(f_id, function)| {
            let function = match function {
                ClosureLike::Closure {
                    env,
                    args,
                    inner_exp,
                    ty,
                } => KNClosureLike::Closure {
                    env: env,
                    args: args,
                    inner_exp: inner_exp.knormalize(),
                    ty: ty,
                },
                ClosureLike::Function {
                    args,
                    inner_exp,
                    ty,
                } => KNClosureLike::Function {
                    args: args,
                    inner_exp: inner_exp.knormalize(),
                    ty: ty,
                },
            };
            (f_id, function)
        })
        .collect()
}
