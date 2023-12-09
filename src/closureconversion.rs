//! this module compile TypedExp to   

use std::{collections::BTreeMap, fmt::Display, sync::atomic::AtomicU64};

use crate::{
    alpha_unique::{AUDeclaration, AUExp, Var},
    syntax_tree::Prim,
    typeinf::Type,
};

#[derive(Debug, Clone, PartialEq, Eq)]
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

#[derive(Debug, Clone, PartialEq, Eq)]
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

#[derive(Debug, Clone, PartialEq, Eq)]
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
#[derive(Debug, Clone, PartialEq, Eq)]
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
    /// ExpMkClosure but NoEnvironment.
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

impl Display for NCExp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NCExp::Value(c) => write!(f, "{c}"),
            NCExp::ExpApp(exp1, exp2, ty) => write!(f, "app({exp1}({exp2})):{ty}"),
            NCExp::ExpCall(fun_id, exp, ty) => write!(f, "call({fun_id}({exp})):{ty}"),
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
            NCExp::ExpSelect(var, _) => {
                write!(f, "#{var}")
            }
            NCExp::FPtr(fid, ty) => write!(f, "fptr({fid}:{ty})"),
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
    mut direct_call: BTreeMap<Var, FunID>,   /*クロージャ呼び出しでない関数 */
) -> (NCExp, BTreeMap<FunID, ClosureLike>, BTreeMap<Var, FunID>) {
    let fv = free_vars(&auexp);
    match auexp {
        AUExp::ExpId(var, ty) => (
            NCExp::Value(CCValue::Var(var.clone(), ty.clone())),
            functions,
            direct_call,
        ),
        AUExp::Int(x) => (NCExp::Value(CCValue::Int(*x)), functions, direct_call),
        AUExp::String(x) => (
            NCExp::Value(CCValue::String(x.clone())),
            functions,
            direct_call,
        ),
        AUExp::True => (NCExp::Value(CCValue::True), functions, direct_call),
        AUExp::False => (NCExp::Value(CCValue::False), functions, direct_call),
        AUExp::ExpPair(exp1, exp2, ty) => {
            let (ncexp1, functions, direct_call) = closure_conversion(exp1, functions, direct_call);
            let (ncexp2, functions, direct_call) = closure_conversion(exp2, functions, direct_call);
            (
                NCExp::ExpPair(Box::new(ncexp1), Box::new(ncexp2), ty.clone()),
                functions,
                direct_call,
            )
        }
        AUExp::ExpProj1(exp, ty) => {
            let (ncexp, functions, direct_call) = closure_conversion(exp, functions, direct_call);
            (
                NCExp::ExpProj1(Box::new(ncexp), ty.clone()),
                functions,
                direct_call,
            )
        }
        AUExp::ExpProj2(exp, ty) => {
            let (ncexp, functions, direct_call) = closure_conversion(exp, functions, direct_call);
            (
                NCExp::ExpProj2(Box::new(ncexp), ty.clone()),
                functions,
                direct_call,
            )
        }
        AUExp::ExpPrim(prim, exp1, exp2, ty) => {
            let (ncexp1, functions, direct_call) = closure_conversion(exp1, functions, direct_call);
            let (ncexp2, functions, direct_call) = closure_conversion(exp2, functions, direct_call);
            (
                NCExp::ExpPrim(prim.clone(), Box::new(ncexp1), Box::new(ncexp2), ty.clone()),
                functions,
                direct_call,
            )
        }
        AUExp::ExpIf(exp1, exp2, exp3) => {
            let (ncexp1, functions, direct_call) = closure_conversion(exp1, functions, direct_call);
            let (ncexp2, functions, direct_call) = closure_conversion(exp2, functions, direct_call);
            let (ncexp3, functions, direct_call) = closure_conversion(exp3, functions, direct_call);
            (
                NCExp::ExpIf(Box::new(ncexp1), Box::new(ncexp2), Box::new(ncexp3)),
                functions,
                direct_call,
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

            let (inner, mut functions, direct_call) =
                closure_conversion(&inner, functions, direct_call);

            //自由変数があるならば関数内で束縛する.
            //環境によって与えられる変数があるならば,closure そうでないならばfunction.
            let function = if bindings.is_empty() {
                ClosureLike::Function {
                    args: vec![(x.clone(), *arg_ty)],
                    inner_exp: inner.clone(),
                    ty: ty.clone(),
                }
            } else {
                ClosureLike::Closure {
                    env: value_env.clone(),
                    args: vec![(x.clone(), *arg_ty)],
                    inner_exp: NCExp::ExpLet(bindings, Box::new(inner), ty.clone()),
                    ty: ty.clone(),
                }
            };

            (
                match &function {
                    ClosureLike::Closure {
                        env,
                        args: _,
                        inner_exp: _,
                        ty,
                    } => NCExp::ExpMkClosure(env.clone(), fun_id, ty.clone()),
                    ClosureLike::Function {
                        args: _,
                        inner_exp: _,
                        ty,
                    } => NCExp::FPtr(fun_id, ty.clone()),
                },
                {
                    functions.insert(fun_id.clone(), function);
                    functions
                },
                direct_call,
            )
        }
        AUExp::ExpApp(exp1, exp2, ty) => {
            let (ncexp1, functions, direct_call) = closure_conversion(exp1, functions, direct_call);
            let (ncexp2, functions, direct_call) = closure_conversion(exp2, functions, direct_call);

            match ncexp1 {
                NCExp::Value(CCValue::Var(ref var, _)) => {
                    // もし変数がdirect_call に登録されているならば,
                    if let Some(fid) = direct_call.get(var) {
                        (
                            NCExp::ExpCall(fid.clone(), Box::new(ncexp2), ty.clone()),
                            functions,
                            direct_call,
                        )
                    } else {
                        (
                            NCExp::ExpApp(Box::new(ncexp1), Box::new(ncexp2), ty.clone()),
                            functions,
                            direct_call,
                        )
                    }
                }
                NCExp::FPtr(fun_id, _) => (
                    NCExp::ExpCall(fun_id, Box::new(ncexp2), ty.clone()),
                    functions,
                    direct_call,
                ),
                ncexp1 => (
                    NCExp::ExpApp(Box::new(ncexp1), Box::new(ncexp2), ty.clone()),
                    functions,
                    direct_call,
                ),
            }
        }
        AUExp::ExpFix(f, x, inner, ty) => {
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

            // inner の　bindings が f しか含まないならば,
            // inner の f は関数ポインタに落とすことができて,
            if bindings.is_empty() | (value_env.0 == vec![(f.clone(), ty.clone())]) {
                direct_call.insert(f.clone(), fun_id.clone());
            }
            let (inner, mut functions, direct_call) =
                closure_conversion(&inner, functions, direct_call);
            // 自由変数があるならば関数内で束縛する.
            // 何も束縛しない場合も関数.
            // 自分以外の何かを束縛するときはクロージャ
            let function = if bindings.is_empty() | (value_env.0 == vec![(f.clone(), ty.clone())]) {
                ClosureLike::Function {
                    args: vec![(x.clone(), *arg_ty)],
                    inner_exp: inner,
                    ty: ty.clone(),
                }
            } else {
                ClosureLike::Closure {
                    env: value_env.clone(),
                    args: vec![(x.clone(), *arg_ty)],
                    inner_exp: NCExp::ExpLet(bindings.clone(), Box::new(inner), ty.clone()),
                    ty: ty.clone(),
                }
            };
            (
                match &function {
                    ClosureLike::Closure {
                        env,
                        args: _,
                        inner_exp: _,
                        ty,
                    } => NCExp::ExpMkClosure(env.clone(), fun_id, ty.clone()),
                    ClosureLike::Function {
                        args: _,
                        inner_exp: _,
                        ty,
                    } => NCExp::FPtr(fun_id, ty.clone()),
                },
                {
                    functions.insert(fun_id.clone(), function);
                    functions
                },
                direct_call,
            )
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NCDeclaration {
    Val(Var, NCExp),
}

impl Display for NCDeclaration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let NCDeclaration::Val(var, exp) = self;
        write!(f, "val {var} = {exp}")
    }
}

fn closure_conversion_decl(
    au_decl: AUDeclaration,
    functions: BTreeMap<FunID, ClosureLike>,
    direct_call: BTreeMap<Var, FunID>,
) -> (
    NCDeclaration,
    BTreeMap<FunID, ClosureLike>,
    BTreeMap<Var, FunID>,
) {
    let AUDeclaration::Val(var, auexp) = au_decl;
    let (ncexp, functions, mut direct_call) = closure_conversion(&auexp, functions, direct_call);
    if let NCExp::ExpMkClosure(e, fid, ty) = &ncexp {
        if e.0.is_empty() {
            println!("{var} is direct call");
            direct_call.insert(var.clone(), fid.clone());
        } else if (e.0.len() == 1) & (e.0 == vec![(var.clone(), ty.clone())]) {
            println!("{var} is direct call");
            direct_call.insert(var.clone(), fid.clone());
        }
    }

    (NCDeclaration::Val(var, ncexp), functions, direct_call)
}

pub fn closure_conversion_decls(
    mut au_decls: Vec<AUDeclaration>,
) -> (Vec<NCDeclaration>, BTreeMap<FunID, ClosureLike>) {
    let (decls, functions, _) = au_decls.drain(..).fold(
        (vec![], BTreeMap::new(), BTreeMap::new()),
        |(mut nc_decls, functions, direct_call), au_decl| {
            let (nc_decl, functions, direct_call) =
                closure_conversion_decl(au_decl, functions, direct_call);
            nc_decls.push(nc_decl);
            (nc_decls, functions, direct_call)
        },
    );
    (decls, functions)
}
