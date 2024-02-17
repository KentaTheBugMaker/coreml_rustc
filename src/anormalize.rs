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
use crate::flat_syntax::Prim;
use crate::typeinf::Type;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AnType {
    TyVar(String),
    Int,
    String,
    Bool,
    Fun(Box<AnType>, Box<AnType>),
    Record(BTreeMap<String, AnType>),
    Poly(Vec<String>, Box<AnType>),
}

impl Into<AnType> for Type {
    fn into(self) -> AnType {
        match self {
            Type::TyVar(x) => AnType::TyVar(x),
            Type::Int => AnType::Int,
            Type::String => AnType::String,
            Type::Bool => AnType::Bool,
            Type::Fun(arg, ret) => AnType::Fun(Box::new((*arg).into()), Box::new((*ret).into())),
            Type::Pair(ty1, ty2) => AnType::Record(
                [
                    ("1".to_owned(), (*ty1).into()),
                    ("2".to_owned(), (*ty2).into()),
                ]
                .into_iter()
                .collect(),
            ),
            Type::Poly(tyvars, ty) => AnType::Poly(tyvars, Box::new((*ty).into())),
        }
    }
}
impl Display for AnType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AnType::TyVar(a) => write!(f, "{a}"),
            AnType::Int => write!(f, "int"),
            AnType::String => write!(f, "string"),
            AnType::Bool => write!(f, "bool"),
            AnType::Fun(ty1, ty2) => write!(f, "({ty1:} -> {ty2:})"),
            AnType::Record(x) => write!(f, "{:?}", x),
            AnType::Poly(type_ids, ty) => write!(
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
/// KNormalized Exp
#[derive(Debug, Clone)]
pub enum ANExp {
    // if exp1 does not contains free variables.
    ExpCall {
        result_var: Var,
        args: Vec<AnValue>,
        next_exp: Box<ANExp>,
        ty: AnType,
    },
    ExpRecord {
        result_var: Var,
        fields: BTreeMap<String, AnValue>,
        ty: AnType,
        next_exp: Box<ANExp>,
    },
    ExpPrim {
        prim: Prim,
        exp_1: AnValue,
        exp_2: AnValue,
        next_exp: Box<ANExp>,
        ty: AnType,
    },
    ExpIf {
        cond: AnValue,
        then_: Box<ANExp>,
        else_: Box<ANExp>,
    },
    /// extract and bind from env.
    /// very restricted version of SML #label
    ExpSelect {
        exp: AnValue,
        label: String,
        ty: AnType,
        result_var: Var,
        next_exp: Box<ANExp>,
    },
}
impl Display for ANExp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
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

pub struct ANFunction {
    /// this is args.
    args: Vec<(Var, AnType)>,
    inner_exp: ANExp,
    ty: Type,
}

impl Display for ANFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let ANFunction {
            args,
            inner_exp,
            ty,
        } = self;
        {
            write!(f, "(",).ok();
            for arg in args {
                write!(f, "{}:{}", arg.0, arg.1).ok();
            }
            writeln!(f, "):{ty}").ok();
            writeln!(f, "{inner_exp}")
        }
    }
}
#[derive(Debug, Clone)]
enum AnValue {
    Var(Var, AnType),
    Int(i64),
    String(String),
    FPtr(FunID),
    True,
    False,
    Bottom,
}

impl ANExp {
    pub fn ty(&self) -> AnType {
        match self {
            ANExp::ExpSelect {
                exp,
                label,
                ty,
                result_var,
                next_exp,
            } => ty.clone(),
            ANExp::ExpRecord {
                result_var,
                fields,
                ty,
                next_exp,
            } => ty.clone(),
            ANExp::ExpCall {
                result_var,
                args,
                next_exp,
                ty,
            } => ty.clone(),
            ANExp::ExpIf { cond, then_, else_ } => else_.ty(),
            ANExp::ExpPrim {
                prim,
                exp_1,
                exp_2,
                next_exp,
                ty,
            } => ty.clone(),
        }
    }
}

fn empty_proc(x: ANExp) -> ANExp {
    x
}

fn compile_value(v: CCValue) -> AnValue {
    match v {
        CCValue::Var(v, ty) => AnValue::Var(v, ty.into()),
        CCValue::Int(x) => AnValue::Int(x),
        CCValue::String(x) => AnValue::String(x),
        CCValue::True => AnValue::True,
        CCValue::False => AnValue::False,
    }
}

impl NCExp {
    pub fn anormalize(&self) -> (Box<dyn Fn(ANExp) -> ANExp>, AnValue) {
        match self {
            NCExp::Value(v) => (Box::new(|x| empty_proc(x)), compile_value(v.clone())),
            NCExp::ExpApp(exp1, exp2, ty) => {
                let (proc1, v1) = exp1.anormalize();
                let (proc2, v2) = exp2.anormalize();
                let v = var("".to_owned());
                let ty: AnType = ty.to_owned().into();

                (
                    Box::new(|k| {
                        proc1(proc2(ANExp::ExpCall {
                            result_var: v.clone(),
                            args: vec![v1, v2],
                            next_exp: Box::new(k),
                            ty: ty.clone(),
                        }))
                    }),
                    AnValue::Var(v, ty),
                )
            }
            NCExp::ExpCall(f_id, exp, ty) => {
                let (proc, exp) = exp.anormalize();
                let code_exp = AnValue::FPtr(*f_id);
                let v = var("".to_owned());
                let ty: AnType = ty.to_owned().into();
                (
                    Box::new(|k| ANExp::ExpCall {
                        result_var: v.clone(),
                        args: vec![exp],
                        next_exp: Box::new(k),
                        ty: ty.clone(),
                    }),
                    AnValue::Var(v, ty),
                )
            }
            NCExp::ExpPair(exp1, exp2, ty) => {
                let (proc1, exp1) = exp1.anormalize();
                let (proc2, exp2) = exp2.anormalize();
                let v = var("".to_owned());
                let ty: AnType = ty.to_owned().into();
                (
                    Box::new(|k| {
                        proc1(proc2(ANExp::ExpRecord {
                            result_var: v.clone(),
                            fields: [("1".to_owned(), exp1), ("2".to_owned(), exp2)]
                                .into_iter()
                                .collect(),
                            ty: ty.clone(),
                            next_exp: Box::new(k),
                        }))
                    }),
                    AnValue::Var(v, ty),
                )
            }
            NCExp::ExpProj1(exp, ty) => {
                let (proc, exp) = exp.anormalize();
                let ty: AnType = ty.clone().into();
                let v = var("".to_owned());
                (
                    Box::new(|k| {
                        proc(ANExp::ExpSelect {
                            exp,
                            label: "1".to_owned(),
                            ty: ty.clone(),
                            result_var: v.clone(),
                            next_exp: Box::new(k),
                        })
                    }),
                    AnValue::Var(v, ty),
                )
            }
            NCExp::ExpProj2(exp, ty) => {
                let (proc, exp) = exp.anormalize();
                let ty: AnType = ty.clone().into();
                let v = var("".to_owned());
                (
                    Box::new(|k| {
                        proc(ANExp::ExpSelect {
                            exp,
                            label: "2".to_owned(),
                            ty: ty.clone(),
                            result_var: v.clone(),
                            next_exp: Box::new(k),
                        })
                    }),
                    AnValue::Var(v, ty),
                )
            }
            NCExp::ExpPrim(prim, exp1, exp2, ty) => {
                let (proc1, exp1) = exp1.anormalize();
                let (proc2, exp2) = exp2.anormalize();
                let v = var("".to_owned());
                let ty: AnType = ty.to_owned().into();
                (
                    Box::new(|k| {
                        proc1(proc2(ANExp::ExpPrim {
                            prim: prim.clone(),
                            exp_1: exp1,
                            exp_2: exp2,
                            next_exp: Box::new(k),
                            ty: ty.clone(),
                        }))
                    }),
                    AnValue::Var(v, ty),
                )
            }
            NCExp::ExpIf(exp1, exp2, exp3) => {
                let (proc1, v1) = exp1.anormalize();
                let (proc2, v2) = exp2.anormalize();
                let (proc3, v3) = exp3.anormalize();
                (
                    Box::new(|k| {
                        proc1(ANExp::ExpIf {
                            cond: v1,
                            then_: Box::new(proc2(k.clone())),
                            else_: Box::new(proc3(k)),
                        })
                    }),
                    AnValue::Bottom,
                )
            }
            NCExp::ExpMkClosure(env, f_id, ty) => {
                let env_var = var("env".to_owned());
                let ty = AnType::Record(
                    env.0
                        .iter()
                        .enumerate()
                        .map(|(id, (var, ty))| (id.to_string(), (ty.clone()).into()))
                        .collect(),
                );

                let fields = env
                    .0
                    .iter()
                    .enumerate()
                    .map(|(id, (var, ty))| {
                        (id.to_string(), AnValue::Var(var.clone(), ty.clone().into()))
                    })
                    .collect();
                (
                    Box::new(|k| ANExp::ExpRecord {
                        result_var: env_var,
                        fields,
                        ty: ty.clone(),
                        next_exp: Box::new(k),
                    }),
                    AnValue::Var(env_var, ty),
                )
            }
            NCExp::ExpLet(binds, exp, ty) => {
                let proc1 = binds
                    .iter()
                    .map(|bind| bind.1.anormalize().0)
                    .reduce(|proc1, proc2| Box::new(|k| proc1(proc2(k))))
                    .unwrap();
                let (proc2, ret) = exp.anormalize();

                (Box::new(|k| proc1(proc2(k))), ret)
            }
            NCExp::ExpSelect(label, v, ty) => {
                let v = var("".to_owned());
                (
                    Box::new(|k| ANExp::ExpSelect {
                        exp: todo!(),
                        label: label.to_owned(),
                        ty: ty.clone().into(),
                        result_var: v,
                        next_exp: Box::new(k),
                    }),
                    AnValue::Var(v, ty.clone().into()),
                )
            }
            NCExp::FPtr(f_id, ty) => (Box::new(|x| empty_proc(x)), AnValue::FPtr(*f_id)),
        }
    }
}

#[derive(Debug, Clone)]
pub enum KNDeclaration {
    Val(Var, ANExp),
}

impl Display for KNDeclaration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let KNDeclaration::Val(var, exp) = self;
        write!(f, "val {var} = {exp}")
    }
}

pub fn anormalize_decls(decls: Vec<NCDeclaration>) -> Vec<Declaration> {
    decls
        .into_iter()
        .map(|decl| {
            let NCDeclaration::Val(v, exp) = decl;
            KNDeclaration::Val(v, exp.anormalize())
        })
        .collect()
}

pub fn anormalize_functions(
    functions: BTreeMap<FunID, ClosureLike>,
) -> BTreeMap<FunID, ANFunction> {
    functions
        .into_iter()
        .map(|(f_id, function)| {
            let function = match function {
                ClosureLike::Closure {
                    env,
                    args,
                    inner_exp,
                    ty,
                } => {
                    let var = var("env".to_owned());
                    let env = env
                        .0
                        .iter()
                        .enumerate()
                        .map(|(id, (var, ty))| (id.to_string(), ty.to_owned().into()))
                        .collect();
                    let env = AnType::Record(env);
                    let mut args: Vec<(Var, AnType)> = args
                        .iter()
                        .map(|(var, ty)| (var.clone(), (ty.clone()).into()))
                        .collect();
                    args.insert(0, (var, env));
                    ANFunction {
                        args,
                        inner_exp: inner_exp.anormalize(),
                        ty,
                    }
                }
                ClosureLike::Function {
                    args,
                    inner_exp,
                    ty,
                } => ANFunction {
                    args: args.into_iter().map(|(v, t)| (v, t.into())).collect(),
                    inner_exp: inner_exp.anormalize(),
                    ty: ty.into(),
                },
            };
            (f_id, function)
        })
        .collect()
}
