//! alpha conversion for all variables.
//! in this phase each variable own unique id.
//!

use std::{
    collections::BTreeMap,
    fmt::{Debug, Display},
    sync::atomic::AtomicU64,
};

use crate::{syntax_tree::Prim, typed_ast::TypedDeclaration, typeinf::Type};
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Var {
    pub id: u64,
    pub name: String,
}

impl std::fmt::Display for Var {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.name.is_empty() {
            write!(f, "${}", self.id)
        } else {
            write!(f, "{}(${})", self.name, self.id)
        }
    }
}

impl Debug for Var {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self, f)
    }
}
#[derive(Debug, Clone)]
pub enum AUExp {
    ExpId(Var, Type),
    Int(i64),
    String(String),
    True,
    False,
    ExpFn(Var, Box<AUExp>, Type),
    ExpApp(Box<AUExp>, Box<AUExp>, Type),
    ExpPair(Box<AUExp>, Box<AUExp>, Type),
    ExpProj1(Box<AUExp>, Type),
    ExpProj2(Box<AUExp>, Type),
    ExpPrim(Prim, Box<AUExp>, Box<AUExp>, Type),
    ExpIf(Box<AUExp>, Box<AUExp>, Box<AUExp>),
    ExpFix(Var, Var, Box<AUExp>, Type),
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

impl Display for AUExp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AUExp::ExpApp(exp1, exp2, ty) => write!(f, "app({exp1}({exp2})):{ty}"),
            AUExp::ExpPair(exp1, exp2, ty) => write!(f, "({exp1},{exp2}):{ty}"),
            AUExp::ExpProj1(exp, _) => write!(f, "#1 {exp}"),
            AUExp::ExpProj2(exp, _) => write!(f, "#2 {exp}"),
            AUExp::ExpPrim(prim, exp1, exp2, _) => write!(f, "{prim}({exp1},{exp2})"),
            AUExp::ExpIf(exp1, exp2, exp3) => {
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
            AUExp::ExpId(x, _) => write!(f, "{x}"),
            AUExp::Int(x) => write!(f, "{x}"),
            AUExp::String(x) => write!(f, "{x:}"),
            AUExp::True => write!(f, "true"),
            AUExp::False => write!(f, "false"),
            AUExp::ExpFn(var, inner, ty) => {
                writeln!(f, "fn {} => ", var).ok();
                let level = indent();
                for _ in 0..level {
                    write!(f, "    ").ok();
                }
                write!(f, "{inner}").ok();
                dedent();
                write!(f, ":{ty}")
            }
            AUExp::ExpFix(f_, x, inner, ty) => {
                writeln!(f, "fix {f_} {x} = ",).ok();
                let level = indent();
                for _ in 0..level {
                    write!(f, "    ").ok();
                }
                write!(f, "{inner}").ok();
                dedent();
                write!(f, ":{ty}")
            }
        }
    }
}
pub(crate) fn var(name: String) -> Var {
    let id = VAR_ID.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
    Var { name, id }
}

#[derive(Debug, Clone)]
pub enum AUDeclaration {
    Val(Var, AUExp),
}
impl Display for AUDeclaration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let AUDeclaration::Val(var, exp) = self;
        write!(f, "val {var} = {exp}")
    }
}

static VAR_ID: AtomicU64 = AtomicU64::new(0);
pub fn alpha_conv_decls(mut decls: Vec<TypedDeclaration>) -> Vec<AUDeclaration> {
    let mut env = BTreeMap::new();
    decls.drain(..).fold(vec![], |mut audecls, decl| {
        let (audecl, new_env) = alpha_conv_decl(decl, env.clone());
        env = new_env;
        audecls.push(audecl);
        audecls
    })
}

fn alpha_conv_decl(
    decl: TypedDeclaration,
    mut env: BTreeMap<String, Var>,
) -> (AUDeclaration, BTreeMap<String, Var>) {
    let TypedDeclaration::Val(var_name, exp) = decl;
    let exp = alpha_conv_exp(&exp, &env);
    let var = match &exp {
        AUExp::ExpFix(f, _, _, _) => f.clone(),
        _ => var(var_name.clone()),
    };
    env.insert(var_name, var.clone());
    let decl = AUDeclaration::Val(var, exp);
    (decl, env)
}

fn alpha_conv_exp(exp: &crate::typed_ast::TypedExp, env: &BTreeMap<String, Var>) -> AUExp {
    match exp {
        crate::typed_ast::TypedExp::ExpId(name, ty) => {
            let var = env.get(name).unwrap();
            let exp = AUExp::ExpId(var.clone(), ty.clone());
            exp
        }
        crate::typed_ast::TypedExp::Int(x) => AUExp::Int(*x),
        crate::typed_ast::TypedExp::String(x) => AUExp::String(x.clone()),
        crate::typed_ast::TypedExp::True => AUExp::True,
        crate::typed_ast::TypedExp::False => AUExp::False,
        crate::typed_ast::TypedExp::ExpFn(name, exp, ty) => {
            let var = var(name.clone());
            let mut new_env = env.clone();
            new_env.insert(name.clone(), var.clone());
            let exp = alpha_conv_exp(exp, &new_env);
            AUExp::ExpFn(var, Box::new(exp), ty.clone())
        }
        crate::typed_ast::TypedExp::ExpApp(exp1, exp2, ty) => {
            let exp1 = alpha_conv_exp(exp1, env);
            let exp2 = alpha_conv_exp(exp2, env);
            AUExp::ExpApp(Box::new(exp1), Box::new(exp2), ty.clone())
        }
        crate::typed_ast::TypedExp::ExpPair(exp1, exp2, ty) => {
            let exp1 = alpha_conv_exp(exp1, env);
            let exp2 = alpha_conv_exp(exp2, env);
            AUExp::ExpPair(Box::new(exp1), Box::new(exp2), ty.clone())
        }
        crate::typed_ast::TypedExp::ExpProj1(exp, ty) => {
            let exp = alpha_conv_exp(&exp, env);
            AUExp::ExpProj1(Box::new(exp), ty.clone())
        }
        crate::typed_ast::TypedExp::ExpProj2(exp, ty) => {
            let exp = alpha_conv_exp(&exp, env);
            AUExp::ExpProj2(Box::new(exp), ty.clone())
        }
        crate::typed_ast::TypedExp::ExpPrim(p, exp1, exp2, ty) => {
            let exp1 = alpha_conv_exp(exp1, env);
            let exp2 = alpha_conv_exp(exp2, env);
            AUExp::ExpPrim(p.clone(), Box::new(exp1), Box::new(exp2), ty.clone())
        }
        crate::typed_ast::TypedExp::ExpIf(exp1, exp2, exp3) => {
            let exp1 = alpha_conv_exp(exp1, env);
            let exp2 = alpha_conv_exp(exp2, env);
            let exp3 = alpha_conv_exp(exp3, env);
            AUExp::ExpIf(Box::new(exp1), Box::new(exp2), Box::new(exp3))
        }
        crate::typed_ast::TypedExp::ExpFix(f, x, exp, ty) => {
            let f_var = var(f.clone());
            let x_var = var(x.clone());
            let mut new_env = env.clone();
            new_env.insert(x.clone(), x_var.clone());
            new_env.insert(f.clone(), f_var.clone());
            let exp1 = alpha_conv_exp(exp, &new_env);
            AUExp::ExpFix(f_var, x_var, Box::new(exp1), ty.clone())
        }
    }
}
