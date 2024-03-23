use std::{fmt::Display, sync::atomic::AtomicU64};

use crate::parser_libchumsky::Spanned;

#[derive(Debug, Clone)]
pub enum Exp {
    ExpId(String),
    Int(i64),
    String(String),
    True,
    False,
    ExpFn(String, Box<Spanned<Exp>>),
    ExpApp(Box<Spanned<Exp>>, Box<Spanned<Exp>>),
    ExpPair(Box<Spanned<Exp>>, Box<Spanned<Exp>>),
    ExpProj1(Box<Spanned<Exp>>),
    ExpProj2(Box<Spanned<Exp>>),
    ExpPrim(Prim, Box<Spanned<Exp>>, Box<Spanned<Exp>>),
    ExpIf(Box<Spanned<Exp>>, Box<Spanned<Exp>>, Box<Spanned<Exp>>),
    ExpFix(String, String, Box<Spanned<Exp>>),
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

impl Display for Exp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Exp::ExpApp(exp1, exp2) => {
                let exp1 = &exp1.0;
                let exp2 = &exp2.0;
                write!(f, "({exp1}({exp2}))")
            }
            Exp::ExpPair(exp1, exp2) => {
                let exp1 = &exp1.0;
                let exp2 = &exp2.0;
                write!(f, "({exp1},{exp2})")
            }
            Exp::ExpProj1(exp) => {
                let exp = &exp.0;
                write!(f, "#1 {exp}")
            }
            Exp::ExpProj2(exp) => {
                let exp = &exp.0;

                write!(f, "#2 {exp}")
            }
            Exp::ExpPrim(prim, exp1, exp2) => {
                let exp1 = &exp1.0;
                let exp2 = &exp2.0;
                write!(f, "{prim}({exp1},{exp2})")
            }
            Exp::ExpIf(exp1, exp2, exp3) => {
                let exp1 = &exp1.0;
                let exp2 = &exp2.0;
                let exp3 = &exp3.0;
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
            Exp::ExpId(x) => write!(f, "{x}"),
            Exp::Int(x) => write!(f, "{x}"),
            Exp::String(x) => write!(f, "{x:}"),
            Exp::True => write!(f, "true"),
            Exp::False => write!(f, "false"),
            Exp::ExpFn(var, inner) => {
                let inner = &inner.0;

                writeln!(f, "fn {} => ", var).ok();
                let level = indent();
                for _ in 0..level {
                    write!(f, "    ").ok();
                }
                let result = write!(f, "{inner}");
                dedent();
                result
            }
            Exp::ExpFix(f_, x, inner) => {
                let inner = &inner.0;
                writeln!(f, "fix {f_} {x} = ",).ok();
                let level = indent();
                for _ in 0..level {
                    write!(f, "    ").ok();
                }
                let result = write!(f, "{inner}");
                dedent();
                result
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum Dec {
    Val(String, Spanned<Exp>),
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Prim {
    Add,
    Eq,
    Sub,
    Div,
    Mul,
}

impl Display for Prim {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Prim::Add => "add",
                Prim::Eq => "eq",
                Prim::Sub => "sub",
                Prim::Div => "div",
                Prim::Mul => "mul",
            }
        )
    }
}
