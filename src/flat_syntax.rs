use std::fmt::Display;

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
