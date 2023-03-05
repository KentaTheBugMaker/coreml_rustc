use crate::syntax_tree::Prim;

#[derive(Debug, Clone)]
pub enum Exp {
    ExpId(String),
    Int(i64),
    String(String),
    True,
    False,
    ExpFn(String, Box<Exp>),
    ExpApp(Box<Exp>, Box<Exp>),
    ExpPair(Box<Exp>, Box<Exp>),
    ExpProj1(Box<Exp>),
    ExpProj2(Box<Exp>),
    ExpPrim(Prim, Box<Exp>, Box<Exp>),
    ExpIf(Box<Exp>, Box<Exp>, Box<Exp>),
    ExpFix(String, String, Box<Exp>),
}
#[derive(Debug, Clone)]
pub enum Dec {
    Val(String, Exp),
}
