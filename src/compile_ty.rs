//! compile type to deeper phases.

use crate::{alpha_unique::Var, flat_syntax::Prim, record_ty::RecordType};
#[derive(Debug)]
pub enum AUExp<T> {
    ExpId(Var, T),
    Int(i64),
    String(String),
    True,
    False,
    ExpFn(Var, Box<AUExp<T>>, T),
    ExpApp(Box<AUExp<T>>, Box<AUExp<T>>, T),
    ExpPair(Box<AUExp<T>>, Box<AUExp<T>>, T),
    ExpProj1(Box<AUExp<T>>, T),
    ExpProj2(Box<AUExp<T>>, T),
    ExpPrim(Prim, Box<AUExp<T>>, Box<AUExp<T>>, T),
    ExpIf(Box<AUExp<T>>, Box<AUExp<T>>, Box<AUExp<T>>),
    ExpFix(Var, Var, Box<AUExp<T>>, T),
}

impl crate::alpha_unique::AUExp {
    pub fn compile(self) -> AUExp<RecordType> {
        match self {
            crate::alpha_unique::AUExp::ExpId(v, t) => AUExp::ExpId(v, t.compile_ty()),
            crate::alpha_unique::AUExp::Int(x) => AUExp::Int(x),
            crate::alpha_unique::AUExp::String(x) => AUExp::String(x),
            crate::alpha_unique::AUExp::True => AUExp::True,
            crate::alpha_unique::AUExp::False => AUExp::False,
            crate::alpha_unique::AUExp::ExpFn(x, y, z) => {
                AUExp::ExpFn(x, Box::new(y.compile()), z.compile_ty())
            }
            crate::alpha_unique::AUExp::ExpApp(x, y, z) => {
                AUExp::ExpApp(Box::new(x.compile()), Box::new(y.compile()), z.compile_ty())
            }
            crate::alpha_unique::AUExp::ExpPair(x, y, z) => {
                AUExp::ExpPair(Box::new(x.compile()), Box::new(y.compile()), z.compile_ty())
            }
            crate::alpha_unique::AUExp::ExpProj1(x, y) => {
                AUExp::ExpProj1(Box::new(x.compile()), y.compile_ty())
            }
            crate::alpha_unique::AUExp::ExpProj2(x, y) => {
                AUExp::ExpProj2(Box::new(x.compile()), y.compile_ty())
            }
            crate::alpha_unique::AUExp::ExpPrim(x, y, z, w) => AUExp::ExpPrim(
                x,
                Box::new(y.compile()),
                Box::new(z.compile()),
                w.compile_ty(),
            ),
            crate::alpha_unique::AUExp::ExpIf(x, y, z) => AUExp::ExpIf(
                Box::new(x.compile()),
                Box::new(y.compile()),
                Box::new(z.compile()),
            ),
            crate::alpha_unique::AUExp::ExpFix(x, y, z, w) => {
                AUExp::ExpFix(x, y, Box::new(z.compile()), w.compile_ty())
            }
        }
    }
}
