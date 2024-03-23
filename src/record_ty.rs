use std::{collections::BTreeMap, fmt::Display};

use crate::typeinf::Type;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RecordType {
    TyVar(String),
    Int,
    String,
    Bool,
    Fun(Box<RecordType>, Box<RecordType>),
    Poly(Vec<String>, Box<RecordType>),
    Record(BTreeMap<String, RecordType>),
}

impl Type {
    pub fn compile_ty(self) -> RecordType {
        match self {
            Type::TyVar(x) => RecordType::TyVar(x),
            Type::Int => RecordType::Int,
            Type::String => RecordType::String,
            Type::Bool => RecordType::Bool,
            Type::Fun(arg, ret) => {
                RecordType::Fun(Box::new(arg.compile_ty()), Box::new(ret.compile_ty()))
            }
            Type::Pair(ty1, ty2) => RecordType::Record(
                [
                    ("tuple_1".to_owned(), ty1.compile_ty()),
                    ("tuple_2".to_owned(), ty2.compile_ty()),
                ]
                .into_iter()
                .collect(),
            ),
            Type::Poly(x, y) => RecordType::Poly(x, Box::new(y.compile_ty())),
        }
    }
}
impl Display for RecordType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RecordType::TyVar(a) => write!(f, "{a}"),
            RecordType::Int => write!(f, "int"),
            RecordType::String => write!(f, "string"),
            RecordType::Bool => write!(f, "bool"),
            RecordType::Fun(ty1, ty2) => write!(f, "({ty1:} -> {ty2:})"),

            RecordType::Poly(type_ids, ty) => write!(
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
            RecordType::Record(x) => {
                write!(
                    f,
                    "{{{}}}",
                    x.iter()
                        .enumerate()
                        .fold(String::new(), |s, (idx, (f_name, f_ty))| {
                            if idx == 0 {
                                s + &format!("{f_name}:{f_ty}")
                            } else {
                                s + &format!(",{f_name}:{f_ty}")
                            }
                        })
                )
            }
        }
    }
}
