use crate::{
    flat_syntax::Exp,
    parser_libchumsky::{Span, Spanned},
    typeinf::Type,
};

pub enum TypeInfErrors {
    VariableNotDefined(Spanned<String>),
    NotExpectedType {
        error_exp: Spanned<Exp>,
        actual_ty: Type,
        ideal_ty: Type,
    },
}

impl TypeInfErrors {
    pub fn span(&self) -> Span {
        match self {
            TypeInfErrors::VariableNotDefined(x) => x.1,
            TypeInfErrors::NotExpectedType {
                error_exp,
                actual_ty,
                ideal_ty,
            } => error_exp.1,
        }
    }
    pub fn message(&self) -> String {
        match self {
            TypeInfErrors::VariableNotDefined(x) => {
                format!("unbound variable {} ", x.0)
            }
            TypeInfErrors::NotExpectedType {
                error_exp,
                actual_ty,
                ideal_ty,
            } => {
                let error_exp = &error_exp.0;
                format!("{error_exp} expected to be {ideal_ty} but {actual_ty} inferred")
            }
        }
    }
}
