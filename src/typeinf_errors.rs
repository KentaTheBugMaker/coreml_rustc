use crate::{
    flat_syntax::Exp,
    parser_libchumsky::{MySpan, Spanned},
    typeinf::SpannedType,
};

pub enum TypeInfErrors {
    VariableNotDefined(Spanned<String>),
    NotExpectedType {
        error_exp: Spanned<Exp>,
        actual_ty: SpannedType,
        ideal_ty: SpannedType,
    },
}

impl TypeInfErrors {
    pub fn span(&self) -> MySpan {
        match self {
            TypeInfErrors::VariableNotDefined(x) => x.1,
            TypeInfErrors::NotExpectedType {
                error_exp,
                actual_ty: _,
                ideal_ty: _,
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
