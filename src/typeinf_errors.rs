use crate::{flat_syntax::Exp, parser_libchumsky::Spanned};

pub enum TypeInfErrors {
    VariableNotDefined(Spanned<Exp>),
    ExpectedToBeInt(Spanned<Exp>),
    ExpectedToBeBool(Spanned<Exp>),
    ExpectedToBeFun(Spanned<Exp>),
    ExpectedToBePair(Spanned<Exp>),
}
