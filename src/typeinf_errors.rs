use ariadne::{Color, Label, Report, ReportKind};

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

    pub(crate) fn report<'a>(
        &'a self,
        src_id: String,
    ) -> ariadne::Report<'static, (std::string::String, std::ops::Range<usize>)> {
        match self {
            TypeInfErrors::VariableNotDefined(identifier) => {
                Report::build(ReportKind::Error, src_id.clone(), identifier.1.start)
                    .with_message(format!("unbound variable {} ", identifier.0))
                    .with_label(
                        Label::new((src_id.clone(), self.span().into_range()))
                            .with_message(format!("{} is not bound here.", identifier.0))
                            .with_color(Color::Red),
                    )
                    .finish()
            }
            TypeInfErrors::NotExpectedType {
                error_exp,
                actual_ty,
                ideal_ty,
            } => {
                let error_exp = &error_exp.0;
                let label1 = Label::new((src_id.clone(), ideal_ty.span().into_range()))
                    .with_message(format!(" expected {} is from here", ideal_ty));
                let label2 = Label::new((src_id.clone(), actual_ty.span().into_range()))
                    .with_message(format!(" inffered {} is from here", actual_ty));
                let labels = vec![label1, label2];
                Report::build(ReportKind::Error, src_id.clone(), self.span().start)
                    .with_message(format!(
                        "{error_exp} expected to be {ideal_ty} but {actual_ty} inferred"
                    ))
                    .with_labels(labels)
                    .finish()
            }
        }
    }
}
