pub type Id = String;
#[derive(Debug, Clone)]
pub enum Top {
    Dec(Dec),
}
#[derive(Debug, Clone)]
pub enum Dec {
    Val {
        name: Id,
        expression: Box<Expression>,
    },
    Fun {
        function_name: Id,
        val_name: Id,
        expression: Box<Expression>,
    },
}
#[derive(Debug, Clone)]
pub enum Expression {
    AppExp(Box<ApplyExpression>),
    If {
        condition: Box<Expression>,
        then_section: Box<Expression>,
        else_section: Box<Expression>,
    },
    Fn {
        var: Id,
        expression: Box<Expression>,
    },
}
#[derive(Debug, Clone)]
pub enum ApplyExpression {
    AtExp(AtomicExpression),
    Apply(Box<ApplyExpression>, AtomicExpression),
}
#[derive(Debug, Clone)]
pub enum AtomicExpression {
    Const(Const),
    Id(Id),
    Pair(Box<Expression>, Box<Expression>),
    Expression(Box<Expression>),
    ExtractFirst(Box<AtomicExpression>),
    ExtractSecond(Box<AtomicExpression>),
    Prim(Prim, Box<Expression>, Box<Expression>),
}
#[derive(Debug, Clone)]
pub enum Const {
    True,
    False,
    Int(i64),
    String(String),
}
#[derive(Debug, Clone)]
pub enum Prim {
    Eq,
    Add,
    Sub,
    Mul,
    Div,
}

impl From<crate::syntax_tree::Dec> for crate::flat_syntax::Dec {
    fn from(value: crate::syntax_tree::Dec) -> Self {
        match value {
            crate::syntax_tree::Dec::Val { name, expression } => {
                Self::Val(name, (*expression.clone()).into())
            }
            crate::syntax_tree::Dec::Fun {
                function_name,
                val_name,
                expression,
            } => Self::Val(
                function_name.clone(),
                crate::flat_syntax::Exp::ExpFix(
                    function_name,
                    val_name,
                    Box::new((*expression.clone()).into()),
                ),
            ),
        }
    }
}

impl From<crate::syntax_tree::Expression> for crate::flat_syntax::Exp {
    fn from(value: crate::syntax_tree::Expression) -> Self {
        match value {
            Expression::AppExp(appexp) => (*appexp.clone()).into(),
            Expression::If {
                condition,
                then_section,
                else_section,
            } => Self::ExpIf(
                Box::new((*condition.clone()).into()),
                Box::new((*then_section.clone()).into()),
                Box::new((*else_section.clone()).into()),
            ),
            Expression::Fn { var, expression } => {
                Self::ExpFn(var, Box::new((*expression.clone()).into()))
            }
        }
    }
}

impl From<AtomicExpression> for crate::flat_syntax::Exp {
    fn from(value: AtomicExpression) -> Self {
        match value {
            AtomicExpression::Const(c) => match c {
                Const::True => Self::True,
                Const::False => Self::False,
                Const::Int(n) => Self::Int(n),
                Const::String(s) => Self::String(s),
            },
            AtomicExpression::Id(id) => Self::ExpId(id),
            AtomicExpression::Pair(exp1, exp2) => Self::ExpPair(
                Box::new((*exp1.clone()).into()),
                Box::new((*exp2.clone()).into()),
            ),
            AtomicExpression::Expression(exp) => (*exp.clone()).into(),
            AtomicExpression::ExtractFirst(exp) => Self::ExpProj1(Box::new((*exp.clone()).into())),
            AtomicExpression::ExtractSecond(exp) => Self::ExpProj2(Box::new((*exp.clone()).into())),
            AtomicExpression::Prim(prim, exp1, exp2) => Self::ExpPrim(
                prim,
                Box::new((*exp1.clone()).into()),
                Box::new((*exp2.clone()).into()),
            ),
        }
    }
}
impl From<ApplyExpression> for crate::flat_syntax::Exp {
    fn from(value: ApplyExpression) -> Self {
        match value {
            ApplyExpression::AtExp(atexp) => atexp.into(),
            ApplyExpression::Apply(appexp, atexp) => crate::flat_syntax::Exp::ExpApp(
                Box::new((*appexp.clone()).into()),
                Box::new(atexp.into()),
            ),
        }
    }
}
