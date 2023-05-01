use crate::flat_syntax::Exp;

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
    AppExp(ApplyExpression),
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
pub struct ApplyExpression(pub Vec<AtomicExpression>);
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

impl std::ops::Add for Const {
    type Output = i64;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Const::Int(v1), Const::Int(v2)) => v1 + v2,
            _ => {
                unimplemented!("add is not allowed for non Int type");
            }
        }
    }
}

impl std::ops::Sub for Const {
    type Output = i64;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Const::Int(v1), Const::Int(v2)) => v1 - v2,
            _ => {
                unimplemented!("sub is not allowed for non Int type");
            }
        }
    }
}

impl std::ops::Mul for Const {
    type Output = i64;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Const::Int(v1), Const::Int(v2)) => v1 * v2,
            _ => {
                unimplemented!("mul is not allowed for non Int type");
            }
        }
    }
}

impl std::ops::Div for Const {
    type Output = i64;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Const::Int(v1), Const::Int(v2)) => v1 / v2,
            _ => {
                unimplemented!("div is not allowed for non Int type");
            }
        }
    }
}

impl std::cmp::PartialEq for Const {
    fn eq(&self, rhs: &Self) -> bool {
        match (self, rhs) {
            (Const::Int(v1), Const::Int(v2)) => v1 == v2,
            _ => {
                unimplemented!("eq  is not implemented for non Int type");
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum PrimOrIdent {
    Eq,
    Add,
    Sub,
    Mul,
    Div,
    Ident(String),
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
                Self::Val(name, (*expression).into())
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
                    Box::new((*expression).into()),
                ),
            ),
        }
    }
}

impl From<crate::syntax_tree::Expression> for crate::flat_syntax::Exp {
    fn from(value: crate::syntax_tree::Expression) -> Self {
        match value {
            Expression::AppExp(appexp) => (appexp).into(),
            Expression::If {
                condition,
                then_section,
                else_section,
            } => Self::ExpIf(
                Box::new((*condition).into()),
                Box::new((*then_section).into()),
                Box::new((*else_section).into()),
            ),
            Expression::Fn { var, expression } => {
                Self::ExpFn(var, Box::new((*expression).into()))
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
                Box::new((*exp1).into()),
                Box::new((*exp2).into()),
            ),
            AtomicExpression::Expression(exp) => (*exp).into(),
            AtomicExpression::ExtractFirst(exp) => Self::ExpProj1(Box::new((*exp).into())),
            AtomicExpression::ExtractSecond(exp) => Self::ExpProj2(Box::new((*exp).into())),
            AtomicExpression::Prim(prim, exp1, exp2) => Self::ExpPrim(
                prim,
                Box::new((*exp1).into()),
                Box::new((*exp2).into()),
            ),
        }
    }
}

impl From<ApplyExpression> for crate::flat_syntax::Exp {
    fn from(mut value: ApplyExpression) -> Self {
        match value.0.len() {
            0 => {
                unimplemented!("Apply for zero expressions is not allowed ")
            }
            1 => value.0[0].clone().into(),
            _ => {
                let first = value.0[0].clone();
                let second = value.0[1].clone();

                let first_apply = Exp::ExpApp(Box::new(first.into()), Box::new(second.into()));
                if value.0.len() > 2 {
                    value.0.drain(..).skip(2).fold(first_apply, |a, b| {
                        Exp::ExpApp(Box::new(a), Box::new(b.into()))
                    })
                } else {
                    first_apply
                }
            }
        }
    }
}
