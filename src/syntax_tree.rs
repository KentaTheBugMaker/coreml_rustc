pub type Id = String;
#[derive(Debug)]
pub enum Top {
    Dec(Dec),
}
#[derive(Debug)]
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
#[derive(Debug)]
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
#[derive(Debug)]
pub enum ApplyExpression {
    AtExp(AtomicExpression),
    Apply(Box<ApplyExpression>, AtomicExpression),
}
#[derive(Debug)]
pub enum AtomicExpression {
    Const(Const),
    Id(Id),
    Pair(Box<Expression>, Box<Expression>),
    Expression(Box<Expression>),
    ExtractFirst(Box<AtomicExpression>),
    ExtractSecond(Box<AtomicExpression>),
    Prim(Prim, Box<Expression>, Box<Expression>),
}
#[derive(Debug)]
pub enum Const {
    True,
    False,
    Int(i64),
    String(String),
}
#[derive(Debug)]
pub enum Prim {
    Eq,
    Add,
    Sub,
    Mul,
    Div,
}
