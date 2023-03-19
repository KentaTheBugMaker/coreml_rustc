pub enum Syntax {
    Unit,
    Bool(bool),
    Int(i64),
    Float(f64),
    Not(Box<Syntax>),
    Neg(Box<Syntax>),
    Add(Box<Syntax>, Box<Syntax>),
    Sub(Box<Syntax>, Box<Syntax>),
    FNeg(Box<Syntax>),
    FAdd(Box<Syntax>, Box<Syntax>),
    FSub(Box<Syntax>, Box<Syntax>),
    FMul(Box<Syntax>, Box<Syntax>),
    FDiv(Box<Syntax>, Box<Syntax>),
    Eq(Box<Syntax>, Box<Syntax>),
    LE(Box<Syntax>, Box<Syntax>),
    If(Box<Syntax>, Box<Syntax>, Box<Syntax>),
    Let((Id, Type), Box<Syntax>, Box<Syntax>),
    Var(Id),
    LetRec(FunDef, Box<Syntax>),
    App(Box<Syntax>, Vec<Syntax>),
    Tuple(Vec<Syntax>),
    LetTuple(Vec<(Id, Type)>, Box<Syntax>, Box<Syntax>),
    Array(Box<Syntax>, Box<Syntax>),
    Get(Box<Syntax>, Box<Syntax>),
    Put(Box<Syntax>, Box<Syntax>, Box<Syntax>),
}

pub struct FunDef {
    name: (Id, Type),
    args: Vec<(Id, Type)>,
    body: Box<Syntax>,
}

pub struct Id(String);

pub fn pp_list(list: &[Id]) -> String {
    match list.len() {
        0 => "".to_owned(),
        1 => list[0].0.clone(),
        _ => {
            let (x, xs) = list.split_first().unwrap();
            format!("{} {}", x.0, pp_list(xs))
        }
    }
}

impl Id {}

pub enum Type {
    Unit,
    Bool,
    Int,
    Float,
    Fun(Vec<Type>, Box<Type>),
    Tuple(Vec<Type>),
    Array(Box<Type>),
    Var(),
}

impl Type {
    pub fn id_of_type(&self) -> String {
        match self {
            Type::Unit => "u",
            Type::Bool => "b",
            Type::Int => "i",
            Type::Float => "d",
            Type::Fun(_, _) => "f",
            Type::Tuple(_) => "t",
            Type::Array(_) => "a",
            Type::Var() => unimplemented!(),
        }
        .to_owned()
    }
}
