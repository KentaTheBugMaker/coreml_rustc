use crate::{
    flat_syntax::Prim,
    typeinf::{Subst, Type},
};

#[derive(Debug, Clone)]
pub enum TypedExp {
    ExpId(String, Type),
    Int(i64),
    String(String),
    True,
    False,
    ExpFn(String, Box<TypedExp>, Type),
    ExpApp(Box<TypedExp>, Box<TypedExp>, Type),
    ExpPair(Box<TypedExp>, Box<TypedExp>, Type),
    ExpProj1(Box<TypedExp>, Type),
    ExpProj2(Box<TypedExp>, Type),
    ExpPrim(Prim, Box<TypedExp>, Box<TypedExp>, Type),
    ExpIf(Box<TypedExp>, Box<TypedExp>, Box<TypedExp>),
    ExpFix(String, String, Box<TypedExp>, Type),
}
impl std::fmt::Display for TypedExp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypedExp::ExpId(x, ty) => write!(f, "{x}:{}", ty.to_string()),
            TypedExp::Int(x) => write!(f, "{x:}:int"),
            TypedExp::String(x) => write!(f, "{x:?}:string"),
            TypedExp::True => write!(f, "true:bool"),
            TypedExp::False => write!(f, "false:bool"),
            TypedExp::ExpFn(x, body, ty) => write!(f, "fn {}=>{body:} :{}", x, ty.to_string()),
            TypedExp::ExpApp(a, b, ty) => write!(f, "{}({}):{}", a, b, ty.to_string()),
            TypedExp::ExpPair(a, b, ty) => write!(f, "({},{}):{}", a, b, ty.to_string()),
            TypedExp::ExpProj1(pair, ty) => write!(f, "#1 {} :{}", pair, ty.to_string()),
            TypedExp::ExpProj2(pair, ty) => write!(f, "#2 {} :{}", pair, ty.to_string()),
            TypedExp::ExpPrim(prim, a, b, ty) => {
                write!(
                    f,
                    "{}({},{}):{}",
                    match prim {
                        Prim::Eq => "eq",
                        Prim::Add => "add",
                        Prim::Sub => "sub",
                        Prim::Mul => "mul",
                        Prim::Div => "div",
                    },
                    a,
                    b,
                    ty.to_string()
                )
            }
            TypedExp::ExpIf(a, b, c) => {
                write!(f, "if {} then {} else {} ", a, b, c)
            }
            TypedExp::ExpFix(f_, x, body, ty) => {
                write!(f, "recfn {f_:} {x:} => {} : {}", body, ty.to_string())
            }
        }
    }
}
#[derive(Debug, Clone)]
pub enum TypedDeclaration {
    Val(String, TypedExp),
}
impl std::fmt::Display for TypedDeclaration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let TypedDeclaration::Val(name, exp) = self;
        write!(f, "val {} = {} ", name, exp)
    }
}

impl TypedExp {
    pub fn ty(&self) -> Type {
        match self {
            TypedExp::ExpId(_, ty) => ty.clone(),
            TypedExp::Int(_) => Type::Int,
            TypedExp::String(_) => Type::String,
            TypedExp::True => Type::Bool,
            TypedExp::False => Type::Bool,
            TypedExp::ExpFn(_, _, ty) => ty.clone(),
            TypedExp::ExpApp(_, _, ty) => ty.clone(),
            TypedExp::ExpPair(_, _, ty) => ty.clone(),
            TypedExp::ExpProj1(_, ty) => ty.clone(),
            TypedExp::ExpProj2(_, ty) => ty.clone(),
            TypedExp::ExpPrim(_, _, _, ty) => ty.clone(),
            TypedExp::ExpIf(_, b, _) => b.ty(),
            TypedExp::ExpFix(_, _, _, ty) => ty.clone(),
        }
    }
    ///残っている型変数をsubstを使って置換する.
    /// Ok
    pub fn apply_subst(&self, subst: &Subst) -> Self {
        match self {
            TypedExp::ExpId(x, ty) => TypedExp::ExpId(x.clone(), ty.apply_subst(subst)),
            TypedExp::ExpFn(x, b, ty) => {
                let b = Box::new(b.apply_subst(subst));
                TypedExp::ExpFn(x.clone(), b, ty.apply_subst(subst))
            }
            TypedExp::ExpApp(a, b, ty) => {
                let a = a.apply_subst(subst);

                let b = b.apply_subst(subst);

                TypedExp::ExpApp(Box::new(a), Box::new(b), ty.apply_subst(subst))
            }
            TypedExp::ExpPair(a, b, ty) => {
                let a = a.apply_subst(subst);
                let b = b.apply_subst(subst);
                TypedExp::ExpPair(Box::new(a), Box::new(b), ty.apply_subst(subst))
            }
            TypedExp::ExpProj1(pair, ty) => {
                let pair = pair.apply_subst(subst);
                TypedExp::ExpProj1(Box::new(pair), ty.apply_subst(subst))
            }
            TypedExp::ExpProj2(pair, ty) => {
                let pair = pair.apply_subst(subst);
                TypedExp::ExpProj2(Box::new(pair), ty.apply_subst(subst))
            }
            TypedExp::ExpPrim(p, a, b, ty) => {
                let a = a.apply_subst(subst);
                let b = b.apply_subst(subst);
                TypedExp::ExpPrim(p.clone(), Box::new(a), Box::new(b), ty.apply_subst(subst))
            }
            TypedExp::ExpIf(a, b, c) => {
                let a = a.apply_subst(subst);
                let b = b.apply_subst(subst);
                let c = c.apply_subst(subst);
                TypedExp::ExpIf(Box::new(a), Box::new(b), Box::new(c))
            }
            TypedExp::ExpFix(f, x, body, ty) => TypedExp::ExpFix(
                f.clone(),
                x.clone(),
                Box::new(body.apply_subst(subst)),
                ty.apply_subst(subst),
            ),
            _ => self.clone(),
        }
    }
}
