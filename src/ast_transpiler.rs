use crate::flat_syntax::Dec;
use crate::flat_syntax::Exp;
use crate::syntax_tree::Const;

use crate::syntax_tree::Id;
use crate::typeinf::Type;
use crate::typeinf::TypeEnvironment;

pub enum RustExpr {
    Const(Const),
    Add(Box<RustExpr>, Box<RustExpr>),
    Sub(Box<RustExpr>, Box<RustExpr>),
    Mul(Box<RustExpr>, Box<RustExpr>),
    Div(Box<RustExpr>, Box<RustExpr>),
    Eq(Box<RustExpr>, Box<RustExpr>),
    If(Box<RustExpr>, Box<RustExpr>, Box<RustExpr>),
    Closure(Id, Box<RustExpr>),
    Fix(Id, Id, Box<RustExpr>),
    Pair(Box<RustExpr>, Box<RustExpr>),
    DestructPair(u8, Box<RustExpr>),
    VarRef(Id),
    Call(Box<RustExpr>, Box<RustExpr>),
}
pub enum RustDeclaration {
    Val(Id, RustExpr),
    Fn(RustExpr),
}

impl Dec {
    pub fn generate_rust_declaration(&self) -> RustDeclaration {
        let Dec::Val(id, expr) = self;
        match expr {
            Exp::ExpFix(_, _, _) => RustDeclaration::Fn(expr.generate_rust_ast()),
            _ => RustDeclaration::Val(id.clone(), expr.generate_rust_ast()),
        }
    }
}

impl Exp {
    pub fn generate_rust_ast(&self) -> RustExpr {
        match self {
            Exp::ExpId(x) => RustExpr::VarRef(x.to_owned()),
            Exp::Int(x) => RustExpr::Const(Const::Int(*x)),
            Exp::String(x) => RustExpr::Const(Const::String(x.clone())),
            Exp::True => RustExpr::Const(Const::True),
            Exp::False => RustExpr::Const(Const::False),
            Exp::ExpFn(x, code) => RustExpr::Closure(x.clone(), Box::new(code.generate_rust_ast())),
            Exp::ExpApp(a, b) => RustExpr::Call(
                Box::new(a.generate_rust_ast()),
                Box::new(b.generate_rust_ast()),
            ),
            Exp::ExpPair(a, b) => RustExpr::Pair(
                Box::new(a.generate_rust_ast()),
                Box::new(b.generate_rust_ast()),
            ),
            Exp::ExpProj1(expr) => RustExpr::DestructPair(1, Box::new(expr.generate_rust_ast())),
            Exp::ExpProj2(expr) => RustExpr::DestructPair(1, Box::new(expr.generate_rust_ast())),
            Exp::ExpPrim(prim, a, b) => match prim {
                crate::syntax_tree::Prim::Eq => RustExpr::Eq(
                    Box::new(a.generate_rust_ast()),
                    Box::new(b.generate_rust_ast()),
                ),
                crate::syntax_tree::Prim::Add => RustExpr::Add(
                    Box::new(a.generate_rust_ast()),
                    Box::new(b.generate_rust_ast()),
                ),
                crate::syntax_tree::Prim::Sub => RustExpr::Sub(
                    Box::new(a.generate_rust_ast()),
                    Box::new(b.generate_rust_ast()),
                ),
                crate::syntax_tree::Prim::Mul => RustExpr::Mul(
                    Box::new(a.generate_rust_ast()),
                    Box::new(b.generate_rust_ast()),
                ),
                crate::syntax_tree::Prim::Div => RustExpr::Div(
                    Box::new(a.generate_rust_ast()),
                    Box::new(b.generate_rust_ast()),
                ),
            },
            Exp::ExpIf(a, b, c) => RustExpr::If(
                Box::new(a.generate_rust_ast()),
                Box::new(b.generate_rust_ast()),
                Box::new(c.generate_rust_ast()),
            ),
            Exp::ExpFix(f, x, code) => RustExpr::Fix(
                f.to_owned(),
                x.to_owned(),
                Box::new(code.generate_rust_ast()),
            ),
        }
    }
}

/// コード生成単位
/// 型環境と宣言が一緒になったもの
pub struct CGU<'a> {
    type_: &'a TypeEnvironment,
    declaration: RustDeclaration,
}
impl<'a> CGU<'a> {
    pub fn new(type_: &'a TypeEnvironment, declaration: RustDeclaration) -> Self {
        Self { type_, declaration }
    }
}
impl<'a> ToString for CGU<'a> {
    fn to_string(&self) -> String {
        let types = self.type_;
        let declaration = &self.declaration;
        match declaration {
            RustDeclaration::Val(x, expr) => format!("let {} = {};", x, expr.codegen_expr(types)),
            RustDeclaration::Fn(expr) => expr.codegen_expr(types),
        }
    }
}

impl RustExpr {
    pub fn codegen_expr(&self, types: &TypeEnvironment) -> String {
        match self {
            RustExpr::Const(_const) => match _const {
                Const::True => "true".to_owned(),
                Const::False => "false".to_owned(),
                Const::Int(x) => x.to_string(),
                Const::String(x) => format!("\"{x}\""),
            },
            RustExpr::Add(a, b) => format!("{} + {}", a.codegen_expr(types), b.codegen_expr(types)),
            RustExpr::Sub(a, b) => format!("{} - {}", a.codegen_expr(types), b.codegen_expr(types)),
            RustExpr::Mul(a, b) => format!("{} * {}", a.codegen_expr(types), b.codegen_expr(types)),
            RustExpr::Div(a, b) => format!("{} / {}", a.codegen_expr(types), b.codegen_expr(types)),
            RustExpr::Eq(a, b) => format!("{} == {}", a.codegen_expr(types), b.codegen_expr(types)),
            RustExpr::If(a, b, c) => format!(
                "if ({}){{ {} }}else{{ {} }}",
                a.codegen_expr(types),
                b.codegen_expr(types),
                c.codegen_expr(types)
            ),
            RustExpr::Closure(var, block) => {
                format!("|{}|{{ {} }}", var, block.codegen_expr(types))
            }
            RustExpr::Pair(a, b) => {
                format!("({},{})", a.codegen_expr(types), b.codegen_expr(types))
            }
            RustExpr::DestructPair(index, expr) => format!("{}.{index:}", expr.codegen_expr(types)),
            RustExpr::Fix(f, x, c) => {
                let f_type = &types[f];
                if let Type::Fun(arg, result) = f_type {
                    format!(
                        "fn {f:}({x:}:{})->{}{{{}}}",
                        arg.rustic_name(),
                        result.rustic_name(),
                        c.codegen_expr(types)
                    )
                } else {
                    unreachable!("fn {}({}:Unknown)->Unknown{}", f, x, c.codegen_expr(types))
                }
            }
            RustExpr::VarRef(x) => x.to_owned(),
            RustExpr::Call(a, b) => {
                if let RustExpr::VarRef(function_name) = a.as_ref() {
                    format!("{}({})", function_name, b.codegen_expr(types))
                } else {
                    format!("({})({})", a.codegen_expr(types), b.codegen_expr(types))
                }
            }
        }
    }
}
