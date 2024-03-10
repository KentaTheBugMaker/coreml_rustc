//! A-Normalization.

use std::{collections::BTreeMap, fmt::Display};

use chumsky::container::Seq;

use crate::{
    alpha_unique::{var, Var},
    closureconversion::{CCValue, FunID, Function, NCDeclaration, NCExp},
    flat_syntax::Prim,
    record_ty::RecordType,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ANExp {
    ExpIf {
        condition: Var,
        then_exp: Box<ANExp>,
        else_exp: Box<ANExp>,
        result_var: Var,
        next_exp: Box<ANExp>,
    },
    Value {
        value: CCValue,
        result_var: Var,
        next_exp: Box<ANExp>,
    },
    // if exp1 contains free variables.
    ExpApp {
        fptr: Var,
        env: Var,
        arg: Var,
        result_var: Var,
        next_exp: Box<ANExp>,
        ty: RecordType,
    },
    // if exp1 does not contains free variables.
    ExpCall {
        f: FunID,
        arg: Var,
        result_var: Var,
        next_exp: Box<ANExp>,
        ty: RecordType,
    },
    ExpPrim {
        op: Prim,
        v1: Var,
        v2: Var,
        ty: RecordType,
        result_var: Var,
        next_exp: Box<ANExp>,
    },
    /// very restricted version of SML #label
    ExpSelect {
        label: String,
        arg: Var,
        result_var: Var,
        next_exp: Box<ANExp>,
        ty: RecordType,
    },
    /// create record.
    Record {
        fields: BTreeMap<String, Var>,
        result_var: Var,
        ty: RecordType,
        next_exp: Box<ANExp>,
    },
    /// Exit the function.
    Return {
        v: Var,
        ty: RecordType,
    },
    /// Exit the IF branch almost assign to result_var
    Phi {
        result_var: Var,
        left: Var,
    },
    Bottom {},
}

fn alpha_convert_var(candidate: Var, target: &Var, replacer: &Var) -> Var {
    if candidate.id == target.id {
        replacer.clone()
    } else {
        candidate
    }
}

impl ANExp {
    pub fn alpha_convert(self, target: &Var, replacer: &Var) -> Self {
        match self {
            ANExp::ExpIf {
                condition,
                then_exp,
                else_exp,
                result_var,
                next_exp,
            } => ANExp::ExpIf {
                condition: alpha_convert_var(condition, target, replacer),
                then_exp: Box::new(then_exp.alpha_convert(target, replacer)),
                else_exp: Box::new(else_exp.alpha_convert(target, replacer)),
                result_var: alpha_convert_var(result_var, target, replacer),
                next_exp: Box::new(next_exp.alpha_convert(target, replacer)),
            },
            ANExp::Value {
                value: CCValue::Var(v, ty),
                next_exp,
                result_var,
            } => ANExp::Value {
                value: CCValue::Var(alpha_convert_var(v, target, replacer), ty),
                next_exp: Box::new(next_exp.alpha_convert(target, replacer)),
                result_var: alpha_convert_var(result_var, target, replacer),
            },
            ANExp::Value {
                value,
                next_exp,
                result_var,
            } => ANExp::Value {
                value,
                next_exp: Box::new(next_exp.alpha_convert(target, replacer)),
                result_var: alpha_convert_var(result_var, target, replacer),
            },
            ANExp::ExpApp {
                fptr,
                env,
                arg,
                result_var,
                next_exp,
                ty,
            } => ANExp::ExpApp {
                fptr: alpha_convert_var(fptr, target, replacer),
                env: alpha_convert_var(env, target, replacer),
                arg: alpha_convert_var(arg, target, replacer),
                result_var: alpha_convert_var(result_var, target, replacer),
                next_exp: Box::new(next_exp.alpha_convert(target, replacer)),
                ty,
            },
            ANExp::ExpCall {
                f,
                arg,
                result_var,
                next_exp,
                ty,
            } => ANExp::ExpCall {
                f: f.clone(),
                arg: alpha_convert_var(arg, target, replacer),
                result_var: alpha_convert_var(result_var, target, replacer),
                next_exp: Box::new(next_exp.alpha_convert(target, replacer)),
                ty,
            },
            ANExp::ExpPrim {
                op,
                v1,
                v2,
                ty,
                result_var,
                next_exp,
            } => ANExp::ExpPrim {
                op: op.clone(),
                v1: alpha_convert_var(v1, target, replacer),
                v2: alpha_convert_var(v2, target, replacer),
                ty: ty.clone(),
                result_var: alpha_convert_var(result_var, target, replacer),
                next_exp: Box::new(next_exp.alpha_convert(target, replacer)),
            },
            ANExp::ExpSelect {
                label,
                arg,
                result_var,
                next_exp,
                ty,
            } => ANExp::ExpSelect {
                label: label.clone(),
                arg: alpha_convert_var(arg, target, replacer),
                result_var: alpha_convert_var(result_var, target, replacer),
                next_exp: Box::new(next_exp.alpha_convert(target, replacer)),
                ty,
            },
            ANExp::Record {
                fields,
                result_var,
                ty,
                next_exp,
            } => ANExp::Record {
                fields: fields
                    .into_iter()
                    .map(|(label, var)| (label, alpha_convert_var(var, target, replacer)))
                    .collect(),
                result_var: alpha_convert_var(result_var, target, replacer),
                ty,
                next_exp: Box::new(next_exp.alpha_convert(target, replacer)),
            },
            ANExp::Return { v, ty } => ANExp::Return {
                v: alpha_convert_var(v, target, replacer),
                ty,
            },
            ANExp::Phi { result_var, left } => ANExp::Phi {
                result_var: alpha_convert_var(result_var, target, replacer),
                left: alpha_convert_var(left, target, replacer),
            },
            ANExp::Bottom {} => ANExp::Bottom {},
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ANFunction {
    /// this is args.
    pub args: Vec<(Var, RecordType)>,
    pub inner_exp: ANExp,
    pub ty: RecordType,
}

impl Display for ANFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let ANFunction {
            args,
            inner_exp,
            ty: _,
        } = self;
        {
            write!(
                f,
                "fn ({}) => {inner_exp}",
                args.iter()
                    .map(|(v, _)| { format!("{v}") })
                    .reduce(|f1, f2| { f1 + "," + &f2 })
                    .unwrap_or_default()
            )
        }
    }
}

impl Display for ANExp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ANExp::ExpIf {
                condition,
                then_exp,
                else_exp,
                result_var,
                next_exp,
            } => write!(
                f,
                "
                let val {result_var} = 
                    if  {condition} then
                        {then_exp}
                    else
                        {else_exp}
                in
                    {next_exp}
                end"
            ),
            ANExp::Value {
                value,
                next_exp,
                result_var,
            } => write!(
                f,
                "
            let
                val {result_var} = {value}
            in 
                {next_exp}
            end"
            ),
            ANExp::ExpApp {
                fptr,
                env,
                arg,
                result_var,
                next_exp,
                ty,
            } => {
                write!(
                    f,
                    "
                let 
                    val {result_var} : {ty} = {fptr}({env},{arg})
                in 
                    {next_exp}
                end"
                )
            }
            ANExp::ExpCall {
                f: fid,
                arg,
                result_var,
                next_exp,
                ty,
            } => write!(
                f,
                "
                let
                    val {result_var} : {ty} = {fid}({arg})
                in 
                    {next_exp}
                end"
            ),
            ANExp::ExpPrim {
                op,
                v1,
                v2,
                ty,
                result_var,
                next_exp,
            } => write!(
                f,
                "
                let
                    val {result_var} : {ty} = {v1} {} {v2}
                in
                    {next_exp}
                end",
                match op {
                    Prim::Add => "+",
                    Prim::Eq => "=",
                    Prim::Sub => "-",
                    Prim::Div => "div",
                    Prim::Mul => "*",
                }
            ),
            ANExp::ExpSelect {
                label,
                arg,
                result_var,
                next_exp,
                ty: _,
            } => write!(
                f,
                "
                let 
                    val {result_var} = #{label} {arg}
                in
                    {next_exp}
                end"
            ),
            ANExp::Record {
                fields,
                result_var,
                ty,
                next_exp,
            } => write!(
                f,
                "
                let
                    val {result_var} : {ty} = {}
                in
                    {next_exp}
                end",
                fields
                    .iter()
                    .map(|field| { format!("{} = {}", field.0, field.1) })
                    .reduce(|f1, f2| { f1 + ",\n" + &f2 })
                    .map(|fields| { format!("{{{fields}}}") })
                    .unwrap_or("()".to_owned())
            ),
            ANExp::Return { v, ty: _ } => {
                write!(f, "return {v}")
            }
            ANExp::Phi { result_var, left } => {
                write!(f, "{left}(*{result_var} := {left}*)")
            }
            ANExp::Bottom {} => {
                write!(f, "() (*No continuation*)")
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ANDeclaration {
    Val(Var, ANExp),
}

impl Display for ANDeclaration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let ANDeclaration::Val(var, exp) = self;
        write!(f, "val {var} = {exp}")
    }
}

pub fn anormalize(exp: NCExp) -> (Box<dyn FnOnce(ANExp) -> ANExp>, Var) {
    match exp {
        NCExp::Value(v) => {
            let var = var("AN_Value".to_owned());
            let result_var = var.clone();
            (
                Box::new(move |k| ANExp::Value {
                    value: v.clone(),
                    result_var,
                    next_exp: Box::new(k),
                }),
                var,
            )
        }
        NCExp::ExpApp(fptr, env, arg, ty) => {
            let (proc1, arg) = anormalize(*arg);
            let (proc2, env) = anormalize(*env);
            let (proc3, fptr) = anormalize(*fptr);
            let v = var("AN_EXPAPP".to_owned());
            let result_var = v.clone();
            (
                Box::new(move |k| {
                    proc1(proc2(proc3(ANExp::ExpApp {
                        ty,
                        fptr,
                        env,
                        arg,
                        result_var,
                        next_exp: Box::new(k),
                    })))
                }),
                v,
            )
        }
        NCExp::ExpCall(f, arg, ty) => {
            let (proc1, arg) = anormalize(*arg);
            let v = var("AN_EXPCALL".to_owned());
            let result_var = v.clone();
            (
                Box::new(move |k| {
                    proc1(ANExp::ExpCall {
                        f,
                        arg,
                        result_var,
                        next_exp: Box::new(k),
                        ty,
                    })
                }),
                v,
            )
        }
        NCExp::ExpPrim(op, e1, e2, ty) => {
            let (proc1, v1) = anormalize(*e1);
            let (proc2, v2) = anormalize(*e2);
            let v = var("AN_EXPPRIM".to_owned());
            let result_var = v.clone();

            (
                Box::new(|k| {
                    proc1(proc2(ANExp::ExpPrim {
                        op,
                        v1,
                        v2,
                        ty,
                        result_var,
                        next_exp: Box::new(k),
                    }))
                }),
                v,
            )
        }
        NCExp::ExpIf(e1, e2, e3) => {
            let (proc1, condition) = anormalize(*e1);
            let (proc2, t) = anormalize(*e2);
            let (proc3, e) = anormalize(*e3);
            let v = var("AN_ExpIf".to_owned());
            let result_var = v.clone();
            (
                Box::new(|k| {
                    proc1(ANExp::ExpIf {
                        condition,
                        then_exp: Box::new(proc2(ANExp::Phi {
                            result_var: result_var.clone(),
                            left: t,
                        })),
                        else_exp: Box::new(proc3(ANExp::Phi {
                            result_var: result_var.clone(),
                            left: e,
                        })),
                        result_var,
                        next_exp: Box::new(k),
                    })
                }),
                v,
            )
        }
        NCExp::ExpLet((bound_var, bound_exp), inner, _) => {
            let (proc1, v) = anormalize(*bound_exp);
            //alpha convert inner before compile.

            let (proc2, kv) = anormalize(*inner);
            (
                Box::new(move |k| proc1(proc2(k).alpha_convert(&bound_var, &v))),
                kv,
            )
        }
        NCExp::ExpSelect(label, record, ty) => {
            let (proc1, arg) = anormalize(*record);
            let v = var("AN_EXPSELECT".to_owned());
            let result_var = v.clone();
            (
                Box::new(move |k| {
                    proc1(ANExp::ExpSelect {
                        label,
                        arg,
                        result_var,
                        next_exp: Box::new(k),
                        ty,
                    })
                }),
                v,
            )
        }
        NCExp::Record(fields, ty) => {
            let field_list: Vec<_> = fields
                .into_iter()
                .map(|(l, exp)| {
                    let (proc, v) = anormalize(exp);
                    (l.clone(), v, proc)
                })
                .collect();
            let fields = field_list
                .iter()
                .map(|(l, v, _)| (l.to_owned(), v.to_owned()))
                .collect();
            let v = var("AN_RECORD".to_owned());
            let result_var = v.clone();
            let core = Box::new(|k| ANExp::Record {
                fields,
                result_var,
                ty,
                next_exp: Box::new(k),
            }) as Box<dyn FnOnce(ANExp) -> ANExp>;
            (
                field_list
                    .into_iter()
                    .rev()
                    .fold(core, |proc2, (_, _, proc1)| {
                        Box::new(|k| proc1(proc2(k))) as Box<dyn FnOnce(ANExp) -> ANExp>
                    }),
                v,
            )
        }
    }
}

pub fn anormalize_functions(functions: BTreeMap<FunID, Function>) -> BTreeMap<FunID, ANFunction> {
    functions
        .into_iter()
        .map(
            |(
                fid,
                Function {
                    args,
                    inner_exp,
                    ty,
                },
            )| {
                let (proc, v) = anormalize(inner_exp);
                (
                    fid,
                    ANFunction {
                        args,
                        inner_exp: proc(ANExp::Return { v, ty: ty.clone() }),
                        ty,
                    },
                )
            },
        )
        .collect()
}

/**
 * TopLevel Declarations will converted to Top Expression
 * like MiniML compiler and SML# compiler.

*/
pub fn anormalize_decls(decls: Vec<NCDeclaration>) -> ANExp {
    let top_env = decls
        .iter()
        .map(|decl| {
            let NCDeclaration::Val(v, e) = decl;
            (
                v.name.clone(),
                NCExp::Value(CCValue::Var(v.clone(), e.ty())),
            )
        })
        .collect();
    let top_ty = RecordType::Record(
        decls
            .iter()
            .map(|decl| {
                let NCDeclaration::Val(v, e) = decl;
                (v.name.clone(), e.ty())
            })
            .collect(),
    );
    let (proc, v) = anormalize(
        decls
            .into_iter()
            .map(|NCDeclaration::Val(v, e)| (v, e))
            .rev()
            .fold(NCExp::Record(top_env, top_ty.clone()), |inner, (v, e)| {
                let ty = inner.ty();
                NCExp::ExpLet((v, Box::new(e)), Box::new(inner), ty)
            }),
    );
    proc(ANExp::Return { v, ty: top_ty })
}
