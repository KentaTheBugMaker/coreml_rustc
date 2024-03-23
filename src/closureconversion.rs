//! this module compile TypedExp to   

use std::{collections::BTreeMap, fmt::Display, sync::atomic::AtomicU64};

use crate::{
    alpha_unique::{var, AUDeclaration, Var},
    compile_ty::AUExp,
    flat_syntax::Prim,
    record_ty::RecordType,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ValueEnvironment(pub Vec<(Var, RecordType)>);

fn free_vars(exp: &AUExp<RecordType>) -> BTreeMap<Var, RecordType> {
    match exp {
        AUExp::<RecordType>::ExpId(x, ty) => [(x.to_owned(), ty.to_owned())].into_iter().collect(),
        AUExp::<RecordType>::Int(_) => BTreeMap::new(),
        AUExp::<RecordType>::String(_) => BTreeMap::new(),
        AUExp::<RecordType>::True => BTreeMap::new(),
        AUExp::<RecordType>::False => BTreeMap::new(),
        AUExp::<RecordType>::ExpFn(bound, exp, _) => {
            let mut fvs = free_vars(exp);
            fvs.remove(bound);
            fvs
        }
        AUExp::<RecordType>::ExpApp(exp1, exp2, _) => {
            let mut fvs1 = free_vars(exp1);
            let fvs2 = free_vars(exp2);
            fvs1.extend(fvs2);
            fvs1
        }
        AUExp::<RecordType>::ExpPair(exp1, exp2, _) => {
            let mut fvs1 = free_vars(exp1);
            let fvs2 = free_vars(exp2);
            fvs1.extend(fvs2);
            fvs1
        }
        AUExp::<RecordType>::ExpProj1(exp, _) => free_vars(exp),
        AUExp::<RecordType>::ExpProj2(exp, _) => free_vars(exp),
        AUExp::<RecordType>::ExpPrim(_, exp1, exp2, _) => {
            let mut fvs1 = free_vars(exp1);
            let fvs2 = free_vars(exp2);
            fvs1.extend(fvs2);
            fvs1
        }
        AUExp::<RecordType>::ExpIf(exp1, exp2, exp3) => {
            let mut fvs1 = free_vars(exp1);
            let fvs2 = free_vars(exp2);
            let fvs3 = free_vars(exp3);
            fvs1.extend(fvs2);
            fvs1.extend(fvs3);
            fvs1
        }
        AUExp::<RecordType>::ExpFix(bound1, bound2, exp, _) => {
            let mut fvs = free_vars(exp);
            fvs.remove(bound2);
            fvs.remove(bound1);
            fvs
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Function {
    /// this is args.
    pub args: Vec<(Var, RecordType)>,
    pub inner_exp: NCExp,
    pub ty: RecordType,
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Function {
            args,
            inner_exp,
            ty,
        } = self;
        {
            write!(f, "(",).ok();
            for (x, arg) in args.iter().enumerate() {
                if x == 0 {
                    write!(f, "{}:{}", arg.0, arg.1).ok();
                } else {
                    write!(f, ",{}:{}", arg.0, arg.1).ok();
                }
            }
            writeln!(f, "):{ty}").ok();
            writeln!(f, "{inner_exp}")
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CCValue {
    Var(Var, RecordType),
    Int(i64),
    String(String),
    True,
    False,
    FPtr(FunID, RecordType),
}

impl CCValue {
    pub fn ty(&self) -> RecordType {
        match self {
            CCValue::Var(_, t) => t.to_owned(),
            CCValue::Int(_) => RecordType::Int,
            CCValue::String(_) => RecordType::String,
            CCValue::True => RecordType::Bool,
            CCValue::False => RecordType::Bool,
            CCValue::FPtr(_, t) => t.to_owned(),
        }
    }
}

impl Display for CCValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CCValue::Var(id, ty) => {
                write!(f, "{id:}:{ty:}")
            }
            CCValue::Int(x) => {
                write!(f, "{x:}")
            }
            CCValue::String(x) => {
                write!(f, "{x:}")
            }
            CCValue::True => {
                write!(f, "true")
            }
            CCValue::False => {
                write!(f, "false")
            }
            CCValue::FPtr(fid, ty) => {
                write!(f, "{fid:}:{ty:}")
            }
        }
    }
}

/// convert closure to function that take (environment , arg)
/// when closure created enviroment and function ptr will created.
///   
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NCExp {
    Value(CCValue),
    // if exp1 contains free variables.
    ExpApp(Box<NCExp>, Box<NCExp>, Box<NCExp>, RecordType),
    // if exp1 does not contains free variables.
    ExpCall(FunID, Box<NCExp>, RecordType),
    ExpPrim(Prim, Box<NCExp>, Box<NCExp>, RecordType),
    ExpIf(Box<NCExp>, Box<NCExp>, Box<NCExp>),
    /// create local variable.
    /// and evaluate exp
    ExpLet((Var, Box<NCExp>), Box<NCExp>, RecordType),
    /// very restricted version of SML #label
    ExpSelect(String, Box<NCExp>, RecordType),
    /// create record.
    Record(BTreeMap<String, NCExp>, RecordType),
}

impl NCExp {
    pub fn ty(&self) -> RecordType {
        match self {
            NCExp::Value(v) => v.ty(),
            NCExp::ExpApp(_, _, _, x) => x.to_owned(),
            NCExp::ExpCall(_, _, x) => x.to_owned(),
            NCExp::ExpPrim(_, _, _, x) => x.to_owned(),
            NCExp::ExpIf(_, _, x) => x.ty(),
            NCExp::ExpLet(_, _, x) => x.to_owned(),
            NCExp::ExpSelect(y, e, x) => match x {
                RecordType::Record(tys) => tys[y].to_owned(),
                RecordType::Poly(_, ty) => match ty.as_ref() {
                    RecordType::Record(tys) => tys[y].to_owned(),
                    _ => unimplemented!("Select must have record type #{y} {e} but {ty}"),
                },
                _ => unimplemented!("Select must have record type #{y} {e} but {x}"),
            },
            NCExp::Record(_, x) => x.to_owned(),
        }
    }
}

static INDENT_LEVEL: AtomicU64 = AtomicU64::new(1);

fn indent() -> u64 {
    INDENT_LEVEL.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
    INDENT_LEVEL.load(std::sync::atomic::Ordering::SeqCst)
}

fn dedent() -> u64 {
    INDENT_LEVEL.fetch_sub(1, std::sync::atomic::Ordering::SeqCst);
    INDENT_LEVEL.load(std::sync::atomic::Ordering::SeqCst)
}

impl Display for NCExp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NCExp::Value(c) => write!(f, "{c}"),
            NCExp::ExpApp(exp1, exp2, exp3, ty) => {
                write!(f, "app({exp1}(env:{exp2},arg:{exp3})):{ty}")
            }
            NCExp::ExpCall(fun_id, exp, ty) => write!(f, "call({fun_id}({exp})):{ty}"),
            NCExp::ExpPrim(prim, exp1, exp2, _) => write!(f, "{prim}({exp1},{exp2})"),
            NCExp::ExpIf(exp1, exp2, exp3) => {
                writeln!(f, "if {exp1} then").ok();
                let level = indent();
                for _ in 0..level {
                    write!(f, "    ").ok();
                }
                writeln!(f, "{exp2}").ok();
                let level = dedent();
                for _ in 0..level {
                    write!(f, "    ").ok();
                }
                writeln!(f, "else").ok();
                let level_exp3 = indent();
                for _ in 0..level_exp3 {
                    write!(f, "    ").ok();
                }
                let result = write!(f, "{exp3}");
                dedent();
                result
            }
            NCExp::ExpLet(binds, inner, ty) => {
                writeln!(f, "let").ok();
                let level = indent();
                let (var, exp) = binds;

                for _ in 0..level {
                    write!(f, "    ").ok();
                }
                writeln!(f, "val {var} = {exp}").ok();

                let level = dedent();
                for _ in 0..level {
                    write!(f, "    ").ok();
                }
                writeln!(f, "in").ok();
                let level = indent();
                for _ in 0..level {
                    write!(f, "    ").ok();
                }
                writeln!(f, "{inner}").ok();
                let level = dedent();
                for _ in 0..level {
                    write!(f, "    ").ok();
                }
                write!(f, "end:{ty}")
            }
            NCExp::ExpSelect(var, x, _) => {
                write!(f, "#{var} {x}")
            }
            NCExp::Record(fields, ty) => {
                write!(
                    f,
                    "{{{}}}:{ty}",
                    fields
                        .iter()
                        .enumerate()
                        .fold(String::new(), |s, (x, (v, t))| {
                            if x == 0 {
                                s + &format!("{v}={t}")
                            } else {
                                s + &format!(",{v}={t}")
                            }
                        })
                )
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct FunID(u64);

impl Display for FunID {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "lambda_{}", self.0)
    }
}

static FUN_ID: AtomicU64 = AtomicU64::new(0);

fn new_fun_id() -> FunID {
    FunID(FUN_ID.fetch_add(1, std::sync::atomic::Ordering::SeqCst))
}

#[derive(Debug)]
pub enum CCError {
    VarNotFound(Var),
    IsNotClosure,
}

pub fn closure_conversion(
    auexp: &AUExp<RecordType>,                     /*クロージャ変換の対象 */
    functions: BTreeMap<FunID, Function>,          /*クロージャ変換によってできた関数.*/
    mut var_fun_map: BTreeMap<Var, (FunID, bool)>, /*true = closure,false=fun v->funid*/
) -> Result<
    (
        NCExp,
        BTreeMap<FunID, Function>,
        BTreeMap<Var, (FunID, bool)>,
    ),
    CCError,
> {
    let fv = free_vars(&auexp);
    match auexp {
        AUExp::<RecordType>::ExpId(var, ty) => Ok((
            NCExp::Value(CCValue::Var(var.clone(), ty.clone())),
            functions,
            var_fun_map,
        )),
        AUExp::<RecordType>::Int(x) => Ok((NCExp::Value(CCValue::Int(*x)), functions, var_fun_map)),
        AUExp::<RecordType>::String(x) => Ok((
            NCExp::Value(CCValue::String(x.clone())),
            functions,
            var_fun_map,
        )),
        AUExp::<RecordType>::True => Ok((NCExp::Value(CCValue::True), functions, var_fun_map)),
        AUExp::<RecordType>::False => Ok((NCExp::Value(CCValue::False), functions, var_fun_map)),
        AUExp::<RecordType>::ExpPair(exp1, exp2, ty) => {
            let (ncexp1, functions, var_fun_map) =
                closure_conversion(exp1, functions, var_fun_map)?;
            let (ncexp2, functions, var_fun_map) =
                closure_conversion(exp2, functions, var_fun_map)?;
            Ok((
                NCExp::Record(
                    [
                        ("tuple_1".to_string(), ncexp1),
                        ("tuple_2".to_string(), ncexp2),
                    ]
                    .into_iter()
                    .collect(),
                    ty.clone(),
                ),
                functions,
                var_fun_map,
            ))
        }
        AUExp::<RecordType>::ExpProj1(exp, _) => {
            let (ncexp, functions, var_fun_map) = closure_conversion(exp, functions, var_fun_map)?;
            Ok((
                NCExp::ExpSelect("tuple_1".to_string(), Box::new(ncexp.clone()), ncexp.ty()),
                functions,
                var_fun_map,
            ))
        }
        AUExp::<RecordType>::ExpProj2(exp, _) => {
            let (ncexp, functions, var_fun_map) = closure_conversion(exp, functions, var_fun_map)?;
            Ok((
                NCExp::ExpSelect("tuple_2".to_string(), Box::new(ncexp.clone()), ncexp.ty()),
                functions,
                var_fun_map,
            ))
        }
        AUExp::<RecordType>::ExpPrim(prim, exp1, exp2, ty) => {
            let (ncexp1, functions, var_fun_map) =
                closure_conversion(exp1, functions, var_fun_map)?;
            let (ncexp2, functions, var_fun_map) =
                closure_conversion(exp2, functions, var_fun_map)?;
            Ok((
                NCExp::ExpPrim(prim.clone(), Box::new(ncexp1), Box::new(ncexp2), ty.clone()),
                functions,
                var_fun_map,
            ))
        }
        AUExp::<RecordType>::ExpIf(exp1, exp2, exp3) => {
            let (ncexp1, functions, var_fun_map) =
                closure_conversion(exp1, functions, var_fun_map)?;
            let (ncexp2, functions, var_fun_map) =
                closure_conversion(exp2, functions, var_fun_map)?;
            let (ncexp3, functions, var_fun_map) =
                closure_conversion(exp3, functions, var_fun_map)?;
            Ok((
                NCExp::ExpIf(Box::new(ncexp1), Box::new(ncexp2), Box::new(ncexp3)),
                functions,
                var_fun_map,
            ))
        }
        AUExp::<RecordType>::ExpFn(x, inner, ty) => {
            let fun_id = new_fun_id();
            let env = var("CC_ENV".to_owned());

            let fv_tys = RecordType::Record(
                fv.iter()
                    .map(|(v, t)| (format!("var_{}", v.id), t.to_owned()))
                    .collect(),
            );
            let bindings: Vec<(Var, NCExp)> = fv
                .iter()
                .map(|(var, ty)| {
                    (
                        var.clone(),
                        NCExp::ExpSelect(
                            format!("var_{}", var.id),
                            Box::new(NCExp::Value(CCValue::Var(env.clone(), fv_tys.clone()))),
                            ty.clone(),
                        ),
                    )
                })
                .collect();

            let (arg_ty, ret_ty) = match ty {
                RecordType::Fun(arg_ty, ret_ty) => (arg_ty.clone(), ret_ty.clone()),
                RecordType::Poly(_, ty) => match ty.as_ref() {
                    RecordType::Fun(arg_ty, ret_ty) => (arg_ty.clone(), ret_ty.clone()),
                    _ => unreachable!("this case will not come here"),
                },
                _ => unreachable!("this case will not come here"),
            };

            let is_closure = !bindings.is_empty();

            let (inner, mut functions, var_fun_map) =
                closure_conversion(&inner, functions, var_fun_map)?;

            //自由変数があるならば関数内で束縛する.
            //環境によって与えられる変数があるならば,closure そうでないならばfunction.
            let function = if is_closure {
                Function {
                    args: vec![(env, fv_tys), (x.clone(), *arg_ty)],
                    inner_exp: bindings.into_iter().fold(inner, |i, bind| {
                        NCExp::ExpLet((bind.0, Box::new(bind.1)), Box::new(i), *ret_ty.clone())
                    }),
                    ty: ty.clone(),
                }
            } else {
                Function {
                    args: vec![(x.clone(), *arg_ty)],
                    inner_exp: inner.clone(),
                    ty: ty.clone(),
                }
            };
            let Function {
                args,
                inner_exp: _,
                ty,
            } = &function;
            let exp = match args.as_slice() {
                [_] => NCExp::Record(
                    [
                        (
                            "f_ptr".to_owned(),
                            NCExp::Value(CCValue::FPtr(fun_id, ty.clone())),
                        ),
                        (
                            "env".to_owned(),
                            NCExp::Record(BTreeMap::new(), RecordType::Record(BTreeMap::new())),
                        ),
                    ]
                    .into_iter()
                    .collect(),
                    ty.clone(),
                ),
                [_, (_, env_ty)] => NCExp::Record(
                    [
                        (
                            "f_ptr".to_owned(),
                            NCExp::Value(CCValue::FPtr(fun_id, ty.clone())),
                        ),
                        (
                            "env".to_owned(),
                            NCExp::Record(
                                fv.iter().fold(BTreeMap::new(), |mut fields, (v, t)| {
                                    fields.insert(
                                        format!("var_{}", v.id),
                                        NCExp::Value(CCValue::Var(v.clone(), t.clone())),
                                    );
                                    fields
                                }),
                                env_ty.clone(),
                            ),
                        ),
                    ]
                    .into_iter()
                    .collect(),
                    ty.clone(),
                ),
                _ => unreachable!(),
            };
            Ok((
                exp,
                {
                    functions.insert(fun_id.clone(), function);
                    functions
                },
                var_fun_map,
            ))
        }
        AUExp::<RecordType>::ExpApp(exp1, exp2, ty) => {
            let (ncexp1, functions, var_fun_map) =
                closure_conversion(exp1, functions, var_fun_map)?;
            let (ncexp2, functions, var_fun_map) =
                closure_conversion(exp2, functions, var_fun_map)?;
            let ncexp2 = match ncexp2 {
                NCExp::Value(CCValue::FPtr(x, y)) => NCExp::Record(
                    [
                        (
                            "f_ptr".to_owned(),
                            NCExp::Value(CCValue::FPtr(x, y.to_owned())),
                        ),
                        (
                            "env".to_owned(),
                            NCExp::Record(BTreeMap::new(), RecordType::Record(BTreeMap::new())),
                        ),
                    ]
                    .into_iter()
                    .collect(),
                    RecordType::Record(
                        [
                            ("f_ptr".to_owned(), y),
                            ("env".to_owned(), RecordType::Record(BTreeMap::new())),
                        ]
                        .into_iter()
                        .collect(),
                    ),
                ),
                _ => ncexp2,
            };
            match ncexp1.clone() {
                //Optimize direct call.
                NCExp::Record(fields, ty) => {
                    let env = fields.get("env").ok_or_else(|| CCError::IsNotClosure)?;
                    let fptr = fields.get("f_ptr").ok_or_else(|| CCError::IsNotClosure)?;
                    match env {
                        NCExp::Record(_, RecordType::Record(fields)) if fields.is_empty() => {
                            match fptr {
                                NCExp::Value(CCValue::FPtr(fun_id, _)) => Ok((
                                    NCExp::ExpCall(*fun_id, Box::new(ncexp2), ty.clone()),
                                    functions,
                                    var_fun_map,
                                )),
                                _ => {
                                    // レコードから関数ポインタを取り出す.
                                    let f_id = NCExp::ExpSelect(
                                        "f_ptr".to_string(),
                                        Box::new(ncexp1.clone()),
                                        ncexp1.ty(),
                                    );
                                    // レコードから環境を取り出す.
                                    let env = NCExp::ExpSelect(
                                        "env".to_string(),
                                        Box::new(ncexp1.clone()),
                                        ncexp1.ty(),
                                    );

                                    Ok((
                                        NCExp::ExpApp(
                                            Box::new(f_id),
                                            Box::new(env),
                                            Box::new(ncexp2),
                                            ty.clone(),
                                        ),
                                        functions,
                                        var_fun_map,
                                    ))
                                }
                            }
                        }

                        _ => {
                            todo!("NORMAL calling convention.")
                        }
                    }
                }
                NCExp::Value(CCValue::FPtr(f_id, ty)) => Ok((
                    NCExp::ExpCall(f_id, Box::new(ncexp2), ty.clone()),
                    functions,
                    var_fun_map,
                )),
                NCExp::Value(CCValue::Var(var, f_ty)) => {
                    let e = match var_fun_map.get(&var) {
                        Some((f_id, is_closure)) => {
                            if *is_closure {
                                NCExp::ExpApp(
                                    Box::new(NCExp::Value(CCValue::FPtr(*f_id, f_ty))),
                                    Box::new(NCExp::ExpSelect(
                                        "env".to_owned(),
                                        Box::new(ncexp1.clone()),
                                        ncexp1.ty(),
                                    )),
                                    Box::new(ncexp2),
                                    ty.clone(),
                                )
                            } else {
                                NCExp::ExpCall(*f_id, Box::new(ncexp2), ty.clone())
                            }
                        }
                        None => NCExp::ExpApp(
                            Box::new(NCExp::ExpSelect(
                                "f_ptr".to_owned(),
                                Box::new(ncexp1.clone()),
                                ncexp1.ty(),
                            )),
                            Box::new(NCExp::ExpSelect(
                                "env".to_owned(),
                                Box::new(ncexp1.clone()),
                                ncexp1.ty(),
                            )),
                            Box::new(ncexp2),
                            ty.clone(),
                        ),
                    };
                    Ok((e, functions, var_fun_map))
                }
                ncexp1 => {
                    // レコードから関数ポインタを取り出す.
                    let f_id = NCExp::ExpSelect(
                        "f_ptr".to_string(),
                        Box::new(ncexp1.clone()),
                        ncexp1.ty(),
                    );
                    // レコードから環境を取り出す.
                    let env =
                        NCExp::ExpSelect("env".to_string(), Box::new(ncexp1.clone()), ncexp1.ty());

                    Ok((
                        NCExp::ExpApp(Box::new(f_id), Box::new(env), Box::new(ncexp2), ty.clone()),
                        functions,
                        var_fun_map,
                    ))
                }
            }
        }
        AUExp::<RecordType>::ExpFix(f, x, inner, ty) => {
            let fun_id = new_fun_id();
            let env = var("CC_ENV".to_owned());

            let fv_tys = RecordType::Record(
                fv.iter()
                    .map(|(v, t)| (format!("var_{}", v.id), t.clone()))
                    .collect(),
            );

            let bindings: Vec<(Var, NCExp)> = fv
                .iter()
                .map(|(var, ty)| {
                    (
                        var.clone(),
                        NCExp::ExpSelect(
                            format!("var_{}", var.id),
                            Box::new(NCExp::Value(CCValue::Var(env.clone(), fv_tys.clone()))),
                            ty.clone(),
                        ),
                    )
                })
                .collect();

            let (arg_ty, ret_ty) = match ty {
                RecordType::Fun(arg_ty, ret_ty) => (arg_ty.clone(), ret_ty.clone()),
                RecordType::Poly(_, ty) => match ty.as_ref() {
                    RecordType::Fun(arg_ty, ret_ty) => (arg_ty.clone(), ret_ty.clone()),
                    _ => unreachable!("this case will not come here"),
                },
                _ => unreachable!("this case will not come here"),
            };

            let is_closure = !bindings.is_empty();
            var_fun_map.insert(f.clone(), (fun_id, is_closure));

            let (inner, mut functions, var_fun_map) =
                closure_conversion(&inner, functions, var_fun_map.clone())?;
            // 自由変数があるならば関数内で束縛する.
            // 何も束縛しない場合も関数.
            // 自分以外の何かを束縛するときはクロージャ
            let function = if is_closure {
                Function {
                    args: vec![(env, fv_tys), (x.clone(), *arg_ty)],
                    inner_exp: bindings.clone().into_iter().fold(inner, |i, bind| {
                        NCExp::ExpLet((bind.0, Box::new(bind.1)), Box::new(i), *ret_ty.clone())
                    }),
                    ty: ty.clone(),
                }
            } else {
                Function {
                    args: vec![(x.clone(), *arg_ty)],
                    inner_exp: inner,
                    ty: ty.clone(),
                }
            };
            let Function {
                args,
                inner_exp: _,
                ty,
            } = &function;
            let exp = match args.as_slice() {
                [_] => NCExp::Record(
                    [
                        (
                            "f_ptr".to_owned(),
                            NCExp::Value(CCValue::FPtr(fun_id, ty.clone())),
                        ),
                        (
                            "env".to_owned(),
                            NCExp::Record(BTreeMap::new(), RecordType::Record(BTreeMap::new())),
                        ),
                    ]
                    .into_iter()
                    .collect(),
                    ty.clone(),
                ),
                [_, (_, env_ty)] => NCExp::Record(
                    [
                        (
                            "f_ptr".to_owned(),
                            NCExp::Value(CCValue::FPtr(fun_id, ty.clone())),
                        ),
                        (
                            "env".to_owned(),
                            NCExp::Record(
                                fv.iter().fold(BTreeMap::new(), |mut fields, (v, t)| {
                                    fields.insert(
                                        format!("var_{}", v.id),
                                        NCExp::Value(CCValue::Var(v.to_owned(), t.to_owned())),
                                    );
                                    fields
                                }),
                                env_ty.clone(),
                            ),
                        ),
                    ]
                    .into_iter()
                    .collect(),
                    ty.clone(),
                ),
                _ => unreachable!("Failed to compile {f}"),
            };
            Ok((
                exp,
                {
                    functions.insert(fun_id.clone(), function);
                    functions
                },
                var_fun_map,
            ))
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NCDeclaration {
    Val(Var, NCExp),
}

impl Display for NCDeclaration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let NCDeclaration::Val(var, exp) = self;
        write!(f, "val {var} = {exp}")
    }
}

fn closure_conversion_decl(
    au_decl: AUDeclaration,
    functions: BTreeMap<FunID, Function>,
    var_fun_map: BTreeMap<Var, (FunID, bool)>,
) -> Result<
    (
        NCDeclaration,
        BTreeMap<FunID, Function>,
        BTreeMap<Var, (FunID, bool)>,
    ),
    CCError,
> {
    let AUDeclaration::Val(var, auexp) = au_decl;
    let (ncexp, functions, mut var_fun_map) =
        closure_conversion(&(auexp.compile()), functions, var_fun_map)?;
    match &ncexp {
        NCExp::Value(CCValue::FPtr(f_id, _)) => {
            var_fun_map.insert(var.to_owned(), (f_id.to_owned(), false));
        }
        NCExp::Record(fields, _) => {
            let f = fields.get("f_ptr");
            match f {
                Some(NCExp::Value(CCValue::FPtr(f_id, _))) => {
                    if let Some(NCExp::Record(fields, _)) = fields.get("env") {
                        let is_closure = !fields.is_empty();
                        var_fun_map.insert(var.to_owned(), (f_id.to_owned(), is_closure));
                    }
                }
                None => {}
                _ => eprintln!("INTERNAL COMPILER ERROR failed to add {var} to var -> fun map"),
            }
        }
        _ => {}
    };
    Ok((NCDeclaration::Val(var, ncexp), functions, var_fun_map))
}

pub fn closure_conversion_decls(
    mut au_decls: Vec<AUDeclaration>,
) -> (Vec<NCDeclaration>, BTreeMap<FunID, Function>) {
    let (decls, functions, _) = au_decls.drain(..).fold(
        (vec![], BTreeMap::new(), BTreeMap::new()),
        |(mut nc_decls, functions, var_fun_map), au_decl| match closure_conversion_decl(
            au_decl.clone(),
            functions.clone(),
            var_fun_map.clone(),
        ) {
            Ok((nc_decl, functions, var_fun_map)) => {
                nc_decls.push(nc_decl);

                (nc_decls, functions, var_fun_map)
            }
            Err(err) => {
                println!("failed to compile {:?} {:?}", au_decl, err);
                (nc_decls, functions, var_fun_map)
            }
        },
    );
    (decls, functions)
}
