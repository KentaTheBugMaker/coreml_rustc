use std::collections::HashSet;

use crate::{secd_machine_code::Environment, syntax_tree::Id, typeinf::Type};

#[derive(Debug, Clone)]
pub enum Value {
    I32(i32),
    I64(i64),
    StringIndex(i64),
    Fn(Id, Code),
    Closure(Environment, Id, Code),
    Rec(Environment, Id, Id, Code),
    Tuple(Vec<Value>),
}
/// WASM にはRetはなくてもよいのでRetがなくなる
///
#[derive(Debug, Clone)]
pub enum Instruction {
    PushI64(i64),
    PushI32(i32),
    PushStringIndex(i64),
    Function(Id, Code),
    Acc(Id),
    MakeClosure(Id, Code),
    MakeRecursiveClosure(Id, Id, Code),
    App,
    Pair,
    Proj1,
    Proj2,
    BoolEq,
    IntEq,
    StringEq,
    IntAdd,
    IntSub,
    IntMul,
    IntDiv,
    If(Code, Code),
}

#[derive(Debug, Clone)]
struct Context {
    strings: Vec<String>,
}
/// ブール値,Stringの変換を行う.
///
fn convert_hir_const(ctx: &mut Context, value: crate::syntax_tree::Const) -> Instruction {
    match value {
        crate::syntax_tree::Const::True => Instruction::PushI32(1),
        crate::syntax_tree::Const::False => Instruction::PushI32(0),
        crate::syntax_tree::Const::Int(x) => Instruction::PushI64(x),
        crate::syntax_tree::Const::String(x) => {
            let index = ctx.strings.len();
            ctx.strings.push(x);
            Instruction::PushStringIndex(index as i64)
        }
    }
}

/// MkClosure , MkRecursiveClosure を変換.
/// MkRecursiveClosure は自分を呼ばなければMkClosure に落とせる.
///
fn mkrec_to_closure(instruction: Instruction) -> Instruction {
    if let Instruction::MakeRecursiveClosure(f, x, code) = &instruction {
        // 関数のコード中で自由変数 f がなければクロージャに落とすことができる.
        let fv = fv(code);
        if fv.contains(f) {
            instruction
        } else {
            Instruction::MakeClosure(x.to_owned(), code.clone())
        }
    } else {
        instruction
    }
}

/// MkClosureは環境への参照がなければFnに落とせる.
fn closure_to_function(instruction: Instruction) -> Instruction {
    match &instruction {
        Instruction::MakeClosure(x, code) => {
            let mut fv = fv(code);
            fv.remove(x);
            if fv.is_empty() {
                Instruction::Function(x.to_owned(), code.clone())
            } else {
                instruction
            }
        }
        Instruction::MakeRecursiveClosure(f, x, code) => {
            let mut fv = fv(code);
            fv.remove(x);
            fv.remove(f);
            if fv.is_empty() {
                Instruction::Function(x.to_owned(), code.clone())
            } else {
                instruction
            }
        }
        _ => instruction,
    }
}

struct HirMachine {
    s: Vec<Value>,
    e: Environment,
    c: Code,
}

struct HirAsm {
    /// 関数の集合.
    /// クロージャ,再帰クロージャなどはこれに変換する.
    ///
    functions: Vec<(Code, Type)>,
    /// 文字列データの置き場
    static_data: Vec<u8>,
}

#[derive(Debug, Clone)]
pub struct Code(pub Vec<Instruction>);
/// SECD のマシン語からHIRに変換
fn secd_to_hir_code(ctx: &mut Context, code: crate::secd_machine_code::Code) -> Code {
    let mut iseq = vec![];
    for inst in code.0 {
        match inst {
            crate::secd_machine_code::Instruction::Push(scon) => {
                iseq.push(convert_hir_const(ctx, scon))
            }
            crate::secd_machine_code::Instruction::Acc(x) => iseq.push(Instruction::Acc(x)),
            crate::secd_machine_code::Instruction::MakeClosure(x, code) => {
                iseq.push(Instruction::MakeClosure(x, secd_to_hir_code(ctx, code)))
            }
            crate::secd_machine_code::Instruction::MakeRecursiveClosure(f, x, code) => iseq.push(
                Instruction::MakeRecursiveClosure(f, x, secd_to_hir_code(ctx, code)),
            ),
            crate::secd_machine_code::Instruction::App => iseq.push(Instruction::App),
            crate::secd_machine_code::Instruction::Ret => {}
            crate::secd_machine_code::Instruction::Pair => iseq.push(Instruction::Pair),
            crate::secd_machine_code::Instruction::Proj1 => iseq.push(Instruction::Proj1),
            crate::secd_machine_code::Instruction::Proj2 => iseq.push(Instruction::Proj2),
            crate::secd_machine_code::Instruction::BoolEq => iseq.push(Instruction::BoolEq),
            crate::secd_machine_code::Instruction::IntEq => iseq.push(Instruction::IntEq),
            crate::secd_machine_code::Instruction::StringEq => iseq.push(Instruction::StringEq),
            crate::secd_machine_code::Instruction::IntAdd => iseq.push(Instruction::IntAdd),
            crate::secd_machine_code::Instruction::IntSub => iseq.push(Instruction::IntSub),
            crate::secd_machine_code::Instruction::IntMul => iseq.push(Instruction::IntMul),
            crate::secd_machine_code::Instruction::IntDiv => iseq.push(Instruction::IntDiv),
            crate::secd_machine_code::Instruction::If(code1, code2) => iseq.push(Instruction::If(
                secd_to_hir_code(ctx, code1),
                secd_to_hir_code(ctx, code2),
            )),
        }
    }
    Code(iseq)
}
/// 自由変数
fn fv(body: &Code) -> HashSet<Id> {
    let mut set = HashSet::new();
    for inst in &body.0 {
        match inst {
            Instruction::Acc(x) => {
                set.insert(x.to_owned());
            }
            Instruction::MakeClosure(x, code) => {
                let mut fv = fv(code);
                fv.remove(x);
                for x in fv {
                    set.insert(x);
                }
            }
            Instruction::MakeRecursiveClosure(f, x, code) => {
                let mut fv = fv(code);
                fv.remove(x);
                fv.remove(f);
                for x in fv {
                    set.insert(x);
                }
            }
            Instruction::If(code1, code2) => {
                let fv1 = fv(code1);
                let fv2 = fv(code2);
                for x in fv1.union(&fv2) {
                    set.insert(x.to_owned());
                }
            }
            _ => {}
        }
    }
    set
}
