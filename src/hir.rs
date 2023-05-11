use crate::secd_machine_code::{Code, Value};

/// リニアメモリ上の大きさを示す.
/// malloc で確保すべき大きさの計算に使用する.
fn calcurate_data_size(value: &Value) -> usize {
    match value {
        Value::Const(c) => match c {
            crate::syntax_tree::Const::True => 4,
            crate::syntax_tree::Const::False => 4,
            crate::syntax_tree::Const::Int(_) => 8,
            // start address and length.
            // wasm uses 32bit for address.
            crate::syntax_tree::Const::String(_) => 8,
        },
        // x ptr and fptr
        Value::Closure(_, _, _) => 8,
        Value::Rec(_, _, _, _) => 8,
        Value::Tuple(a, b) => calcurate_data_size(a) + calcurate_data_size(b),
    }
}

enum WasmStackType {
    I32,
    I64,
    F32,
    F64,
}

/// WASM スタック上でのデータ型
///
fn calcurate_data_size_of_stack(value: &Value) -> Vec<WasmStackType> {
    use WasmStackType::*;
    match value {
        Value::Const(c) => match c {
            //wasm上では i32.const 1 として扱う
            crate::syntax_tree::Const::True => vec![I32],
            //wasm上では i32.const 0 として扱う
            crate::syntax_tree::Const::False => vec![I32],
            //wasm上ではi64として扱う
            crate::syntax_tree::Const::Int(_) => vec![I64],
            //wasm上では 132 i32 として扱う
            crate::syntax_tree::Const::String(_) => vec![I32, I32],
        },
        // FPTR XPTR
        Value::Closure(_, _, _) => vec![I32, I32],
        // FPTR XPTR
        Value::Rec(_, _, _, _) => vec![I32, I32],
        //スタック上ではリニアメモリへのポインタとして表現する.
        Value::Tuple(_, _) => vec![I32],
    }
}

enum WasmOp {
    I32Const(i32),
    F32Const(f32),
    I64Const(i64),
    F64Const(f64),
    I32Eq,
    I32Add,
    I32Sub,
    I32Mul,
    I32Div,

    I64Eq,
    I64Add,
    I64Sub,
    I64Mul,
    I64Div,
    /// type リストのインデックス.が入る.
    CallIndirect(i32),
    If(Vec<WasmOp>, Vec<WasmOp>),
}
/*
struct WasmBuildCtx {
    strings: Vec<String>,
}
fn secd_machine_code_to_wasm_code(secd_code: &Code, ctx: &mut WasmBuildCtx) -> Vec<WasmOp> {
    use WasmOp::*;
    let code = vec![];
    for op in &secd_code.0 {
        let op_code = match op {
            crate::secd_machine_code::Instruction::Push(c) => match c {
                crate::syntax_tree::Const::True => I32Const(1),
                crate::syntax_tree::Const::False => I32Const(0),
                crate::syntax_tree::Const::Int(x) => I64Const(*x),
                crate::syntax_tree::Const::String(x) => {
                    let index = ctx.strings.len();
                    ctx.strings.push(x.clone());
                    I32Const(index as i32);
                    I32Const(x.len() as i32)
                }
            },
            crate::secd_machine_code::Instruction::Acc(x) => {

            }
            crate::secd_machine_code::Instruction::MakeClosure(_, _) => {
                // save environment
                //
            },
            crate::secd_machine_code::Instruction::MakeRecursiveClosure(_, _, _) =>{
                // save environment
                //
                1 alloc environment data to linear memory .
                2 push environment data ptr to stack.
                3 push closure body pointer to stack.

            },
            crate::secd_machine_code::Instruction::App => WasmOp::CallIndirect(0),
            crate::secd_machine_code::Instruction::Ret => {
                //nop
            },
            crate::secd_machine_code::Instruction::Pair => {
                //スタックから2つの値を取りペアをリニアメモリ上に生成する.
                // これのポインタをスタックに残すコードを生成する
            },
            crate::secd_machine_code::Instruction::Proj1 =>{
                //スタックにあるポインタを取り除き,
                // bool,intならばスタックに展開する.
            },
            crate::secd_machine_code::Instruction::Proj2 => todo!(),
            crate::secd_machine_code::Instruction::BoolEq => {
                I32Eq
            },
            crate::secd_machine_code::Instruction::IntEq => {
                I64Eq
            },
            crate::secd_machine_code::Instruction::StringEq => todo!(),
            crate::secd_machine_code::Instruction::IntAdd => {
                I64Add
            },
            crate::secd_machine_code::Instruction::IntSub => {
                I64Sub
            },
            crate::secd_machine_code::Instruction::IntMul => {
                I64Mul
            },
            crate::secd_machine_code::Instruction::IntDiv => {
                I64Div
            },
            crate::secd_machine_code::Instruction::If(_, _) => {
                If
            },
        };
    }
    code
}
*/
