//! トランスパイラ　モジュール
//! SECD Machine Code からの変換を行う.

use crate::{
    secd_machine_code::{Code, Instruction},
    typeinf::{Type, TypeEnvironment},
};

///スタックの深さの計算
#[derive(Debug, Clone, Copy)]
pub struct StackCounter(u32);
impl StackCounter {
    pub fn new() -> Self {
        StackCounter(0)
    }
    fn push(&mut self) {
        self.0 += 1;
    }
    fn pop(&mut self) {
        self.0 -= 1;
    }
    fn depth(self) -> u32 {
        self.0
    }
}

///Rust をターゲットにしてコード生成を行う
pub fn code_gen_rust(
    code: Code,
    tyenv: TypeEnvironment,
    mut depth: StackCounter,
) -> (String, StackCounter) {
    let mut src = String::new();
    for instruction in code.0.iter().rev() {
        match instruction {
            Instruction::Push(value) => {
                let code = match value {
                    crate::syntax_tree::Const::True => {
                        format!("let var_{} = true;\n", depth.depth())
                    }
                    crate::syntax_tree::Const::False => {
                        format!("let var_{} = false;\n", depth.depth())
                    }
                    crate::syntax_tree::Const::Int(x) => {
                        format!("let var_{} = {x:};\n", depth.depth())
                    }
                    crate::syntax_tree::Const::String(x) => {
                        format!("let var_{} = \"{}\";\n", depth.depth(), x)
                    }
                };
                depth.push();
                src.push_str(&code);
            }
            Instruction::Acc(x) => {
                let code = format!("let var_{} = {};\n", depth.depth(), x);
                depth.push();
                src.push_str(&code);
            }
            Instruction::MakeClosure(x, code) => {
                let code = format!(
                    "let var_{}=|{x:}|{{
                        {}
                    }};\n",
                    depth.depth(),
                    code_gen_rust(code.clone(), tyenv.clone(), depth).0
                );
                depth.push();
                src.push_str(&code);
            }
            Instruction::MakeRecursiveClosure(f, x, code) => {
                let type_ = tyenv[f].clone();
                if let Type::Fun(arg, rt) = type_ {
                    let arg = arg.rustic_name();
                    let rt = rt.rustic_name();
                    let code = format!(
                        "fn {f:}({x:}:{arg:})->{rt:}{{
                            {}
                        }}\n
                         let var_{} = {f:};\n",
                        code_gen_rust(code.clone(), tyenv.clone(), depth).0,
                        depth.depth()
                    );
                    depth.push();
                    src.push_str(&code);
                }
            }
            Instruction::App => {
                // 関数適用
                // すべてローカル変数に束縛してしまえば任意の関数に対して行うことができる
                depth.pop();
                let var_1 = depth.depth();

                depth.pop();
                let var_2 = depth.depth();

                let top = depth.depth();
                depth.push();
                let code = format!("let var_{}=var_{}(var_{});\n", top, var_2, var_1);

                src.push_str(&code);
            }
            Instruction::Ret => {
                depth.pop();
                let code = format!("var_{}", depth.depth());
                src.push_str(&code);
            }
            Instruction::Pair => {
                depth.pop();
                let var_1 = depth.depth();

                depth.pop();
                let var_2 = depth.depth();

                let top = depth.depth();
                depth.push();
                let code = format!("let var_{}=(var_{},var_{});\n", top, var_2, var_1);

                src.push_str(&code);
            }
            Instruction::Proj1 => {
                let code = format!("let var_{} = var_{}.1;\n", depth.depth(), depth.depth());

                src.push_str(&code);
            }
            Instruction::Proj2 => {
                let code = format!("let var_{} = var_{}.2;\n", depth.depth(), depth.depth());

                src.push_str(&code);
            }
            Instruction::Prim(prim) => {
                depth.pop();
                let var_1 = depth.depth();

                depth.pop();
                let var_2 = depth.depth();

                let top = depth.depth();
                depth.push();
                let code = format!(
                    "let var_{}=var_{} {} var_{};\n",
                    top,
                    var_2,
                    match prim {
                        crate::syntax_tree::Prim::Eq => "==",
                        crate::syntax_tree::Prim::Add => "+",
                        crate::syntax_tree::Prim::Sub => "-",
                        crate::syntax_tree::Prim::Mul => "*",
                        crate::syntax_tree::Prim::Div => "/",
                    },
                    var_1
                );

                src.push_str(&code);
            }
            Instruction::If(code1, code2) => {
                depth.pop();

                let then_block = code_gen_rust(code1.clone(), tyenv.clone(), depth);

                let else_block = code_gen_rust(code2.clone(), tyenv.clone(), depth);
                let code = format!(
                    "let var_{} = if var_{}{{
                        {}
                        var_{}
                    }}else{{
                        {}
                        var_{}
                    }};\n",
                    depth.depth(),
                    depth.depth(),
                    then_block.0,
                    then_block.1.depth() - 1,
                    else_block.0,
                    else_block.1.depth() - 1
                );
                depth.push();
                src.push_str(&code);
            }
            Instruction::Bind(x) => {
                let code = format!("let {x:} = var_{};\n", depth.depth());
                src.push_str(&code);
            }
        }
    }
    (src, depth)
}
