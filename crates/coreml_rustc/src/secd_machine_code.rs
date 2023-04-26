//! SECD Machine Code generator and interpreter
//!

use std::collections::HashMap;

use crate::{
    flat_syntax::Exp,
    syntax_tree::{Const, Id, Prim},
};
#[derive(Debug, Clone)]
pub enum Value {
    Const(Const),
    Closure(Environment, Id, Code),
    Rec(Environment, Id, Id, Code),
    Tuple(Box<Value>, Box<Value>),
}

#[derive(Debug, Clone)]
pub struct Environment(HashMap<Id, Value>);
#[derive(Debug, Clone)]
pub struct Code(pub Vec<Instruction>);
impl Code {
    pub fn blank() -> Self {
        Code(vec![])
    }
}
#[derive(Debug, Clone)]
pub enum Instruction {
    Push(Const),
    Acc(Id),
    MakeClosure(Id, Code),
    MakeRecursiveClosure(Id, Id, Code),
    App,
    Ret,
    Pair,
    Proj1,
    Proj2,
    Prim(Prim),
    If(Code, Code),
    /// 変数への束縛
    Bind(Id),
}
#[derive(Debug)]
pub struct Machine {
    s: Vec<Value>,
    e: Environment,
    c: Code,
    d: Vec<(Environment, Code)>,
}
#[derive(Debug)]
pub enum RuntimeError {
    TypeError,
    VariableNotFind,
    StackIsEmpty,
}

impl Machine {
    pub fn eval_one(&mut self) -> Result<Option<Value>, RuntimeError> {
        if let Some(i) = self.c.0.pop() {
            println!("machine state {:?}", self);
            match i {
                Instruction::Push(x) => {
                    self.s.push(Value::Const(x));
                    Ok(None)
                }
                Instruction::Acc(id) => {
                    if let Some(value) = self.e.0.get(&id) {
                        self.s.push(value.clone());
                        Ok(None)
                    } else {
                        Err(RuntimeError::VariableNotFind)
                    }
                }
                Instruction::MakeClosure(x, code) => {
                    self.s.push(Value::Closure(self.e.clone(), x, code));
                    Ok(None)
                }
                Instruction::MakeRecursiveClosure(f, x, code) => {
                    self.s.push(Value::Rec(self.e.clone(), f, x, code));
                    Ok(None)
                }
                Instruction::App => {
                    let v1 = self.s.pop().ok_or(RuntimeError::StackIsEmpty)?;
                    let closure = self.s.pop().ok_or(RuntimeError::StackIsEmpty)?;
                    match closure {
                        Value::Closure(e0, x, code) => {
                            self.d.push((self.e.clone(), self.c.clone()));
                            self.e = e0;
                            self.e.0.insert(x, v1);
                            self.c = code;
                            Ok(None)
                        }
                        Value::Rec(e0, f, x, code) => {
                            self.d.push((self.e.clone(), self.c.clone()));
                            self.e = e0.clone();
                            self.e.0.insert(x.clone(), v1);
                            self.e
                                .0
                                .insert(f.clone(), Value::Rec(e0, f, x, code.clone()));
                            self.c = code;
                            Ok(None)
                        }
                        _ => Err(RuntimeError::TypeError),
                    }
                }
                Instruction::Ret => {
                    let (e, c) = self.d.pop().ok_or(RuntimeError::StackIsEmpty)?;
                    self.e = e;
                    self.c = c;
                    Ok(None)
                }
                Instruction::Pair => {
                    let v1 = self.s.pop().ok_or(RuntimeError::StackIsEmpty)?;
                    let v2 = self.s.pop().ok_or(RuntimeError::StackIsEmpty)?;
                    self.s.push(Value::Tuple(Box::new(v2), Box::new(v1)));
                    Ok(None)
                }
                Instruction::Proj1 => {
                    if let Value::Tuple(a, _b) = self.s.pop().ok_or(RuntimeError::StackIsEmpty)? {
                        self.s.push(a.as_ref().to_owned());
                        Ok(None)
                    } else {
                        eprintln!("Interpreter detected a error #1 is applied  for non tuple type");
                        Err(RuntimeError::TypeError)
                    }
                }
                Instruction::Proj2 => {
                    if let Value::Tuple(_a, b) = self.s.pop().ok_or(RuntimeError::StackIsEmpty)? {
                        self.s.push(b.as_ref().to_owned());
                        Ok(None)
                    } else {
                        eprintln!("Interpreter detected a error #2 is applied for non tuple type ");
                        Err(RuntimeError::TypeError)
                    }
                }
                Instruction::Prim(prim) => {
                    let v1 = self.s.pop().ok_or(RuntimeError::StackIsEmpty)?;
                    let v2 = self.s.pop().ok_or(RuntimeError::StackIsEmpty)?;
                    match (v1, v2) {
                        (Value::Const(v1), Value::Const(v2)) => {
                            let value = match prim {
                                Prim::Eq => {
                                    if v1 == v2 {
                                        Value::Const(Const::True)
                                    } else {
                                        Value::Const(Const::False)
                                    }
                                }
                                Prim::Add => Value::Const(Const::Int(v2 + v1)),
                                Prim::Sub => Value::Const(Const::Int(v2 - v1)),
                                Prim::Mul => Value::Const(Const::Int(v2 * v1)),
                                Prim::Div => Value::Const(Const::Int(v2 / v1)),
                            };
                            self.s.push(value);
                            Ok(None)
                        }
                        _ => {
                            eprintln!("Interpreter detected a error primitive function applied to non int types");
                            Err(RuntimeError::TypeError)
                        }
                    }
                }
                Instruction::If(c1, c2) => {
                    let v1 = self.s.pop().ok_or(RuntimeError::StackIsEmpty)?;
                    match v1 {
                        Value::Const(Const::True) => {
                            self.c.0.extend_from_slice(&c1.0);
                            Ok(None)
                        }

                        Value::Const(Const::False) => {
                            self.c.0.extend_from_slice(&c2.0);
                            Ok(None)
                        }
                        _ => {
                            eprintln!("Interpreter detected a error if expression's condition block contains non bool type expression ");
                            Err(RuntimeError::TypeError)
                        }
                    }
                }
                Instruction::Bind(name) => {
                    let value = self.s.last().ok_or(RuntimeError::StackIsEmpty)?;
                    self.e.0.insert(name, value.clone());
                    Ok(None)
                }
            }
        } else {
            Ok(self.s.pop())
        }
    }
    pub fn new() -> Self {
        Self {
            s: vec![],
            e: Environment(HashMap::new()),
            c: Code(vec![]),
            d: vec![],
        }
    }
    pub fn load_code(mut self, code: Code) -> Self {
        self.c = code;
        self
    }
    pub fn load_value(&mut self, id: Id, value: Value) {
        self.e.0.insert(id, value);
    }
}

pub fn code_gen(ast: Exp, mut code: Code) -> Code {
    match ast {
        Exp::ExpId(x) => {
            code.0.push(Instruction::Acc(x));
        }
        Exp::Int(x) => {
            code.0.push(Instruction::Push(Const::Int(x)));
        }
        Exp::String(x) => {
            code.0.push(Instruction::Push(Const::String(x)));
        }
        Exp::True => {
            code.0.push(Instruction::Push(Const::True));
        }
        Exp::False => code.0.push(Instruction::Push(Const::False)),
        Exp::ExpFn(x, c) => code.0.push(Instruction::MakeClosure(
            x,
            code_gen(c.as_ref().to_owned(), Code(vec![Instruction::Ret])),
        )),
        Exp::ExpApp(e1, e2) => {
            code.0.push(Instruction::App);
            let e2 = code_gen(e2.as_ref().clone(), code);
            code = code_gen(e1.as_ref().to_owned(), e2);
        }
        Exp::ExpPair(e1, e2) => {
            code.0.push(Instruction::Pair);
            let e2 = code_gen(e2.as_ref().clone(), code);
            code = code_gen(e1.as_ref().to_owned(), e2);
        }
        Exp::ExpProj1(e) => {
            code.0.push(Instruction::Proj1);
            code = code_gen(e.as_ref().clone(), code);
        }
        Exp::ExpProj2(e) => {
            code.0.push(Instruction::Proj2);
            code = code_gen(e.as_ref().clone(), code);
        }
        Exp::ExpPrim(prim, e1, e2) => {
            code.0.push(Instruction::Prim(prim));
            let e2 = code_gen(e2.as_ref().clone(), code);
            code = code_gen(e1.as_ref().to_owned(), e2);
        }
        Exp::ExpIf(e1, e2, e3) => {
            let e2 = code_gen(e2.as_ref().clone(), Code(vec![]));
            let e3 = code_gen(e3.as_ref().clone(), Code(vec![]));
            code.0.push(Instruction::If(e2, e3));
            code = code_gen(e1.as_ref().clone(), code);
        }
        Exp::ExpFix(f, x, e) => code.0.push(Instruction::MakeRecursiveClosure(
            f,
            x,
            code_gen(e.as_ref().to_owned(), Code(vec![Instruction::Ret])),
        )),
    }
    code
}
