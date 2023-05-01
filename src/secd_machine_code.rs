//! SECD Machine Code generator and interpreter
//!

use std::collections::HashMap;

use crate::{
    syntax_tree::{Const, Id, Prim},
    typed_ast::TypedExp,
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
    BoolEq,
    IntEq,
    StringEq,
    IntAdd,
    IntSub,
    IntMul,
    IntDiv,

    If(Code, Code),
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
                Instruction::BoolEq => {
                    let v1 = self.s.pop().ok_or(RuntimeError::StackIsEmpty)?;
                    let v2 = self.s.pop().ok_or(RuntimeError::StackIsEmpty)?;
                    match (v1, v2) {
                        (Value::Const(v1), Value::Const(v2)) => {
                            let value = if v1 == v2 {
                                Value::Const(Const::True)
                            } else {
                                Value::Const(Const::False)
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
                Instruction::IntEq => {
                    let v1 = self.s.pop().ok_or(RuntimeError::StackIsEmpty)?;
                    let v2 = self.s.pop().ok_or(RuntimeError::StackIsEmpty)?;
                    match (v1, v2) {
                        (Value::Const(v1), Value::Const(v2)) => {
                            let value = if v1 == v2 {
                                Value::Const(Const::True)
                            } else {
                                Value::Const(Const::False)
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
                Instruction::StringEq => {
                    let v1 = self.s.pop().ok_or(RuntimeError::StackIsEmpty)?;
                    let v2 = self.s.pop().ok_or(RuntimeError::StackIsEmpty)?;
                    match (v1, v2) {
                        (Value::Const(v1), Value::Const(v2)) => {
                            let value = if v1 == v2 {
                                Value::Const(Const::True)
                            } else {
                                Value::Const(Const::False)
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
                Instruction::IntAdd => {
                    let v1 = self.s.pop().ok_or(RuntimeError::StackIsEmpty)?;
                    let v2 = self.s.pop().ok_or(RuntimeError::StackIsEmpty)?;
                    match (v1, v2) {
                        (Value::Const(v1), Value::Const(v2)) => {
                            let value = match (v1, v2) {
                                (Const::Int(v1), Const::Int(v2)) => {
                                    Value::Const(Const::Int(v1 + v2))
                                }
                                _ => unreachable!("Error"),
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
                Instruction::IntSub => {
                    let v1 = self.s.pop().ok_or(RuntimeError::StackIsEmpty)?;
                    let v2 = self.s.pop().ok_or(RuntimeError::StackIsEmpty)?;
                    match (v1, v2) {
                        (Value::Const(v1), Value::Const(v2)) => {
                            let value = match (v1, v2) {
                                (Const::Int(v1), Const::Int(v2)) => {
                                    Value::Const(Const::Int(v1 - v2))
                                }
                                _ => unreachable!("Error"),
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
                Instruction::IntMul => {
                    let v1 = self.s.pop().ok_or(RuntimeError::StackIsEmpty)?;
                    let v2 = self.s.pop().ok_or(RuntimeError::StackIsEmpty)?;
                    match (v1, v2) {
                        (Value::Const(v1), Value::Const(v2)) => {
                            let value = match (v1, v2) {
                                (Const::Int(v1), Const::Int(v2)) => {
                                    Value::Const(Const::Int(v1 * v2))
                                }
                                _ => unreachable!("Error"),
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
                Instruction::IntDiv => {
                    let v1 = self.s.pop().ok_or(RuntimeError::StackIsEmpty)?;
                    let v2 = self.s.pop().ok_or(RuntimeError::StackIsEmpty)?;
                    match (v1, v2) {
                        (Value::Const(v1), Value::Const(v2)) => {
                            let value = match (v1, v2) {
                                (Const::Int(v1), Const::Int(v2)) => {
                                    Value::Const(Const::Int(v1 / v2))
                                }
                                _ => unreachable!("Error"),
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

pub fn code_gen(ast: TypedExp, mut code: Code) -> Code {
    match &ast {
        TypedExp::ExpId(x, _) => {
            code.0.push(Instruction::Acc(x.to_owned()));
        }
        TypedExp::Int(x) => {
            code.0.push(Instruction::Push(Const::Int(*x)));
        }
        TypedExp::String(x) => {
            code.0.push(Instruction::Push(Const::String(x.to_owned())));
        }
        TypedExp::True => {
            code.0.push(Instruction::Push(Const::True));
        }
        TypedExp::False => code.0.push(Instruction::Push(Const::False)),
        TypedExp::ExpFn(x, c, _) => code.0.push(Instruction::MakeClosure(
            x.to_owned(),
            code_gen(c.as_ref().to_owned(), Code(vec![Instruction::Ret])),
        )),
        TypedExp::ExpApp(e1, e2, _) => {
            code.0.push(Instruction::App);
            let e2 = code_gen(e2.as_ref().clone(), code);
            code = code_gen(e1.as_ref().to_owned(), e2);
        }
        TypedExp::ExpPair(e1, e2, _) => {
            code.0.push(Instruction::Pair);
            let e2 = code_gen(e2.as_ref().clone(), code);
            code = code_gen(e1.as_ref().to_owned(), e2);
        }
        TypedExp::ExpProj1(e, _) => {
            code.0.push(Instruction::Proj1);
            code = code_gen(e.as_ref().clone(), code);
        }
        TypedExp::ExpProj2(e, _) => {
            code.0.push(Instruction::Proj2);
            code = code_gen(e.as_ref().clone(), code);
        }
        TypedExp::ExpPrim(prim, e1, e2, _) => {
            use crate::typeinf::Type;

            let ty_e1 = e1.ty();
            let ty_e2 = e2.ty();
            let op = match (ty_e1, ty_e2, prim) {
                (Type::Int, Type::Int, Prim::Eq) => Instruction::IntEq,
                (Type::Int, Type::Int, Prim::Add) => Instruction::IntAdd,
                (Type::Int, Type::Int, Prim::Sub) => Instruction::IntSub,
                (Type::Int, Type::Int, Prim::Mul) => Instruction::IntMul,
                (Type::Int, Type::Int, Prim::Div) => Instruction::IntDiv,
                (Type::Bool, Type::Bool, Prim::Eq) => Instruction::BoolEq,
                (Type::String, Type::String, Prim::Eq) => Instruction::StringEq,
                (a, b, op) => {
                    unimplemented!(
                        "Primitive operation {:?} is not implemented for {} {}",
                        op,
                        a.to_string(),
                        b.to_string(),
                    )
                }
            };
            code.0.push(op);
            let e2 = code_gen(e2.as_ref().clone(), code);
            code = code_gen(e1.as_ref().to_owned(), e2);
        }
        TypedExp::ExpIf(e1, e2, e3) => {
            let e2 = code_gen(e2.as_ref().clone(), Code(vec![]));
            let e3 = code_gen(e3.as_ref().clone(), Code(vec![]));
            code.0.push(Instruction::If(e2, e3));
            code = code_gen(e1.as_ref().clone(), code);
        }
        TypedExp::ExpFix(f, x, e, _) => code.0.push(Instruction::MakeRecursiveClosure(
            f.to_owned(),
            x.to_owned(),
            code_gen(e.as_ref().to_owned(), Code(vec![Instruction::Ret])),
        )),
    }
    code
}
