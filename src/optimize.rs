//! optimizer for A-Normal form. This phase will redux
//! var<-int
//! let val x = if true then e1 else e2 in k -> let x = e1 in k end
//! let val x = if false then e1 else e2 in -> let x = e2 in k end

use std::collections::BTreeMap;

use crate::{
    alpha_unique::Var,
    anormalize::{ANExp, ANFunction},
    closureconversion::{CCValue, FunID},
};

fn env_map(var_env: &BTreeMap<Var, Var>, check: Var) -> Var {
    var_env
        .get(&check)
        .map(|v| {
            println!("{check} reduced to {v}");

            v.clone()
        })
        .unwrap_or(check)
}

fn is_used_var(exp: &ANExp, var: &Var) -> bool {
    match exp {
        ANExp::ExpIf {
            condition,
            then_exp,
            else_exp,
            result_var: _,
            next_exp,
        } => {
            condition == var
                || is_used_var(&then_exp, var)
                || is_used_var(&else_exp, var)
                || is_used_var(&next_exp, var)
        }
        ANExp::Value {
            value,
            result_var: _,
            next_exp,
        } => {
            if let CCValue::Var(v, _) = value {
                v == var || is_used_var(&next_exp, var)
            } else {
                is_used_var(&next_exp, var)
            }
        }
        ANExp::ExpApp {
            fptr,
            env,
            arg,
            result_var: _,
            next_exp,
            ty: _,
        } => fptr == var || env == var || arg == var || is_used_var(&next_exp, var),
        ANExp::ExpCall {
            f: _,
            arg,
            result_var: _,
            next_exp,
            ty: _,
        } => arg == var || is_used_var(&next_exp, var),
        ANExp::ExpPrim {
            op: _,
            v1,
            v2,
            ty: _,
            result_var: _,
            next_exp,
        } => v1 == var || v2 == var || is_used_var(&next_exp, var),
        ANExp::ExpSelect {
            label: _,
            arg,
            result_var: _,
            next_exp,
            ty: _,
        } => arg == var || is_used_var(&next_exp, var),
        ANExp::Record {
            fields,
            result_var: _,
            ty: _,
            next_exp,
        } => fields.values().any(|field| field == var) || is_used_var(&next_exp, var),
        ANExp::Return { v, ty: _ } => v == var,
        ANExp::Phi {
            result_var: _,
            left,
        } => left == var,
        ANExp::Bottom {} => false,
    }
}

pub fn remove_unused_def(exp: ANExp) -> ANExp {
    match exp {
        ANExp::ExpIf {
            condition,
            then_exp,
            else_exp,
            result_var,
            next_exp,
        } => {
            if is_used_var(&next_exp, &result_var) {
                ANExp::ExpIf {
                    condition,
                    then_exp: Box::new(remove_unused_def(*then_exp)),
                    else_exp: Box::new(remove_unused_def(*else_exp)),
                    result_var,
                    next_exp: Box::new(remove_unused_def(*next_exp)),
                }
            } else {
                remove_unused_def(*next_exp)
            }
        }
        ANExp::Value {
            value,
            result_var,
            next_exp,
        } => {
            if is_used_var(&next_exp, &result_var) {
                ANExp::Value {
                    value,
                    result_var,
                    next_exp: Box::new(remove_unused_def(*next_exp)),
                }
            } else {
                remove_unused_def(*next_exp)
            }
        }
        ANExp::ExpApp {
            fptr,
            env,
            arg,
            result_var,
            next_exp,
            ty,
        } => ANExp::ExpApp {
            fptr,
            env,
            arg,
            result_var,
            next_exp: Box::new(remove_unused_def(*next_exp)),
            ty,
        },
        ANExp::ExpCall {
            f,
            arg,
            result_var,
            next_exp,
            ty,
        } => ANExp::ExpCall {
            f,
            arg,
            result_var,
            next_exp: Box::new(remove_unused_def(*next_exp)),
            ty,
        },
        ANExp::ExpPrim {
            op,
            v1,
            v2,
            ty,
            result_var,
            next_exp,
        } => {
            if is_used_var(&next_exp, &result_var) {
                ANExp::ExpPrim {
                    op,
                    v1,
                    v2,
                    ty,
                    result_var,
                    next_exp: Box::new(remove_unused_def(*next_exp)),
                }
            } else {
                remove_unused_def(*next_exp)
            }
        }
        ANExp::ExpSelect {
            label,
            arg,
            result_var,
            next_exp,
            ty,
        } => {
            if is_used_var(&next_exp, &result_var) {
                ANExp::ExpSelect {
                    label,
                    arg,
                    result_var,
                    next_exp: Box::new(remove_unused_def(*next_exp)),
                    ty,
                }
            } else {
                remove_unused_def(*next_exp)
            }
        }
        ANExp::Record {
            fields,
            result_var,
            ty,
            next_exp,
        } => {
            if is_used_var(&next_exp, &result_var) {
                ANExp::Record {
                    fields,
                    result_var,
                    ty,
                    next_exp: Box::new(remove_unused_def(*next_exp)),
                }
            } else {
                remove_unused_def(*next_exp)
            }
        }

        exp => exp,
    }
}

pub fn optimize_var(exp: ANExp, mut var_env: BTreeMap<Var, Var>) -> (ANExp, BTreeMap<Var, Var>) {
    match exp {
        ANExp::Value {
            value,
            result_var,
            next_exp,
        } => match value {
            CCValue::Var(v, _) => {
                let v2 = v.clone();
                let result_var2 = result_var.clone();
                var_env.insert(result_var2, v);

                let (exp, env) = optimize_var(*next_exp, var_env);
                (exp.alpha_convert(&result_var, &v2), env)
            }
            value => {
                let (next_exp, var_env) = optimize_var(*next_exp, var_env);
                (
                    ANExp::Value {
                        value,
                        result_var,
                        next_exp: Box::new(next_exp),
                    },
                    var_env,
                )
            }
        },
        ANExp::ExpApp {
            fptr,
            env,
            arg,
            result_var,
            next_exp,
            ty,
        } => {
            let (next_exp, var_env) = optimize_var(*next_exp, var_env);
            let fptr = env_map(&var_env, fptr);

            let env = env_map(&var_env, env);

            let arg = env_map(&var_env, arg);

            (
                ANExp::ExpApp {
                    fptr: fptr.clone(),
                    env: env.clone(),
                    arg: arg.clone(),
                    result_var,
                    next_exp: Box::new(next_exp),
                    ty,
                },
                var_env,
            )
        }
        ANExp::ExpCall {
            f,
            arg,
            result_var,
            next_exp,
            ty,
        } => {
            let (next_exp, var_env) = optimize_var(*next_exp, var_env);
            let arg = env_map(&var_env, arg);

            (
                ANExp::ExpCall {
                    arg: arg.clone(),
                    result_var,
                    next_exp: Box::new(next_exp),
                    ty,
                    f,
                },
                var_env,
            )
        }
        ANExp::ExpIf {
            condition,
            then_exp,
            else_exp,
            result_var,
            next_exp,
        } => {
            let (next_exp, var_env) = optimize_var(*next_exp, var_env);

            let (then_exp, var_env) = optimize_var(*then_exp, var_env);

            let (else_exp, var_env) = optimize_var(*else_exp, var_env);

            let condition = env_map(&var_env, condition);
            (
                ANExp::ExpIf {
                    condition: condition.clone(),
                    then_exp: Box::new(then_exp),
                    else_exp: Box::new(else_exp),
                    result_var,
                    next_exp: Box::new(next_exp),
                },
                var_env,
            )
        }
        ANExp::ExpPrim {
            op,
            v1,
            v2,
            ty,
            result_var,
            next_exp,
        } => {
            let (next_exp, var_env) = optimize_var(*next_exp, var_env);
            let v1 = env_map(&var_env, v1);
            let v2 = env_map(&var_env, v2);
            (
                ANExp::ExpPrim {
                    op,
                    v1: v1.clone(),
                    v2: v2.clone(),
                    ty,
                    result_var,
                    next_exp: Box::new(next_exp),
                },
                var_env,
            )
        }
        ANExp::ExpSelect {
            label,
            arg,
            result_var,
            next_exp,
            ty,
        } => {
            let (next_exp, var_env) = optimize_var(*next_exp, var_env);
            let arg = env_map(&var_env, arg);
            (
                ANExp::ExpSelect {
                    label,
                    arg: arg.clone(),
                    result_var,
                    next_exp: Box::new(next_exp),
                    ty,
                },
                var_env,
            )
        }
        ANExp::Record {
            fields,
            result_var,
            ty,
            next_exp,
        } => {
            let (next_exp, var_env) = optimize_var(*next_exp, var_env);
            (
                ANExp::Record {
                    fields: fields
                        .into_iter()
                        .map(|(l, v)| (l, env_map(&var_env, v)))
                        .collect(),
                    result_var,
                    ty,
                    next_exp: Box::new(next_exp),
                },
                var_env,
            )
        }
        _ => (exp, var_env),
    }
}

pub fn optimize_functions(functions: BTreeMap<FunID, ANFunction>) -> BTreeMap<FunID, ANFunction> {
    functions
        .into_iter()
        .map(|(fid, f)| {
            let var_env = BTreeMap::new();
            println!("optimizing {fid}");
            (
                fid,
                ANFunction {
                    args: f.args,
                    inner_exp: remove_unused_def(optimize_var(f.inner_exp, var_env).0),
                    ty: f.ty,
                },
            )
        })
        .collect()
}
