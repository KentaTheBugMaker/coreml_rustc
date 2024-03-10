//! emit javascript from ANormalized-IR.

use std::collections::BTreeMap;

use crate::{
    anormalize::{ANExp, ANFunction},
    closureconversion::{CCValue, FunID},
};
pub fn emit_value(value: &CCValue) -> String {
    match value {
        crate::closureconversion::CCValue::Var(v, ty) => {
            format!("var_{} /* ExpVar {ty} */", v.id)
        }
        crate::closureconversion::CCValue::Int(x) => format!("{}", x),
        crate::closureconversion::CCValue::String(x) => format!("{x:?}"),
        crate::closureconversion::CCValue::True => "true".to_owned(),
        crate::closureconversion::CCValue::False => "false".to_owned(),
        crate::closureconversion::CCValue::FPtr(lambda, ty) => {
            format!("{lambda} /* Function Pointer {ty} */")
        }
    }
}
pub fn emit_exp(exp: &ANExp) -> String {
    match exp {
        ANExp::ExpIf {
            condition,
            then_exp,
            else_exp,
            result_var,
            next_exp,
        } => {
            /*
               if ()
            */
            format!(
                "
            let var_{};
            if (var_{}){{ {} }} else {{ {} }} {}",
                result_var.id,
                condition.id,
                emit_exp(then_exp),
                emit_exp(else_exp),
                emit_exp(next_exp)
            )
        }
        ANExp::Value {
            value,
            result_var,
            next_exp,
        } => format!(
            "let var_{} = {};/* Value */
        {}",
            result_var.id,
            emit_value(value),
            emit_exp(next_exp)
        ),
        ANExp::ExpApp {
            fptr,
            env,
            arg,
            result_var,
            next_exp,
            ty,
        } => {
            format!(
                "let var_{} = var_{}(var_{},var_{});/* ExpApp {ty} */
            {}",
                result_var.id,
                fptr.id,
                env.id,
                arg.id,
                emit_exp(next_exp)
            )
        }
        ANExp::ExpCall {
            f,
            arg,
            result_var,
            next_exp,
            ty,
        } => {
            format!(
                "let var_{} = {}(var_{});/* ExpCall {ty}*/
            {}",
                result_var.id,
                f,
                arg.id,
                emit_exp(next_exp)
            )
        }
        ANExp::ExpPrim {
            op,
            v1,
            v2,
            ty,
            result_var,
            next_exp,
        } => format!(
            "let var_{} = var_{} {} var_{}; /* ExpPrim {ty}*/
        {}",
            result_var.id,
            v1.id,
            match op {
                crate::flat_syntax::Prim::Add => "+",
                crate::flat_syntax::Prim::Eq => "==",
                crate::flat_syntax::Prim::Sub => "-",
                crate::flat_syntax::Prim::Div => "/",
                crate::flat_syntax::Prim::Mul => "*",
            },
            v2.id,
            emit_exp(next_exp)
        ),
        ANExp::ExpSelect {
            label,
            arg,
            result_var,
            next_exp,
            ty,
        } => format!(
            "let var_{} = (var_{}).{}; /* {ty} */
        {}",
            result_var.id,
            arg.id,
            label,
            emit_exp(next_exp)
        ),
        ANExp::Record {
            fields,
            result_var,
            ty,
            next_exp,
        } => format!(
            "let var_{} = {{{}}}; /* Record {ty} */
            {}",
            result_var.id,
            fields
                .iter()
                .map(|(label, var)| { format!("{label} : var_{}", var.id) })
                .reduce(|f1, f2| { f1 + ",\n" + &f2 })
                .unwrap_or_default(),
            emit_exp(next_exp)
        ),
        ANExp::Return { v, ty } => {
            format!("return var_{}; /* Return {ty} */", v.id)
        }
        ANExp::Phi { result_var, left } => {
            format!("var_{} = var_{}; /* Phi Node */", result_var.id, left.id)
        }
        ANExp::Bottom {} => format!("/* No Continuation */"),
    }
}

pub fn emit_functions(functions: BTreeMap<FunID, ANFunction>) -> String {
    functions
        .into_iter()
        .map(|(fun_id, function)| {
            let args = function.args;
            let exp = function.inner_exp;

            format!(
                "function {fun_id} ({}){{{}}}",
                args.iter()
                    .map(|(v, _)| { format!("var_{} /* {} */", v.id, v.name) })
                    .reduce(|a1, a2| { a1 + "," + &a2 })
                    .unwrap_or_default(),
                emit_exp(&exp)
            )
        })
        .reduce(|f1, f2| f1 + "\n" + &f2)
        .unwrap_or_default()
}
