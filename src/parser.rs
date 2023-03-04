use nom::{
    bytes::complete::{is_not, tag},
    character::complete::{alpha1, alphanumeric0, char, digit1, multispace0, multispace1},
    sequence::tuple,
    IResult,
};
use nom_recursive::{recursive_parser, RecursiveInfo};

use crate::syntax_tree::Prim;
pub type Span<'a> = nom_locate::LocatedSpan<&'a str, RecursiveInfo>;
pub fn parse(s: Span) -> IResult<Span, crate::syntax_tree::Top> {
    parse_dec(s).map(|(remain, dec)| (remain, crate::syntax_tree::Top::Dec(dec)))
}

fn parse_dec(s: Span) -> IResult<Span, crate::syntax_tree::Dec> {
    nom::branch::alt((parse_val, parse_fun))(s)
}

fn parse_val(s: Span) -> IResult<Span, crate::syntax_tree::Dec> {
    nom::sequence::tuple((
        tag("val"),
        multispace1,
        parse_ident,
        multispace0,
        char('='),
        multispace0,
        parse_exp,
    ))(s)
    .map(|(remain, consumed)| {
        (
            remain,
            crate::syntax_tree::Dec::Val {
                name: consumed.2,
                expression: Box::new(consumed.6),
            },
        )
    })
}
fn parse_fun(s: Span) -> IResult<Span, crate::syntax_tree::Dec> {
    nom::sequence::tuple((tag("fun"), parse_ident, parse_ident, char('='), parse_exp))(s).map(
        |(remain, consumed)| {
            (
                remain,
                crate::syntax_tree::Dec::Fun {
                    function_name: consumed.1,
                    val_name: consumed.2,
                    expression: Box::new(consumed.4),
                },
            )
        },
    )
}

fn parse_exp(s: Span) -> IResult<Span, crate::syntax_tree::Expression> {
    let if_or_fn = nom::branch::alt((parse_fn, parse_if))(s);
    if if_or_fn.is_ok() {
        if_or_fn
    } else {
        parse_appexp(s).map(|(remain, appexp)| {
            (
                remain,
                crate::syntax_tree::Expression::AppExp(Box::new(appexp)),
            )
        })
    }
}

fn parse_if(s: Span) -> IResult<Span, crate::syntax_tree::Expression> {
    nom::sequence::tuple((
        tag("if"),
        multispace1,
        parse_exp,
        multispace1,
        tag("then"),
        multispace1,
        parse_exp,
        multispace1,
        tag("else"),
        multispace1,
        parse_exp,
    ))(s)
    .map(|(remain, consumed)| {
        (
            remain,
            crate::syntax_tree::Expression::If {
                condition: Box::new(consumed.2),
                then_section: Box::new(consumed.6),
                else_section: Box::new(consumed.10),
            },
        )
    })
}

fn parse_fn(s: Span) -> IResult<Span, crate::syntax_tree::Expression> {
    nom::sequence::tuple((
        tag("fn"),
        multispace1,
        parse_ident,
        multispace0,
        tag("=>"),
        multispace0,
        parse_exp,
    ))(s)
    .map(|(remain, consumed)| {
        (
            remain,
            crate::syntax_tree::Expression::Fn {
                var: consumed.2,
                expression: Box::new(consumed.6),
            },
        )
    })
}
#[recursive_parser]
fn parse_appexp(s: Span) -> IResult<Span, crate::syntax_tree::ApplyExpression> {
    let appexp = nom::sequence::tuple((parse_appexp, parse_atexp))(s);
    if appexp.is_ok() {
        appexp.map(|(remain, consumed)| {
            (
                remain,
                crate::syntax_tree::ApplyExpression::Apply(Box::new(consumed.0), consumed.1),
            )
        })
    } else {
        parse_atexp(s).map(|(remain, consumed)| {
            (remain, crate::syntax_tree::ApplyExpression::AtExp(consumed))
        })
    }
}

fn parse_atexp(s: Span) -> IResult<Span, crate::syntax_tree::AtomicExpression> {
    nom::branch::alt((
        parse_const,
        parse_id,
        parse_pair,
        parse_nested_expression,
        parse_extract,
        parse_primitive_apply,
    ))(s)
}

fn parse_pair(s: Span) -> IResult<Span, crate::syntax_tree::AtomicExpression> {
    nom::sequence::tuple((
        tag("("),
        multispace0,
        parse_exp,
        multispace0,
        tag(","),
        multispace0,
        parse_exp,
        multispace0,
        tag(")"),
    ))(s)
    .map(|(remain, consumed)| {
        (
            remain,
            crate::syntax_tree::AtomicExpression::Pair(Box::new(consumed.2), Box::new(consumed.6)),
        )
    })
}

fn parse_nested_expression(s: Span) -> IResult<Span, crate::syntax_tree::AtomicExpression> {
    nom::sequence::tuple((tag("("), parse_exp, tag(")")))(s).map(|(remain, consumed)| {
        (
            remain,
            crate::syntax_tree::AtomicExpression::Expression(Box::new(consumed.1)),
        )
    })
}

fn parse_extract(s: Span) -> IResult<Span, crate::syntax_tree::AtomicExpression> {
    nom::sequence::tuple((
        nom::branch::alt((tag("#1"), tag("#2"))),
        multispace0,
        parse_atexp,
    ))(s)
    .map(|(remain, (accessor, _, atomic_expression))| {
        (
            remain,
            match accessor.trim() {
                "#1" => {
                    crate::syntax_tree::AtomicExpression::ExtractFirst(Box::new(atomic_expression))
                }
                "#2" => {
                    crate::syntax_tree::AtomicExpression::ExtractSecond(Box::new(atomic_expression))
                }
                _ => {
                    unreachable!("")
                }
            },
        )
    })
}

fn parse_id(s: Span) -> IResult<Span, crate::syntax_tree::AtomicExpression> {
    parse_ident(s)
        .map(|(remain, consumed)| (remain, crate::syntax_tree::AtomicExpression::Id(consumed)))
}

fn parse_const(s: Span) -> IResult<Span, crate::syntax_tree::AtomicExpression> {
    nom::branch::alt((parse_int, parse_string, parse_true, parse_false))(s).map(
        |(remain, consumed)| {
            (
                remain,
                crate::syntax_tree::AtomicExpression::Const(consumed),
            )
        },
    )
}

fn parse_int(s: Span) -> IResult<Span, crate::syntax_tree::Const> {
    digit1(s).map(|(remain, consumed)| {
        (
            remain,
            crate::syntax_tree::Const::Int(consumed.parse().unwrap()),
        )
    })
}
fn parse_string(s: Span) -> IResult<Span, crate::syntax_tree::Const> {
    nom::sequence::tuple((tag("\""), is_not("\""), tag("\"")))(s).map(|(remain, consumed)| {
        (
            remain,
            crate::syntax_tree::Const::String(consumed.1.to_string()),
        )
    })
}

fn parse_true(s: Span) -> IResult<Span, crate::syntax_tree::Const> {
    tag("true")(s).map(|(remain, _)| ((remain, crate::syntax_tree::Const::True)))
}
fn parse_false(s: Span) -> IResult<Span, crate::syntax_tree::Const> {
    tag("false")(s).map(|(remain, _)| ((remain, crate::syntax_tree::Const::False)))
}

fn parse_ident(s: Span) -> IResult<Span, crate::syntax_tree::Id> {
    tuple((alpha1, alphanumeric0))(s).map(|(remain, (first, second))| {
        let mut id = first.to_string();
        id.push_str(&second.to_string());
        (remain, id)
    })
}

fn parse_prim(s: Span) -> IResult<Span, crate::syntax_tree::Prim> {
    nom::branch::alt((tag("eq"), tag("add"), tag("sub"), tag("mul"), tag("div")))(s).map(
        |(remain, operator)| {
            (
                remain,
                match operator.trim() {
                    "eq" => Prim::Eq,
                    "add" => Prim::Add,
                    "sub" => Prim::Sub,
                    "mul" => Prim::Mul,
                    "div" => Prim::Div,
                    _ => unreachable!("Other operator is not implemented for coreml"),
                },
            )
        },
    )
}

fn parse_primitive_apply(s: Span) -> IResult<Span, crate::syntax_tree::AtomicExpression> {
    nom::sequence::tuple((
        parse_prim,
        multispace0,
        char('('),
        multispace0,
        parse_exp,
        multispace0,
        char(','),
        multispace0,
        parse_exp,
        multispace0,
        char(')'),
    ))(s)
    .map(|(remain, (primtive, _, _, _, exp1, _, _, _, exp2, _, _))| {
        (
            remain,
            crate::syntax_tree::AtomicExpression::Prim(primtive, Box::new(exp1), Box::new(exp2)),
        )
    })
}
