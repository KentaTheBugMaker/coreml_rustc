use crate::syntax_tree::{ApplyExpression, AtomicExpression, Dec, Expression, Prim};
use chumsky::prelude::*;
use chumsky::Parser;

pub type Span = SimpleSpan<usize>;
#[derive(Debug, Clone, PartialEq)]
pub enum Token<'src> {
    String(&'src str),
    Add,
    Comma,
    DArrow,
    Div,
    Else,
    Eof,
    Eq,
    Equal,
    False,
    Fn,
    Fun,
    Hash1,
    Hash2,
    Id(&'src str),
    If,
    Int(i64),
    LParen,
    Mul,
    RParen,
    Semicolon,
    Sub,
    Then,
    True,
    Val,
}
pub fn lexer<'src>() -> impl Parser<'src, &'src str, Vec<(Token<'src>, Span)>> {
    use Token::*;
    let int = text::int(10).slice().from_str().unwrapped().map(Int);
    let string = just('"')
        .ignore_then(none_of('"').repeated())
        .then_ignore(just('"'))
        .map_slice(String);
    let id = regex("[a-zA-Z][a-zA-Z0-9]*").map_slice(|id: &str| match id {
        "add" => Add,
        "sub" => Sub,
        "mul" => Mul,
        "div" => Div,
        "fn" => Fn,
        "fun" => Fun,
        "val" => Val,
        "eq" => Eq,
        "if" => If,
        "then" => Then,
        "true" => True,
        "false" => False,
        "else" => Else,
        any => Id(any),
    });
    let hash1 = just("#1").to(Hash1);
    let hash2 = just("#2").to(Hash2);
    let lparen = just("(").to(LParen);
    let rparen = just(")").to(RParen);
    let comma = just(",").to(Comma);
    let equal = just("=").to(Equal);
    let darrow = just("=>").to(DArrow);
    let semicolon = just(";").to(Semicolon);
    let token = int
        .or(string)
        .or(id)
        .or(hash1)
        .or(hash2)
        .or(lparen)
        .or(rparen)
        .or(comma)
        .or(darrow)
        .or(equal)
        .or(semicolon);
    token
        .map_with_span(|tok, span| (tok, span))
        .padded()
        .recover_with(skip_then_retry_until(any().ignored(), end()))
        .repeated()
        .collect()
}

type ParserInput<'tokens, 'src> =
    chumsky::input::SpannedInput<Token<'src>, Span, &'tokens [(Token<'src>, Span)]>;
pub fn parse<'src>(
) -> impl Parser<'src, ParserInput<'src, 'src>, Dec, extra::Err<Rich<'src, Token<'src>, Span>>> + Clone
{
    let val_parser = just(Token::Val)
        .ignore_then(select! {
            Token::Id(name)=>name.to_owned()
        })
        .then_ignore(just(Token::Equal))
        .then(expr_parser())
        .map(|(name, exp)| Dec::Val {
            name,
            expression: Box::new(exp),
        });
    let fun_parser = just(Token::Fun)
        .ignore_then(select! {
            Token::Id(name)=>name.to_owned()
        })
        .then(select! {
            Token::Id(name)=>name.to_owned()
        })
        .then_ignore(just(Token::Equal))
        .then(expr_parser())
        .map(|((function_name, val_name), exp)| Dec::Fun {
            function_name,
            val_name,
            expression: Box::new(exp),
        });
    val_parser.or(fun_parser)
}
pub fn expr_parser<'src>(
) -> impl Parser<'src, ParserInput<'src, 'src>, Expression, extra::Err<Rich<'src, Token<'src>, Span>>>
       + Clone {
    recursive(|exp| {
        let app_exp = recursive(|appexp| {
            let at_exp = recursive(|at_exp| {
                let _const = select! {Token::True=>AtomicExpression::Const(crate::syntax_tree::Const::True),
                    Token::False=>AtomicExpression::Const(crate::syntax_tree::Const::False),
                    Token::Int(x)=>AtomicExpression::Const(crate::syntax_tree::Const::Int(x)),
                    Token::String(x)=>AtomicExpression::Const(crate::syntax_tree::Const::String(x.to_owned())),
                };

                let ident = select! { Token::Id(ident) => ident.to_owned() }
                    .labelled("identifier")
                    .map(|name| AtomicExpression::Id(name));

                let tuple = just(Token::LParen)
                    .ignore_then(exp.clone())
                    .then_ignore(just(Token::Comma))
                    .then(exp.clone())
                    .then_ignore(just(Token::RParen))
                    .map(|(a, b)| AtomicExpression::Pair(Box::new(a), Box::new(b)));

                let nested = just(Token::LParen)
                    .ignore_then(exp.clone())
                    .then_ignore(just(Token::RParen))
                    .map(|expr| AtomicExpression::Expression(Box::new(expr)));

                let proj = select! {
                    Token::Hash1=>1,
                    Token::Hash2=>2,
                }
                .labelled("destruct tuple")
                .then(at_exp.clone())
                .map(|(accessor, exp)| match accessor {
                    1 => AtomicExpression::ExtractFirst(Box::new(exp)),
                    2 => AtomicExpression::ExtractSecond(Box::new(exp)),
                    x => unimplemented!("tuple accessor is not implemented for {x:?}"),
                });

                let prim = choice((
                    just(Token::Add),
                    just(Token::Sub),
                    just(Token::Mul),
                    just(Token::Div),
                    just(Token::Eq),
                ))
                .then_ignore(just(Token::LParen))
                .then(exp.clone())
                .then_ignore(just(Token::Comma))
                .then(exp.clone())
                .then_ignore(just(Token::RParen))
                .map(|((operator, exp1), exp2)| {
                    println!("operator {operator:?} detected");
                    AtomicExpression::Prim(
                        match operator {
                            Token::Add => Prim::Add,
                            Token::Div => Prim::Div,
                            Token::Sub => Prim::Sub,
                            Token::Mul => Prim::Mul,
                            Token::Eq => Prim::Eq,
                            x => unreachable!(" {x:?} is not primitive operation"),
                        },
                        Box::new(exp1),
                        Box::new(exp2),
                    )
                });
                choice((_const, ident, tuple, nested, proj, prim))
            });

            at_exp
                .clone()
                .repeated()
                .at_least(1)
                .collect()
                .map(|apply| ApplyExpression(apply))
            /*
            at_exp
                .clone()
                .map(|at_exp| ApplyExpression::AtExp(at_exp))
                .or(appexp
                    .then(at_exp)
                    .map(|(appexp, atexp)| ApplyExpression::Apply(Box::new(appexp), atexp)))
            */
            /*
            choice((
                at_exp.clone().map(|at_exp| ApplyExpression::AtExp(at_exp)),
                appexp
                    .then(at_exp)
                    .map(|(appexp, atexp)| ApplyExpression::Apply(Box::new(appexp), atexp)),
            ))*/
        });
        let if_exp = just(Token::If)
            .ignore_then(exp.clone())
            .then_ignore(just(Token::Then))
            .then(exp.clone())
            .then_ignore(just(Token::Else))
            .then(exp.clone())
            .map(|((cond, then), els)| Expression::If {
                condition: Box::new(cond),
                then_section: Box::new(then),
                else_section: Box::new(els),
            });
        let fn_expression = just(Token::Fn)
            .ignore_then(select! {Token::Id(ident)=>ident.to_owned()})
            .then_ignore(just(Token::DArrow))
            .then(exp.clone())
            .map(|(var, exp)| Expression::Fn {
                var,
                expression: Box::new(exp),
            });
        if_exp
            .or(fn_expression)
            .or(app_exp.map(|appexp| Expression::AppExp(Box::new(appexp))))
            /*
            choice((
                app_exp.map(|appexp| Expression::AppExp(Box::new(appexp))),
                if_exp,
                fn_expression,
            ))*/
            .map(|result| {
                println!("parsed {result:?}");
                result
            })
    })
}
