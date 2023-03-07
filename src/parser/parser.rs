use nom::{
    branch::alt,
    bytes::complete::tag,
    combinator::map,
    error::{context, make_error, ContextError, Error, ErrorKind, ParseError, VerboseError},
    sequence::separated_pair,
    IResult,
};

use super::{
    lexer::{Token, Tokenable},
    utils::{spanned, Span, Spanned},
};

enum Filter {
    Eq,   // =
    Neq,  // !=
    Req,  // =~
    NReq, // !~
}

// let mappings = (
//     filter_p("=".delimiter(), Filter::Eq),
//     filter_p("!=".delimiter(), Filter::Neq),
//     filter_p("=~".delimiter(), Filter::Req),
//     filter_p("!~".delimiter(), Filter::NReq),
// );

// context("filter", alt(mappings))(i)

fn just<'a, I, E>(tag: I) -> impl Fn(Spanned<'a, I>) -> IResult<Spanned<'a, I>, Spanned<'a, I>, E>
where
    I: PartialEq + Clone,
    E: ParseError<&'a str>,
    nom::Err<E>: nom::error::ParseError<&'a str>,
{
    move |(s, tok)| match tok {
        tok if tok == tag => Ok(((s, tok.clone()), (s, tok))),
        _ => Err(make_error(*s.fragment(), ErrorKind::Tag)),
    }
}
