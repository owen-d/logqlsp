use nom::{
    branch::alt,
    bytes::complete::tag,
    combinator::map,
    error::{context, make_error, ContextError, Error, ErrorKind, ParseError, VerboseError},
    sequence::separated_pair,
    Compare, CompareResult, IResult, InputLength, InputTake,
};

use super::{
    lexer::{Head, Token, TokenStream, Tokenable},
    utils::{spanned, Span, Spanned},
};

#[derive(Debug, Clone, PartialEq)]
enum Filter {
    Eq,   // =
    Neq,  // !=
    Req,  // =~
    NReq, // !~
}

pub fn parse_filter<'a, E>(i: TokenStream<'a>) -> IResult<TokenStream<'a>, Spanned<'a, Filter>, E>
where
    E: ParseError<TokenStream<'a>> + ContextError<TokenStream<'a>>,
    nom::Err<E>: nom::error::ParseError<TokenStream<'a>>,
{
    let mappings = (
        parse_filter_variant("=".delimiter(), Filter::Eq),
        parse_filter_variant("!=".delimiter(), Filter::Neq),
        parse_filter_variant("=~".delimiter(), Filter::Req),
        parse_filter_variant("!~".delimiter(), Filter::NReq),
    );

    context("filter", alt(mappings))(i)
}

fn parse_filter_variant<'a, E>(
    from: Token,
    to: Filter,
) -> impl Fn(TokenStream<'a>) -> IResult<TokenStream<'a>, Spanned<'a, Filter>, E>
where
    E: ParseError<TokenStream<'a>>,
    nom::Err<E>: nom::error::ParseError<TokenStream<'a>>,
{
    move |i| {
        let (s, (sp, _)) = just(from.clone())(i)?;
        Ok((s, (sp, to.clone())))
    }
}

// token, stream, spannedtoken, e
pub fn just<'a, T, Input, Error>(tag: T) -> impl Fn(Input) -> IResult<Input, Spanned<'a, T>, Error>
where
    Input: Head<Item = Spanned<'a, T>> + InputTake,
    T: PartialEq + InputLength,
    Error: ParseError<Input>,
{
    move |i: Input| {
        if let Some((sp, found)) = i.head() {
            if found == tag {
                let tag_len = tag.input_len();
                let (rest, _) = i.take_split(tag_len);
                return Ok((rest, (sp, found)));
            }
        };
        let e: ErrorKind = ErrorKind::Tag;
        Err(nom::Err::Error(Error::from_error_kind(i, e)))
    }
}