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
pub enum Filter {
    Eq,   // |=
    Neq,  // !=
    Req,  // =~
    NReq, // !~
}

pub fn parse_filter<'a, E>(i: TokenStream<'a>) -> IResult<TokenStream<'a>, Spanned<'a, Filter>, E>
where
    E: ParseError<TokenStream<'a>> + ContextError<TokenStream<'a>>,
{
    let mappings = (
        map(just("|=".delimiter()), |x| x.map_v(|_| Filter::Eq)),
        map(just("!=".delimiter()), |x| x.map_v(|_| Filter::Neq)),
        map(just("=~".delimiter()), |x| x.map_v(|_| Filter::Req)),
        map(just("!~".delimiter()), |x| x.map_v(|_| Filter::NReq)),
    );

    context("filter", alt(mappings))(i)
}

// token, stream, spannedtoken, e
pub fn just<'a, T, Input, Error>(tag: T) -> impl Fn(Input) -> IResult<Input, Spanned<'a, T>, Error>
where
    Input: Head<Item = Spanned<'a, T>> + InputTake,
    T: PartialEq + InputLength,
    Error: ParseError<Input>,
{
    move |i: Input| {
        if let Some(found) = i.head() {
            if found.value == tag {
                let tag_len = tag.input_len();
                let (rest, _) = i.take_split(tag_len);
                return Ok((rest, found));
            }
        };
        let e: ErrorKind = ErrorKind::Tag;
        Err(nom::Err::Error(Error::from_error_kind(i, e)))
    }
}

// MatcherType is the type of matcher. It can be equality, regex, etc.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MatcherType {
    Eq,  // =
    Neq, // !=
    Re,  // =~
    Nre, // !~
}

pub fn parse_matcher_type<'a, E>(
    input: TokenStream<'a>,
) -> IResult<TokenStream<'a>, Spanned<'a, MatcherType>, E>
where
    E: ParseError<TokenStream<'a>> + ContextError<TokenStream<'a>>,
{
    let mappings = (
        map(just("=".delimiter()), |x| x.map_v(|_| MatcherType::Eq)),
        map(just("!=".delimiter()), |x| x.map_v(|_| MatcherType::Neq)),
        map(just("=~".delimiter()), |x| x.map_v(|_| MatcherType::Re)),
        map(just("!~".delimiter()), |x| x.map_v(|_| MatcherType::Nre)),
    );
    context("matcher_type", alt(mappings))(input)
}

#[cfg(test)]
#[test]
fn test_parse_filter() {
    let input = Span::new("|=");
    let (_, toks) = super::lexer::lex::<VerboseError<Span>>(input).unwrap();

    let ts = TokenStream::new(&toks);

    let (_, f) = parse_filter::<VerboseError<_>>(ts).unwrap();
    assert_eq!(Filter::Eq, *f)
}
