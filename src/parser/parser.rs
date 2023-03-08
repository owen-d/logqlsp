use nom::{
    branch::alt,
    bytes::complete::tag,
    combinator::map,
    error::{context, make_error, ContextError, Error, ErrorKind, ParseError, VerboseError},
    sequence::{separated_pair, Tuple},
    Compare, CompareResult, IResult, InputLength, InputTake,
};

use super::{
    lexer::{Delimited, Head, Token, TokenStream, Tokenable},
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

// LabelMatcher is a matcher for a label.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LabelMatcher<'a> {
    pub name: Spanned<'a, String>,
    pub value: Spanned<'a, String>,
    pub matcher_type: Spanned<'a, MatcherType>,
}

pub fn parse_label_matcher<'a, E>(
    input: TokenStream<'a>,
) -> IResult<TokenStream<'a>, Spanned<'a, LabelMatcher>, E>
where
    E: ParseError<TokenStream<'a>> + ContextError<TokenStream<'a>>,
{
    let mut p = context("label_matcher", |i| {
        let label_name = context("label_name", parse_identifier);
        let label_value = context("label_value", parse_string);
        (label_name, parse_matcher_type, label_value).parse(i)
    });
    let (s, (name, matcher, val)) = p(input)?;
    // copy the name+span to include in return value while reusing the span for the labelmatcher
    let name_cpy = name.clone();
    Ok((
        s,
        name.map_v(|_| LabelMatcher {
            name: name_cpy,
            matcher_type: matcher,
            value: val,
        }),
    ))
}

// macro to generate parsers that match a specific token variant
#[macro_export]
macro_rules! impl_token_type_parser {
    ($name: ident, $i:ident, $return_ty:ty, $extractor:expr) => {
        pub(crate) fn $name<'a, E>(
            input: TokenStream<'a>,
        ) -> IResult<TokenStream<'a>, Spanned<'a, $return_ty>, E>
        where
            E: ParseError<TokenStream<'a>> + ContextError<TokenStream<'a>>,
        {
            let f = move |input: TokenStream<'a>| {
                if let Some(head) = input.head() {
                    if let Token::$i(x) = head.clone().value {
                        let len = head.input_len();
                        let (rest, _) = input.take_split(len);
                        return Ok((rest, head.map_v(|_| $extractor(x))));
                    }
                }
                let e: ErrorKind = ErrorKind::Tag;
                Err(nom::Err::Error(E::from_error_kind(input, e)))
            };
            context("$i", f)(input)
        }
    };
}

impl_token_type_parser!(parse_comment, Comment, String, |x| x);
impl_token_type_parser!(parse_string, String, String, |x| x);
impl_token_type_parser!(parse_identifier, Identifier, String, |x| x);
impl_token_type_parser!(parse_delimiter, Delimiter, Delimited<String>, |x| x);

#[cfg(test)]
#[test]
fn test_parse_filter() {
    let input = Span::new("|=");
    let (_, toks) = super::lexer::lex::<VerboseError<Span>>(input).unwrap();

    let ts = TokenStream::new(&toks);

    let (_, f) = parse_filter::<VerboseError<_>>(ts).unwrap();
    assert_eq!(Filter::Eq, *f)
}

#[cfg(test)]
#[test]
fn test_parse_label_matcher() {
    let input = Span::new(r#"foo="bar""#);
    let (_, toks) = super::lexer::lex::<VerboseError<Span>>(input).unwrap();

    let ts = TokenStream::new(&toks);

    let (_, f) = parse_label_matcher::<VerboseError<_>>(ts).unwrap();
    let m = f.value;
    assert_eq!("foo".to_string(), *m.name);
    assert_eq!(r#""bar""#.to_string(), *m.value);
    assert_eq!(MatcherType::Eq, *m.matcher_type);
}
