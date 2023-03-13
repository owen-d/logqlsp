use nom::{
    branch::alt,
    bytes::complete::tag,
    combinator::{map, opt, peek},
    error::{context, make_error, ContextError, Error, ErrorKind, ParseError, VerboseError},
    multi::separated_list0,
    sequence::{delimited, separated_pair, terminated, Tuple},
    Compare, CompareResult, IResult, InputLength, InputTake, Parser,
};
use nom_locate::position;

use super::{
    lexer::{Delimited, Head, Token, TokenStream, Tokenable},
    pipeline::{parse_pipeline_expr, PipelineExpr},
    utils::{spanned, RefSpanned, Span, Spanned},
};

#[derive(Debug, Clone, PartialEq)]
pub struct LogExpr<S> {
    pub selector: Spanned<S, Selector<S>>,
    pipeline: Option<Spanned<S, PipelineExpr<S>>>,
}

pub fn parse_log_expr<'a, E>(
    input: TokenStream<'a>,
) -> IResult<TokenStream<'a>, RefSpanned<'a, LogExpr<Span<'a>>>, E>
where
    E: ParseError<TokenStream<'a>> + ContextError<TokenStream<'a>>,
{
    let (input, selector) = parse_selector(input)?;
    let (input, pipeline) = opt(parse_pipeline_expr)(input)?;
    Ok((
        input,
        Spanned::from((selector.span.clone(), LogExpr { selector, pipeline })),
    ))
}

#[derive(Debug, Clone, PartialEq)]
pub enum Filter {
    Eq,   // |=
    Neq,  // !=
    Req,  // =~
    NReq, // !~
}

pub fn parse_filter<'a, E>(
    i: TokenStream<'a>,
) -> IResult<TokenStream<'a>, RefSpanned<'a, Filter>, E>
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
pub fn just<S, T, Input, Error>(tag: T) -> impl Fn(Input) -> IResult<Input, Spanned<S, T>, Error>
where
    Input: Head<Item = Spanned<S, T>> + InputTake,
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
) -> IResult<TokenStream<'a>, RefSpanned<'a, MatcherType>, E>
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
pub struct LabelMatcher<S> {
    pub name: Spanned<S, String>,
    pub value: Spanned<S, String>,
    pub matcher_type: Spanned<S, MatcherType>,
}

pub fn parse_label_matcher<'a, E>(
    input: TokenStream<'a>,
) -> IResult<TokenStream<'a>, RefSpanned<'a, LabelMatcher<Span<'a>>>, E>
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

// Selector is a set of label matchers.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Selector<S> {
    pub labels: Vec<Spanned<S, LabelMatcher<S>>>,
}

pub fn parse_selector<'a, E>(
    input: TokenStream<'a>,
) -> IResult<TokenStream<'a>, RefSpanned<'a, Selector<Span<'a>>>, E>
where
    E: ParseError<TokenStream<'a>> + ContextError<TokenStream<'a>>,
{
    context("selector", |input| {
        // extract the span for the beginning of the selector
        let (input, sp) = peek(just("{".delimiter())).parse(input)?;

        delimited(
            just("{".delimiter()),
            separated_list0(just(",".delimiter()), parse_label_matcher),
            just("}".delimiter()),
        )
        .parse(input)
        .map(|(rest, labels)| (rest, sp.map_v(|_| Selector { labels })))
    })
    .parse(input)
}

// macro to generate parsers that match a specific token variant
#[macro_export]
macro_rules! impl_token_type_parser {
    ($name: ident, $i:ident, $return_ty:ty, $extractor:expr) => {
        pub(crate) fn $name<'a, E>(
            input: TokenStream<'a>,
        ) -> IResult<TokenStream<'a>, RefSpanned<'a, $return_ty>, E>
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

#[cfg(test)]
#[test]
fn test_parse_selector() {
    let input = Span::new(r#"{foo="bar", bazz!~"buzz"}"#);
    let (_, toks) = super::lexer::lex::<VerboseError<Span>>(input).unwrap();

    let ts = TokenStream::new(&toks);

    let (_, f) = parse_selector::<VerboseError<_>>(ts).unwrap();
    let s = f.value;
    assert_eq!(2, s.labels.len());
    assert_eq!("foo".to_string(), *s.labels[0].value.name);
    assert_eq!(r#""bar""#.to_string(), *s.labels[0].value.value);
    assert_eq!(MatcherType::Eq, *s.labels[0].value.matcher_type);
    assert_eq!("bazz".to_string(), *s.labels[1].value.name);
    assert_eq!(r#""buzz""#.to_string(), *s.labels[1].value.value);
    assert_eq!(MatcherType::Nre, *s.labels[1].value.matcher_type);
}
