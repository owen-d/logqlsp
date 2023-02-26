use std::marker::PhantomData;

use nom::{
    bytes::complete::{tag, take_until},
    character::{
        complete::{char, multispace0, none_of, one_of},
        is_newline,
    },
    error::ParseError,
    sequence::{delimited, Tuple},
    IResult,
};

use super::utils::Span;

// ------------------------------ Types ------------------------------
// Example valid input: {job="foo", bar="buzz"} |= "bonk"

// Generally there are two sets of types:
// One for the lexing stage, and one for the parsing stage.
// This is for the lexing stage.
pub enum Either<A, B> {
    Left(A),
    Right(B),
}
pub type Number = Either<i64, f64>;

// type info:
// A -> Something to delimit by
pub enum Token {
    // Sequence of chars
    Word(String),
    // (, ), [, ], {, }, ", `, ,, |, =, ~, !, *, /, +, -, #, \n
    Delimiter(Delimited<String>),
}

pub const SINGLE_CHAR_DELIMITERS: &'static [char] = &[
    '(', ')', '[', ']', '{', '}', '"', '`', ',', '|', '*', '/', '+', '-', '#', '\n', '=',
];

pub const MULTI_CHAR_DELIMS: &'static [&'static str] = &["!=", "=~", "!~", "|="];

pub struct Delimited<T> {
    // a type to implement a trait which maps to the valid variants
    pub(crate) valid: PhantomData<T>,
}

// trait to map to valid variants
pub trait Resolver<T> {
    fn resolve() -> T;
}

// Ha, I bet this is redundant from a compiler's perspective,
// but it's a good way to document the intent.
pub type Nothing = PhantomData<()>;

// Tool for things which don't need to be overridden,
// such as default Resolvers
pub struct Stubby {}

pub type Result<'a> = IResult<Span<'a>, Vec<Token>>;

pub type LexErr<'a> = nom::error::Error<Span<'a>>;

// ------------------------------ Logic ------------------------------
fn lex(input: Span) -> Result {
    // Word|Delimiter
    // # -> any until end of line or document
    // todo(owen-d): implemente escaped strings
    // str_delims -> parse until closing delim
    // whitespace -> continue
    // other delim -> delim
    // else it's not a protected char, repeatedly accumulate to string
    let multi_char_delims = tag::<_, _, LexErr>("foo");
    let single_char_delimts = one_of::<Span, _, LexErr>(SINGLE_CHAR_DELIMITERS);

    todo!()
}

fn comment(input: Span) -> Result {
    let (s, (_, comment, _)) = (tag("#"), take_until("\n"), tag("\n")).parse(input)?;
    todo!()
}

/// A combinator that takes a parser `inner` and produces a parser that also consumes both leading and
/// trailing whitespace, returning the output of `inner`.
fn whitespace<'a, F, O, E: ParseError<&'a str>>(
    inner: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    F: FnMut(&'a str) -> IResult<&'a str, O, E>,
{
    delimited(multispace0, inner, multispace0)
}
