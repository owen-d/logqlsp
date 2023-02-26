use std::marker::PhantomData;

use nom::IResult;

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
    // (, ), [, ], {, }, ", `, ,, |, =, ~, !, *, /, +, -, #
    Delimiter(Delimited<String>),
}

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

pub type Result<'a> = IResult<Span<'a>, Token>;

// ------------------------------ Logic ------------------------------
fn lex(input: Span) -> Result {
    // Word|Delimiter
    // # -> any until end of line or document
    // todo(owen-d): implemente escaped strings
    // str_delims -> parse until closing delim
    // whitespace -> continue
    // other delim -> delim
    // else it's not a protected char, repeatedly accumulate to string
    todo!()
}
