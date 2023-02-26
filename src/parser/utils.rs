use core::fmt;
use std::fmt::Display;

use nom::{error::ParseError, IResult, Parser};
use nom_locate::LocatedSpan;
use tower_lsp::lsp_types::CompletionItem;

pub type Span<'a> = LocatedSpan<&'a str>;
//  Alias for colocating a span with some <T>
pub type Spanned<'a, T> = (T, Span<'a>);

// For use somewhere down the line
#[derive(Debug, Clone)]
pub struct SuggestiveError<A> {
    err: A,
    completions: Option<Vec<CompletionItem>>,
}

impl<T: Display> Display for SuggestiveError<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.err)
    }
}

impl<E> SuggestiveError<E> {
    pub fn new(err: E, completions: Option<Vec<CompletionItem>>) -> Self {
        Self { err, completions }
    }
}

fn ignore_then<I, O1, O2, E: ParseError<I>, F, G>(
    mut f1: F,
    mut f2: G,
) -> impl FnMut(I) -> IResult<I, O2, E>
where
    F: Parser<I, O1, E>,
    G: Parser<I, O2, E>,
{
    move |input: I| {
        let (input, _) = f1.parse(input)?;
        f2.parse(input)
    }
}

fn then_ignore<I, O1, O2, E: ParseError<I>, F, G>(
    mut f1: F,
    mut f2: G,
) -> impl FnMut(I) -> IResult<I, O1, E>
where
    F: Parser<I, O1, E>,
    G: Parser<I, O2, E>,
{
    move |input: I| {
        let (input, o1) = f1.parse(input)?;
        f2.parse(input).map(|(i, _)| (i, o1))
    }
}
