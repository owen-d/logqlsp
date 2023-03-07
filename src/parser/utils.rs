use core::fmt;
use std::fmt::Display;

use nom::{IResult, Parser};
use nom_locate::{position, LocatedSpan};
use tower_lsp::lsp_types::CompletionItem;

pub type Span<'a> = LocatedSpan<&'a str>;
//  Alias for colocating a span with some <T>
pub type Spanned<'a, T> = (Span<'a>, T);

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

pub fn id<A>(x: A) -> A {
    x
}

// Run a parser, extracting the position and mapping the result
pub fn spanned<I, O, O2, E, P, F>(mut f: F, mut p: P) -> impl FnMut(I) -> IResult<I, (I, O2), E>
where
    P: Parser<I, O, E>,
    E: nom::error::ParseError<I>,
    I: nom::InputTake + nom::InputIter,
    F: FnMut(O) -> O2,
{
    move |s| {
        let (s, pos) = position(s)?;
        let (s, x) = p.parse(s)?;
        Ok((s, (pos, f(x))))
    }
}
