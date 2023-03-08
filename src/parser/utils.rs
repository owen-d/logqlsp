use core::fmt;
use std::{fmt::Display, ops::Deref};

use nom::{error::ParseError, IResult, Parser};
use nom_locate::{position, LocatedSpan};
use tower_lsp::lsp_types::CompletionItem;

use super::lexer::TokenStream;

pub type Span<'a> = LocatedSpan<&'a str>;

#[derive(Debug, Clone, PartialEq)]
pub struct Spanned<'a, T> {
    pub span: Span<'a>,
    pub value: T,
}

impl<'a, T> From<(Span<'a>, T)> for Spanned<'a, T> {
    fn from((span, value): (Span<'a>, T)) -> Self {
        Self { span, value }
    }
}

impl<'a, A> Spanned<'a, A> {
    pub fn map_v<B>(self, f: impl FnOnce(A) -> B) -> Spanned<'a, B> {
        Spanned {
            span: self.span,
            value: f(self.value),
        }
    }
    pub fn map_sp(self, f: impl FnOnce(Span<'a>) -> Span<'a>) -> Spanned<'a, A> {
        Spanned {
            span: f(self.span),
            value: self.value,
        }
    }
}

impl<T> Deref for Spanned<'_, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

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

// Run a parser, extracting the position and mapping the result
pub fn spanned<I, O, O2, O3, E, P, F>(mut f: F, mut p: P) -> impl FnMut(I) -> IResult<I, O3, E>
where
    P: Parser<I, O, E>,
    E: nom::error::ParseError<I>,
    I: nom::InputTake + nom::InputIter,
    F: FnMut(O) -> O2,
    (I, O2): Into<O3>,
{
    move |s| {
        let (s, pos) = position(s)?;
        let (s, x) = p.parse(s)?;
        Ok((s, (pos, f(x)).into()))
    }
}
