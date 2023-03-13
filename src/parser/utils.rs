use core::fmt;
use std::{fmt::Display, ops::Deref};

use nom::{error::ParseError, IResult, Parser, Slice};
use nom_locate::{position, LocatedSpan};
use tower_lsp::lsp_types::CompletionItem;

use super::lexer::TokenStream;

// usize stores length of captured input
pub type Span<'a> = LocatedSpan<&'a str, Option<usize>>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Spanned<'a, T> {
    pub span: Span<'a>,
    pub value: T,
}

impl<'a, T> Spanned<'a, T> {
    pub fn new(span: Span<'a>, value: T) -> Self {
        Self { span, value }
    }
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
pub fn spanned<'a, O, O2, E, P, F>(
    mut f: F,
    mut p: P,
) -> impl FnMut(Span<'a>) -> IResult<Span, Spanned<'a, O2>, E>
where
    P: Parser<Span<'a>, O, E>,
    E: nom::error::ParseError<Span<'a>>,
    F: FnMut(O) -> O2,
{
    move |s| {
        let (s, pos) = position(s)?;
        let (s1, x) = p.parse(s)?;

        let byte_ln = s1.location_offset() - s.location_offset();
        let content_ln = s.slice(0..byte_ln).fragment().chars().count();
        let sp = pos.map_extra(|_| Some(content_ln));
        Ok((s1, Spanned::new(sp, f(x))))
    }
}
