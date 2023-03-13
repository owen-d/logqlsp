use core::fmt;
use std::{fmt::Display, ops::Deref};

use nom::{error::ParseError, IResult, Parser, Slice};
use nom_locate::{position, LocatedSpan};
use tower_lsp::lsp_types::CompletionItem;

use super::lexer::TokenStream;

// usize stores length of captured input
pub type Span<'a> = LocatedSpan<&'a str>;

// RefSpanned is a Spanned with a reference to the input
pub type RefSpanned<'a, T> = Spanned<Span<'a>, T>;

#[derive(Debug, Clone)]
pub struct Offset {
    pub line: u32,
    pub column: u32,
}

impl<'a> From<Span<'a>> for Offset {
    fn from(span: Span<'a>) -> Self {
        Self {
            line: span.location_line(),
            column: span.get_utf8_column() as u32,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Spanned<S, T> {
    pub span: S,
    pub content_ln: Option<usize>,
    pub value: T,
}

impl<S, T> Spanned<S, T> {
    pub fn new(span: S, value: T) -> Self {
        Self {
            span,
            value,
            content_ln: None,
        }
    }

    pub fn new_with_ln(span: S, value: T, content_ln: usize) -> Self {
        Self {
            span,
            value,
            content_ln: Some(content_ln),
        }
    }
}

impl<'a, T> From<(Span<'a>, T)> for Spanned<Span<'a>, T> {
    fn from((span, value): (Span<'a>, T)) -> Self {
        Self {
            span,
            value,
            content_ln: None,
        }
    }
}
impl<S, T> From<(S, T, usize)> for Spanned<S, T> {
    fn from((span, value, ln): (S, T, usize)) -> Self {
        Self::new_with_ln(span, value, ln)
    }
}

impl<S, A> Spanned<S, A> {
    pub fn map_v<B>(self, f: impl FnOnce(A) -> B) -> Spanned<S, B> {
        Spanned {
            span: self.span,
            value: f(self.value),
            content_ln: self.content_ln,
        }
    }
    pub fn map_sp<B>(self, f: impl FnOnce(S) -> B) -> Spanned<B, A> {
        Spanned {
            span: f(self.span),
            value: self.value,
            content_ln: self.content_ln,
        }
    }
}

impl<S, T> Deref for Spanned<S, T> {
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
) -> impl FnMut(Span<'a>) -> IResult<Span, Spanned<Span<'a>, O2>, E>
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
        Ok((s1, Spanned::new_with_ln(pos, f(x), content_ln)))
    }
}
