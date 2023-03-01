use core::fmt;
use std::fmt::Display;

use nom_locate::LocatedSpan;
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
