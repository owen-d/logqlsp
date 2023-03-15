// Model Errors !format

// #specification     unexpected:          <T> <got> <next type>
// #test              {|=}                 (FilterExpr, LabelMatcher)
// #test              sum("hello")         (String, MetricExpr<Stubby>)
//
// #ignore            Implement a Stubby (stubbable) struct to be
//                    used as a dummy type/non-overriding parameter.
// #hint              Use  PhantomData as a trait to map to the valid
//                    variants allowed in this instance of MetricExpr,
//                    in this case MetricExpr<SumTypePicker>.
//
// #test              sum([5m])            (LogRange, MetricExpr<Stubby>)
// #specification     unstarted input:     <T> ", `, (, [, {
// #test              rate|                (Delimiter, Delimiter)
// #test
// #test              sum)                 (Delimiter, Delimiter)
// #test              sumQ                 (Token, Delimiter)
// #specification     unterminated input:  <T> ", `, ), ], }
// #test              sum("hello"          (Delimiter, Nothing)
//

// Generate a list of tests below with matching signatures:
// 1 + 1 => int
// 1 - 3 => int
// 2 * 3 => int
// 6 / 4 => float

use std::{
    collections::HashMap,
    fmt::{Debug, Display, Error, Formatter},
};

use nom::{
    error::{ErrorKind, FromExternalError, ParseError},
    InputIter, InputLength, Offset,
};
use nom_supreme::{
    context::ContextError,
    error::{ErrorTree, GenericErrorTree},
    final_parser::{ExtractContext, Location, RecreateContext},
    tag::TagError,
};
use serde_json::to_string;
use tower_lsp::lsp_types::{Diagnostic, DiagnosticSeverity, Position, Range};

use super::lexer::{Head, TokenStream};

// shorthand for all the error dependencies when building parsers
pub trait Errorable<I>:
    ParseError<I>
    + ContextError<I, &'static str>
    + TagError<I, &'static str>
    + nom::error::ContextError<I>
{
}

impl<I, T> Errorable<I> for T where
    T: ParseError<I>
        + ContextError<I, &'static str>
        + TagError<I, &'static str>
        + nom::error::ContextError<I>
{
}

#[derive(Debug)]
pub struct SuggestiveError<I> {
    pub error: ErrorTree<I>,
}

fn loc_from_span(input: &str, sp: &str) -> Location {
    Location::locate_tail(input, sp)
}

pub fn entire_range(input: &str) -> Range {
    range_from_span(input, input).into()
}

fn range_from_span(input: &str, sp: &str) -> HashableRange {
    let start = loc_from_span(input, sp);
    let end = loc_from_span(input, &sp[sp.len()..]);
    // positions are 0-indexed whereas the locations are
    // 1-indexed
    HashableRange {
        from: HashablePosition {
            line: start.line as u32 - 1,
            character: start.column as u32 - 1,
        },
        to: HashablePosition {
            line: end.line as u32 - 1,
            character: end.column as u32 - 1,
        },
    }
}

impl SuggestiveError<&str> {
    pub fn diagnostics(&self, input: &str) -> Vec<Diagnostic> {
        fn add_diagnostic(e: &ErrorTree<&str>, input: &str, diagnostics: &mut Vec<Diagnostic>) {
            match e {
                GenericErrorTree::Base { location, kind } => {
                    let range = range_from_span(input, location).into();
                    let message = format!("{}", kind);
                    diagnostics.push(Diagnostic {
                        severity: Some(DiagnosticSeverity::ERROR),
                        range,
                        message,
                        ..Default::default()
                    });
                }
                GenericErrorTree::Stack { base, contexts } => {
                    let mut h = HashMap::<HashableRange, _, _>::new();
                    // unwind the trace, preferring the lower level context that errored for each range
                    for (s, ctx) in contexts.iter().rev() {
                        // decrement contexts so they don't overlap with the base
                        let range = HashableRange::from(range_from_span(input, s)).decrement();
                        let message = format!("{}", ctx);
                        // prefer the highest level context for each range
                        h.entry(range).or_insert(message);
                    }

                    // add diagnostics for each context, deduped by range & preferring the highest level context
                    for (range, v) in h.into_iter() {
                        diagnostics.push(Diagnostic {
                            severity: Some(DiagnosticSeverity::WARNING),
                            range: range.into(),
                            message: v,
                            ..Default::default()
                        });
                    }

                    add_diagnostic(base, input, diagnostics)
                }
                GenericErrorTree::Alt(siblings) => {
                    for sibling in siblings {
                        add_diagnostic(sibling, input, diagnostics)
                    }
                }
            };
        }

        let mut diagnostics = Vec::new();
        add_diagnostic(&self.error, input, &mut diagnostics);
        diagnostics
    }
}

impl<I: Display> Display for SuggestiveError<I> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        self.error.fmt(f)
    }
}

impl<I: Display + Debug> std::error::Error for SuggestiveError<I> {}

impl<I> ParseError<I> for SuggestiveError<I>
where
    ErrorTree<I>: ParseError<I>,
{
    fn from_error_kind(input: I, kind: ErrorKind) -> Self {
        SuggestiveError {
            error: ErrorTree::from_error_kind(input, kind),
        }
    }

    fn append(input: I, kind: ErrorKind, mut other: Self) -> Self {
        SuggestiveError {
            error: ErrorTree::append(input, kind, other.error),
        }
    }

    fn from_char(input: I, c: char) -> Self {
        SuggestiveError {
            error: ErrorTree::from_char(input, c),
        }
    }

    fn or(self, other: Self) -> Self {
        let error = self.error.or(other.error);
        SuggestiveError { error }
    }
}

impl<I> ContextError<I, &'static str> for SuggestiveError<I>
where
    ErrorTree<I>: ContextError<I, &'static str>,
{
    fn add_context(input: I, ctx: &'static str, other: Self) -> Self {
        SuggestiveError {
            error: ErrorTree::add_context(input, ctx, other.error),
        }
    }
}

impl<I> nom::error::ContextError<I> for SuggestiveError<I>
where
    ErrorTree<I>: nom::error::ContextError<I>,
{
    fn add_context(input: I, ctx: &'static str, other: Self) -> Self {
        Self {
            error: ContextError::add_context(input, ctx, other.error),
        }
    }
}

impl<I, T> TagError<I, T> for SuggestiveError<I>
where
    ErrorTree<I>: TagError<I, T>,
{
    fn from_tag(input: I, tag: T) -> Self {
        SuggestiveError {
            error: ErrorTree::from_tag(input, tag),
        }
    }
}

// Allows us to remap to the source string via a TokenStream in ErrorTree
impl<'a> RecreateContext<TokenStream<'a>> for &'a str {
    fn recreate_context(_original_input: TokenStream<'a>, tail: TokenStream<'a>) -> Self {
        match tail.head() {
            Some(x) => {
                let offset = tail.src.offset(x.span);
                &tail.src[offset..]
            }
            None => {
                // we're at end of input, return the empty tail
                &tail.src[tail.src.len()..]
            }
        }
    }
}

impl<I, I2> ExtractContext<I, SuggestiveError<I2>> for SuggestiveError<I>
where
    ErrorTree<I>: ExtractContext<I, ErrorTree<I2>>,
{
    fn extract_context(self, original_input: I) -> SuggestiveError<I2> {
        SuggestiveError {
            error: self.error.extract_context(original_input),
        }
    }
}

// helpers for hashing the lsp-types
#[derive(Debug, Hash, PartialEq, Eq)]
struct HashableRange {
    from: HashablePosition,
    to: HashablePosition,
}
impl HashableRange {
    // helper for decrementing range by 1 character
    fn decrement(self) -> Self {
        HashableRange {
            to: HashablePosition {
                line: self.to.line,
                character: self.to.character - 1,
            },
            ..self
        }
    }
}

#[derive(Debug, Hash, PartialEq, Eq)]
struct HashablePosition {
    line: u32,
    character: u32,
}

impl From<Range> for HashableRange {
    fn from(r: Range) -> Self {
        HashableRange {
            from: HashablePosition {
                line: r.start.line,
                character: r.start.character,
            },
            to: HashablePosition {
                line: r.end.line,
                character: r.end.character,
            },
        }
    }
}

impl From<HashableRange> for Range {
    fn from(r: HashableRange) -> Self {
        Range {
            start: Position {
                line: r.from.line,
                character: r.from.character,
            },
            end: Position {
                line: r.to.line,
                character: r.to.character,
            },
        }
    }
}
