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

use std::fmt::{Debug, Display, Error, Formatter};

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

fn range_from_span(input: &str, sp: &str) -> Range {
    let start = loc_from_span(input, sp);
    let end = loc_from_span(input, &sp[sp.len()..]);
    // positions are 0-indexed whereas the locations are
    // 1-indexed
    Range {
        start: Position {
            line: start.line as u32 - 1,
            character: start.column as u32 - 1,
        },
        end: Position {
            line: end.line as u32 - 1,
            character: end.column as u32 - 1,
        },
    }
}

impl SuggestiveError<&str> {
    pub fn diagnostics(&self, input: &str) -> Option<Vec<Diagnostic>> {
        Some(vec![Diagnostic {
            severity: Some(DiagnosticSeverity::ERROR),
            range: range_from_span(input, input),
            message: format!("{}", self),
            ..Default::default()
        }])
        // fn add_diagnostic(e: &ErrorTree<&str>, input: &str, diagnostics: &mut Vec<Diagnostic>) {
        //     match e {
        //         GenericErrorTree::Base { location, kind } => {
        //             let range = range_from_span(input, location);
        //             let message = format!("{}", kind);
        //             log::warn!(
        //                 "kind {}, s {:#}, loc {:#?}",
        //                 kind,
        //                 location,
        //                 range_from_span(input, location)
        //             );
        //             diagnostics.push(Diagnostic {
        //                 severity: Some(DiagnosticSeverity::ERROR),
        //                 range,
        //                 message,
        //                 ..Default::default()
        //             });
        //         }
        //         GenericErrorTree::Stack { base, contexts } => {
        //             for (s, ctx) in contexts.iter() {
        //                 log::warn!(
        //                     "ctx {}, s {:#}, range {:#?}",
        //                     ctx,
        //                     s,
        //                     range_from_span(input, s)
        //                 );
        //                 let range = range_from_span(input, s);
        //                 let message = format!("{}", ctx);
        //                 diagnostics.push(Diagnostic {
        //                     severity: Some(DiagnosticSeverity::WARNING),
        //                     range,
        //                     message,
        //                     ..Default::default()
        //                 });
        //                 add_diagnostic(base, input, diagnostics)
        //             }
        //         }
        //         GenericErrorTree::Alt(siblings) => {
        //             for sibling in siblings {
        //                 log::warn!("sibling {:#?}", sibling);
        //                 add_diagnostic(sibling, input, diagnostics)
        //             }
        //         }
        //     };
        // }

        // let mut diagnostics = Vec::new();
        // add_diagnostic(&self.error, input, &mut diagnostics);
        // if diagnostics.len() > 0 {
        //     Some(diagnostics)
        // } else {
        //     None
        // }
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
