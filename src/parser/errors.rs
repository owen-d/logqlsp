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
    InputLength,
};
use nom_supreme::{context::ContextError, error::ErrorTree, tag::TagError};

#[derive(Debug)]
pub struct SuggestiveError<I> {
    pub error: ErrorTree<I>,
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
