use std::marker::PhantomData;

use nom::{
    branch::alt,
    bytes::complete::{tag, take_until, take_while},
    character::complete::{char, multispace0, one_of},
    combinator::eof,
    error::ParseError,
    multi::many_till,
    sequence::{delimited, Tuple},
    IResult, Parser,
};

use super::utils::Span;

// ------------------------------ Types ------------------------------

// Generally there are two sets of types:
// One for the lexing stage, and one for the parsing stage.
// This is for the lexing stage.

#[derive(Debug, PartialEq)]
pub enum Token {
    // Sequence of chars
    Word(String),
    // (, ), [, ], {, }, ", `, ,, |, =, ~, !, *, /, +, -, #, \n
    Delimiter(Delimited<String>),
}

trait Tokenable {
    fn word(&self) -> Token;
    fn delimiter(&self) -> Token;
}

impl Tokenable for &str {
    fn word(&self) -> Token {
        Token::Word(self.to_string())
    }

    fn delimiter(&self) -> Token {
        Token::Delimiter(Delimited {
            s: self.to_string(),
            valid: PhantomData,
        })
    }
}

pub const SINGLE_CHAR_DELIMITERS: &'static [char] = &[
    '(', ')', '[', ']', '{', '}', '"', '`', ',', '|', '*', '/', '+', '-', '#', '\n', '=',
];

pub const MULTI_CHAR_DELIMS: &'static [&'static str] = &["!=", "=~", "!~", "|="];

#[derive(Debug, PartialEq)]
pub struct Delimited<T> {
    s: String,
    // a type to implement a trait which maps to the valid variants
    // TBD if this is necessary
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

pub type Result<'a> = IResult<Span<'a>, Vec<Token>>;

pub type LexErr<'a> = nom::error::Error<Span<'a>>;

// ------------------------------ Logic ------------------------------
fn lex(input: Span) -> Result {
    // todo(owen-d): implement escaped strings

    let multi_char_delims = alt::<Span, _, LexErr, _>((tag("!="), tag("=~"), tag("!~"), tag("|=")))
        .map(|s| {
            vec![Token::Delimiter(Delimited {
                s: s.to_string(),
                valid: PhantomData,
            })]
        });

    let single_char_delims = one_of::<Span, _, LexErr>(SINGLE_CHAR_DELIMITERS).map(|c| {
        vec![Token::Delimiter(Delimited {
            s: c.to_string(),
            valid: PhantomData,
        })]
    });

    // listed in terms of parsing precedence
    let token = whitespaced(alt((
        comment,
        string,
        multi_char_delims,
        single_char_delims,
        word,
    )));

    let mut tokens =
        many_till(token, eof).map(|(toks, _)| toks.into_iter().flatten().collect::<Vec<Token>>());

    tokens.parse(input)
}

fn string(input: Span) -> Result {
    let (s, (_, string, _)) = (char('"'), take_until("\""), char('"')).parse(input)?;
    Ok((
        s,
        vec![
            Token::Delimiter(Delimited {
                s: "\"".to_string(),
                valid: PhantomData,
            }),
            Token::Word(string.to_string()),
            Token::Delimiter(Delimited {
                s: "\"".to_string(),
                valid: PhantomData,
            }),
        ],
    ))
}

fn word(input: Span) -> Result {
    let is_valid = |c: char| c.is_alphanumeric() || c == '_';
    take_while(is_valid)
        .map(|c: Span| vec![Token::Word(c.to_string())])
        .parse(input)
}

fn comment(input: Span) -> Result {
    let (s, (_, comment, _)) = (tag("#"), take_until("\n"), tag("\n")).parse(input)?;
    Ok((
        s,
        vec![
            Token::Delimiter(Delimited {
                s: "#".to_string(),
                valid: PhantomData,
            }),
            Token::Word(comment.to_string()),
        ],
    ))
}

/// A combinator that takes a parser `inner` and produces a parser that also consumes both leading and
/// trailing whitespace, returning the output of `inner`.
fn whitespaced<I, F, O, E>(inner: F) -> impl FnMut(I) -> IResult<I, O, E>
where
    F: FnMut(I) -> IResult<I, O, E>,
    I: nom::InputLength + nom::InputTakeAtPosition,
    <I as nom::InputTakeAtPosition>::Item: nom::AsChar + Clone,
    E: ParseError<I>,
{
    delimited(multispace0, inner, multispace0)
}

#[test]
fn test_input() {
    let input = Span::new(r#"{foo="bar", bazz!="buzz"} |= "bonk""#);
    let (s, toks) = lex(input).unwrap();
    assert_eq!("", *s.fragment());
    let expected_toks: Vec<Token> = vec![
        "{".delimiter(),
        "foo".word(),
        "=".delimiter(),
        "\"".delimiter(),
        "bar".word(),
        "\"".delimiter(),
        ",".delimiter(),
        "bazz".word(),
        "!=".delimiter(),
        "\"".delimiter(),
        "buzz".word(),
        "\"".delimiter(),
        "}".delimiter(),
        "|=".delimiter(),
        "\"".delimiter(),
        "bonk".word(),
        "\"".delimiter(),
    ];
    assert_eq!(expected_toks, toks)
}
