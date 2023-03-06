use std::marker::PhantomData;

use nom::error::VerboseError;
use nom::{
    branch::alt,
    bytes::complete::{escaped, is_not, tag, take_until, take_while},
    character::complete::{multispace0, one_of},
    combinator::{eof, map},
    error::ParseError,
    multi::many_till,
    sequence::{delimited, preceded, terminated, Tuple},
    IResult, InputIter,
};
use nom::{InputTakeAtPosition, Parser};
use nom_locate::position;

use super::utils::{id, Span, Spanned};

// ------------------------------ Types ------------------------------

// Generally there are two sets of types:
// One for the lexing stage, and one for the parsing stage.
// This is for the lexing stage.

#[derive(Debug, PartialEq)]
pub enum Token {
    Comment(String),
    // String, may include escaped characters
    String(String),
    // Sequence of chars
    Word(String),
    // (, ), [, ], {, }, ", `, ,, |, =, ~, !, *, /, +, -, #, \n
    Delimiter(Delimited<String>),
}

// Helper trait to make it easier to create tokens
trait Tokenable {
    fn word(&self) -> Token;
    fn delimiter(&self) -> Token;
    fn string_tok(&self, delim: &str) -> Token;
    fn comment(&self) -> Token;
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

    fn string_tok(&self, delim: &str) -> Token {
        Token::String(format!("{}{}{}", delim, self.to_string(), delim))
    }
    fn comment(&self) -> Token {
        Token::Comment(format!("#{}", self.to_string()))
    }
}

impl Tokenable for char {
    fn word(&self) -> Token {
        Token::Word(self.to_string())
    }

    fn delimiter(&self) -> Token {
        Token::Delimiter(Delimited {
            s: self.to_string(),
            valid: PhantomData,
        })
    }

    fn string_tok(&self, delim: &str) -> Token {
        Token::String(format!("{}{}{}", delim, self.to_string(), delim))
    }

    fn comment(&self) -> Token {
        Token::Comment(format!("#{}", self.to_string()))
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

pub type Result<'a, E> = IResult<Span<'a>, Spanned<'a, Token>, E>;

// ------------------------------ Logic ------------------------------
fn lex<'a, E: ParseError<Span<'a>>>(
    input: Span<'a>,
) -> IResult<Span<'a>, Vec<Spanned<'a, Token>>, E> {
    // todo(owen-d): implement escaped strings

    let multi_char_delims = |s| {
        spanned(
            |x| x.delimiter(),
            alt::<Span, _, _, _>((tag("!="), tag("=~"), tag("!~"), tag("|="))),
        )
        .parse(s)
    };

    let single_char_delims =
        |s| spanned(|x| x.delimiter(), one_of(SINGLE_CHAR_DELIMITERS)).parse(s);

    // listed in terms of parsing precedence
    let token = whitespaced(alt((
        comment,
        string,
        multi_char_delims,
        single_char_delims,
        word,
    )));

    let mut tokens =
        many_till(token, eof).map(|(toks, _)| toks.into_iter().collect::<Vec<Spanned<Token>>>());

    tokens.parse(input)
}

fn string<'a, E: ParseError<Span<'a>>>(input: Span<'a>) -> Result<E> {
    alt((raw_string, double_quoted_string)).parse(input)
}

fn double_quoted_string<'a, E: ParseError<Span<'a>>>(input: Span<'a>) -> Result<E> {
    fn except_control_or_quote<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
    where
        T: nom::InputTakeAtPosition,
        <T as nom::InputTakeAtPosition>::Item: PartialEq<char>,
    {
        input.split_at_position1_complete(
            |item| item == '"' || item == '\\',
            nom::error::ErrorKind::Escaped,
        )
    }

    fn parse<'a, E: ParseError<Span<'a>>>(
        i: Span<'a>,
    ) -> IResult<Span<'a>, (Span<'a>, Span<'a>, Span<'a>), E> {
        (
            tag(r#"""#),
            escaped(except_control_or_quote, '\\', one_of(r#""n\"#)),
            tag(r#"""#),
        )
            .parse(i)
    }

    spanned(|(_, x, _)| x.string_tok("\""), parse).parse(input)
}

fn raw_string<'a, E: ParseError<Span<'a>>>(input: Span<'a>) -> Result<E> {
    fn parse<'a, E: ParseError<Span<'a>>>(
        i: Span<'a>,
    ) -> IResult<Span<'a>, (Span<'a>, Span<'a>, Span<'a>), E> {
        (tag(r"`"), is_not("`"), tag(r"`")).parse(i)
    }

    spanned(|(_, x, _)| x.string_tok("`"), parse).parse(input)
}

fn word<'a, E: ParseError<Span<'a>>>(input: Span<'a>) -> Result<E> {
    let is_valid = |c: char| c.is_alphanumeric() || c == '_';
    spanned(|x: Span| x.word(), take_while(is_valid)).parse(input)
}

fn comment<'a, E: ParseError<Span<'a>>>(input: Span<'a>) -> Result<E> {
    spanned(
        |x| x.comment(),
        terminated(
            preceded(tag::<_, Span, _>("#"), take_while(|c| c != '\n')),
            alt((tag("\n"), eof)),
        ),
    )
    .parse(input)
}

// Run a parser, extracting the position and mapping the result
fn spanned<I, O, O2, E, P, F>(mut f: F, mut p: P) -> impl FnMut(I) -> IResult<I, (I, O2), E>
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

#[cfg(test)]
#[test]
fn test_string() {
    let input = Span::new(r#""fo\"o""#);
    let (s, tok) = string::<VerboseError<Span>>(input).unwrap();
    assert_eq!("", *s.fragment());
    let expected_tok = Token::String("\"fo\\\"o\"".to_string());
    assert_eq!(expected_tok, tok.1)
}

#[cfg(test)]
#[test]
fn test_raw_string() {
    let input = Span::new(r#"`foo\`"#);
    let (s, tok) = string::<VerboseError<Span>>(input).unwrap();
    assert_eq!("", *s.fragment());
    let expected_tok: Token = Token::String(r#"`foo\`"#.to_string());
    assert_eq!(expected_tok, tok.1)
}

#[cfg(test)]
#[test]
fn test_input() {
    let input = Span::new(
        r#"{foo="bar", bazz!="buzz"} |= "bonk" |= `\n` |= "sno\"t " # foo|bar"
        #final"#,
    );
    let (s, toks) = lex::<VerboseError<Span>>(input).unwrap();
    assert_eq!("", *s.fragment());
    let expected_toks: Vec<Token> = vec![
        "{".delimiter(),
        "foo".word(),
        "=".delimiter(),
        "bar".string_tok("\""),
        ",".delimiter(),
        "bazz".word(),
        "!=".delimiter(),
        "buzz".string_tok("\""),
        "}".delimiter(),
        "|=".delimiter(),
        r#"bonk"#.string_tok("\""),
        "|=".delimiter(),
        r#"\n"#.string_tok("`"),
        "|=".delimiter(),
        "sno\\\"t ".string_tok("\""),
        " foo|bar\"".comment(),
        "final".comment(),
    ];
    assert_eq!(
        expected_toks,
        toks.into_iter().map(|(_, x)| x).collect::<Vec<Token>>()
    )
}
