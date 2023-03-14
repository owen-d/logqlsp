use std::fmt::Display;
use std::iter::Enumerate;
use std::marker::PhantomData;
use std::ops::Deref;
use std::slice::{self, Iter};
use std::vec;

use nom::error::VerboseError;
use nom::{
    branch::alt,
    bytes::complete::{escaped, is_not, take_until, take_while},
    character::complete::{multispace0, one_of},
    combinator::{eof, map},
    error::ParseError,
    multi::many_till,
    sequence::{delimited, preceded, terminated, Tuple},
    IResult, InputIter,
};
use nom::{Compare, InputLength, InputTake, InputTakeAtPosition, Needed, Parser};
use nom_locate::position;
use nom_supreme::tag::complete::tag;
use nom_supreme::tag::TagError;
use tower_lsp::lsp_types::{DidChangeWatchedFilesRegistrationOptions, SemanticTokenType};

use super::utils::{spanned, RefSpanned, Span, Spanned};

// ------------------------------ Types ------------------------------

// Generally there are two sets of types:
// One for the lexing stage, and one for the parsing stage.
// This is for the lexing stage.

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Comment(String),
    // String, may include escaped characters
    String(String),
    // Sequence of chars
    Identifier(String),
    // (, ), [, ], {, }, ", `, ,, |, =, ~, !, *, /, +, -, #, \n
    Delimiter(Delimited<String>),
}

impl Token {
    pub fn semantic_token_type(&self) -> SemanticTokenType {
        match self {
            Token::Comment(_) => SemanticTokenType::COMMENT,
            Token::String(_) => SemanticTokenType::STRING,
            Token::Identifier(_) => SemanticTokenType::VARIABLE,
            Token::Delimiter(_) => SemanticTokenType::OPERATOR,
        }
    }

    pub fn as_str(&self) -> &str {
        match self {
            Token::Comment(s) => s,
            Token::String(s) => s,
            Token::Identifier(s) => s,
            Token::Delimiter(s) => &s.s,
        }
    }
}

impl InputLength for Token {
    // tokens in a token-stream always have len=1
    fn input_len(&self) -> usize {
        1
    }
}

// Helper trait to make it easier to create tokens
pub(crate) trait Tokenable {
    fn identifier(&self) -> Token;
    fn delimiter(&self) -> Token;
    fn string_tok(&self, delim: &str) -> Token;
    fn comment(&self) -> Token;
}

impl Tokenable for &str {
    fn identifier(&self) -> Token {
        Token::Identifier(self.to_string())
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
    fn identifier(&self) -> Token {
        Token::Identifier(self.to_string())
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

#[derive(Debug, PartialEq, Clone)]
pub struct Delimited<T> {
    pub s: String,
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

pub type Result<'a, E> = IResult<Span<'a>, RefSpanned<'a, Token>, E>;

#[derive(Clone, Debug)]
pub struct TokenStream<'a> {
    toks: &'a [RefSpanned<'a, Token>],
    pub src: &'a str,
}

impl<'a> TokenStream<'a> {
    pub fn new(src: &'a str, toks: &'a [RefSpanned<'a, Token>]) -> Self {
        Self { toks, src }
    }
}

// helper trait for dequeing first token
pub trait Head {
    type Item;
    fn head(&self) -> Option<Self::Item>;
}

impl<'a> Head for TokenStream<'a> {
    type Item = RefSpanned<'a, Token>;

    fn head(&self) -> Option<Self::Item> {
        self.toks.get(0).map(|x| x.clone())
    }
}

impl<'a> Compare<Token> for TokenStream<'a> {
    fn compare(&self, t: Token) -> nom::CompareResult {
        match self.toks.get(0) {
            Some(sp) if sp.value == t => nom::CompareResult::Ok,
            Some(_) => nom::CompareResult::Error,
            None => nom::CompareResult::Incomplete,
        }
    }

    fn compare_no_case(&self, t: Token) -> nom::CompareResult {
        self.compare(t)
    }
}

impl<'a> InputIter for TokenStream<'a> {
    type Item = RefSpanned<'a, Token>;

    type Iter = TokenIndices<'a>;

    type IterElem = TokenIter<'a>;

    fn iter_indices(&self) -> Self::Iter {
        TokenIndices {
            offset: 0,
            iter: self.iter_elements(),
        }
    }

    fn iter_elements(&self) -> Self::IterElem {
        TokenIter {
            iter: self.toks.iter(),
        }
    }

    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Item) -> bool,
    {
        for (i, x) in self.iter_indices() {
            if predicate(x) {
                return Some(i);
            }
        }
        None
    }

    fn slice_index(&self, count: usize) -> std::result::Result<usize, Needed> {
        let mut cnt = 0;
        for (index, _) in self.iter_indices() {
            if cnt == count {
                return Ok(index);
            }
            cnt += 1;
        }
        if cnt == count {
            return Ok(self.toks.len());
        }
        Err(Needed::Unknown)
    }
}

impl InputTake for TokenStream<'_> {
    fn take(&self, count: usize) -> Self {
        Self::new(self.src, &self.toks[..count])
    }

    fn take_split(&self, count: usize) -> (Self, Self) {
        let prefix = self.take(count);
        let suffix = Self::new(self.src, &self.toks[count..]);
        (suffix, prefix)
    }
}

impl InputLength for TokenStream<'_> {
    fn input_len(&self) -> usize {
        self.toks.len()
    }
}

pub struct TokenIter<'a> {
    iter: slice::Iter<'a, RefSpanned<'a, Token>>,
}

impl<'a> Iterator for TokenIter<'a> {
    type Item = RefSpanned<'a, Token>;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|x| x.clone())
    }
}

pub struct TokenIndices<'a> {
    offset: usize,
    iter: TokenIter<'a>,
}

impl<'a> Iterator for TokenIndices<'a> {
    type Item = (usize, RefSpanned<'a, Token>);

    fn next(&mut self) -> Option<Self::Item> {
        match self.iter.next() {
            None => None,
            Some(ch) => {
                let index = self.offset;
                self.offset += 1;
                Some((index, ch))
            }
        }
    }
}

// ------------------------------ Logic ------------------------------
pub fn lex<'a, E>(input: Span<'a>) -> IResult<Span<'a>, Vec<RefSpanned<'a, Token>>, E>
where
    E: ParseError<Span<'a>> + TagError<Span<'a>, &'static str>,
{
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
        many_till(token, eof).map(|(toks, _)| toks.into_iter().collect::<Vec<RefSpanned<Token>>>());

    tokens.parse(input)
}

fn string<'a, E>(input: Span<'a>) -> Result<E>
where
    E: ParseError<Span<'a>> + TagError<Span<'a>, &'static str>,
{
    alt((raw_string, double_quoted_string)).parse(input)
}

fn double_quoted_string<'a, E>(input: Span<'a>) -> Result<E>
where
    E: ParseError<Span<'a>> + TagError<Span<'a>, &'static str>,
{
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

    fn parse<'a, E>(i: Span<'a>) -> IResult<Span<'a>, (Span<'a>, Span<'a>, Span<'a>), E>
    where
        E: ParseError<Span<'a>> + TagError<Span<'a>, &'static str>,
    {
        (
            tag(r#"""#),
            escaped(except_control_or_quote, '\\', one_of(r#""n\"#)),
            tag(r#"""#),
        )
            .parse(i)
    }

    spanned(|(_, x, _)| x.string_tok("\""), parse).parse(input)
}

fn raw_string<'a, E: ParseError<Span<'a>>>(input: Span<'a>) -> Result<E>
where
    E: ParseError<Span<'a>> + TagError<Span<'a>, &'static str>,
{
    fn parse<'a, E>(i: Span<'a>) -> IResult<Span<'a>, (Span<'a>, Span<'a>, Span<'a>), E>
    where
        E: ParseError<Span<'a>> + TagError<Span<'a>, &'static str>,
    {
        (tag(r"`"), is_not("`"), tag(r"`")).parse(i)
    }

    spanned(|(_, x, _)| x.string_tok("`"), parse).parse(input)
}

fn word<'a, E: ParseError<Span<'a>>>(input: Span<'a>) -> Result<E> {
    let is_valid = |c: char| c.is_alphanumeric() || c == '_';
    spanned(|x: Span| x.identifier(), take_while(is_valid)).parse(input)
}

fn comment<'a, E>(input: Span<'a>) -> Result<E>
where
    E: ParseError<Span<'a>> + TagError<Span<'a>, &'static str>,
{
    spanned(
        |x| x.comment(),
        terminated(
            preceded(tag::<_, Span, _>("#"), take_while(|c| c != '\n')),
            alt((tag("\n"), eof)),
        ),
    )
    .parse(input)
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
    let input = r#""fo\"o""#;
    let (s, tok) = string::<VerboseError<Span>>(input).unwrap();
    assert_eq!("", s);
    let expected_tok = Token::String("\"fo\\\"o\"".to_string());
    assert_eq!(expected_tok, *tok)
}

#[cfg(test)]
#[test]
fn test_raw_string() {
    let input = r#"`foo\`"#;
    let (s, tok) = string::<VerboseError<Span>>(input).unwrap();
    assert_eq!("", s);
    let expected_tok: Token = Token::String(r#"`foo\`"#.to_string());
    assert_eq!(expected_tok, *tok)
}

#[cfg(test)]
#[test]
fn test_input() {
    use nom_supreme::final_parser::Location;

    let input = r#"{foo="bar", bazz!="buzz"} |= "bonk" |= `\n` |= "sno\"t " # foo|bar"
#final"#;
    let (s, toks) = lex::<VerboseError<Span>>(input).unwrap();
    assert_eq!("", s);
    let expected_toks: Vec<Token> = vec![
        "{".delimiter(),
        "foo".identifier(),
        "=".delimiter(),
        "bar".string_tok("\""),
        ",".delimiter(),
        "bazz".identifier(),
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

    let x = toks[toks.len() - 1].clone();
    let loc = Location::locate_tail(input, x.span);
    assert_eq!(2, loc.line);
    assert_eq!(1, loc.column);

    assert_eq!(
        // expected_toks
        //     .into_iter()
        //     .map(|x| (Span::new(""), x).into())
        //     .collect::<Vec<Spanned<Token>>>(),
        // toks,
        expected_toks,
        toks.into_iter()
            .map(|x: RefSpanned<Token>| x.value)
            .collect::<Vec<Token>>()
    )
}
