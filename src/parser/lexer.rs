use std::iter::Enumerate;
use std::marker::PhantomData;
use std::ops::Deref;
use std::slice::{self, Iter};
use std::vec;

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
use nom::{Compare, InputLength, InputTake, InputTakeAtPosition, Needed, Parser};
use nom_locate::position;
use tower_lsp::lsp_types::SemanticTokenType;

use super::utils::{spanned, Span, Spanned};

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

pub type Result<'a, E> = IResult<Span<'a>, Spanned<'a, Token>, E>;

#[derive(Clone, Debug)]
pub struct TokenStream<'a>(&'a [Spanned<'a, Token>]);
impl<'a> TokenStream<'a> {
    pub fn new(toks: &'a [Spanned<'a, Token>]) -> Self {
        Self(toks)
    }
}

impl<'a> Deref for TokenStream<'a> {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.0.get(0).map_or("", |x| x.span.fragment())
    }
}
// helper trait for dequeing first token
pub trait Head {
    type Item;
    fn head(&self) -> Option<Self::Item>;
}

impl<'a> Head for TokenStream<'a> {
    type Item = Spanned<'a, Token>;

    fn head(&self) -> Option<Self::Item> {
        self.0.get(0).map(|x| x.clone())
    }
}

impl<'a> Compare<Token> for TokenStream<'a> {
    fn compare(&self, t: Token) -> nom::CompareResult {
        match self.0.get(0) {
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
    type Item = Spanned<'a, Token>;

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
            iter: self.0.iter(),
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
            return Ok(self.0.len());
        }
        Err(Needed::Unknown)
    }
}

impl InputTake for TokenStream<'_> {
    fn take(&self, count: usize) -> Self {
        Self(&self.0[..count])
    }

    fn take_split(&self, count: usize) -> (Self, Self) {
        let prefix = self.take(count);
        let suffix = Self(&self.0[count..]);
        (suffix, prefix)
    }
}

impl InputLength for TokenStream<'_> {
    fn input_len(&self) -> usize {
        self.0.len()
    }
}

pub struct TokenIter<'a> {
    iter: slice::Iter<'a, Spanned<'a, Token>>,
}

impl<'a> Iterator for TokenIter<'a> {
    type Item = Spanned<'a, Token>;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|x| x.clone())
    }
}

pub struct TokenIndices<'a> {
    offset: usize,
    iter: TokenIter<'a>,
}

impl<'a> Iterator for TokenIndices<'a> {
    type Item = (usize, Spanned<'a, Token>);

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
pub(crate) fn lex<'a, E: ParseError<Span<'a>>>(
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
    spanned(|x: Span| x.identifier(), take_while(is_valid)).parse(input)
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
    let input = Span::new_extra(r#""fo\"o""#, None);
    let (s, tok) = string::<VerboseError<Span>>(input).unwrap();
    assert_eq!("", *s.fragment());
    let expected_tok = Token::String("\"fo\\\"o\"".to_string());
    assert_eq!(expected_tok, *tok)
}

#[cfg(test)]
#[test]
fn test_raw_string() {
    let input = Span::new_extra(r#"`foo\`"#, None);
    let (s, tok) = string::<VerboseError<Span>>(input).unwrap();
    assert_eq!("", *s.fragment());
    let expected_tok: Token = Token::String(r#"`foo\`"#.to_string());
    assert_eq!(expected_tok, *tok)
}

#[cfg(test)]
#[test]
fn test_input() {
    let input = Span::new_extra(
        r#"{foo="bar", bazz!="buzz"} |= "bonk" |= `\n` |= "sno\"t " # foo|bar"
#final"#,
        None,
    );
    let (s, toks) = lex::<VerboseError<Span>>(input).unwrap();
    assert_eq!("", *s.fragment());
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
    assert_eq!(2, x.span.location_line());
    assert_eq!(1, x.span.get_utf8_column());

    assert_eq!(
        // expected_toks
        //     .into_iter()
        //     .map(|x| (Span::new_extra("", None), x).into())
        //     .collect::<Vec<Spanned<Token>>>(),
        // toks,
        expected_toks,
        toks.into_iter()
            .map(|x: Spanned<Token>| x.value)
            .collect::<Vec<Token>>()
    )
}
