use std::fmt;

use chumsky::{prelude::*, Parser};

pub type Span = std::ops::Range<usize>;
pub type Spanned<T> = (T, Span);

#[derive(Debug)]
pub struct ImCompleteSemanticToken {
    pub start: usize,
    pub length: usize,
    pub token_type: usize,
}
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Token {
    Str(String),   // "<label_value>"
    Ident(String), // <label_name>
    Op(String),    // =, !=, =~, !~, |
    Ctrl(char),    // {, }, [, ], (, ), ,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Str(s) => write!(f, "\"{}\"", s),
            Token::Ident(s) => write!(f, "{}", s),
            Token::Op(s) => write!(f, "{}", s),
            Token::Ctrl(c) => write!(f, "{}", c),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr {
    Error,
    LogExpr(LogExpr),
}

// LogExpr is a subset of the LogQL expression language.
// It takes a selector and a pipeline of operations.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum LogExpr {
    Selector(Selector),
    // Pipelined((Selector, Pipeline)),
}

// Selector is a set of label matchers.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Selector {
    pub labels: Vec<LabelMatcher>,
}

// LabelMatcher is a matcher for a label.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LabelMatcher {
    pub name: String,
    pub value: String,
    pub matcher_type: MatcherType,
}

// MatcherType is the type of matcher. It can be equality, regex, etc.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MatcherType {
    Eq,
    Neq,
    Re,
    Nre,
}

// lexer turns a char stream into a set of spanned tokens.
pub fn lexer() -> impl Parser<char, Vec<(Token, Span)>, Error = Simple<char>> {
    // A parser for strings
    let str_ = just('"')
        .ignore_then(filter(|c| *c != '"').repeated())
        .then_ignore(just('"'))
        .collect::<String>()
        .map(Token::Str);

    // A parser for operators
    let op = one_of("|!=~")
        .repeated()
        .at_least(1)
        .collect::<String>()
        .map(Token::Op);

    // A parser for control characters (delimiters, semicolons, etc.)
    let ctrl = one_of("()[]{},").map(|c| Token::Ctrl(c));

    // A parser for identifiers and keywords
    let ident = text::ident().map(|ident: String| match ident.as_str() {
        _ => Token::Ident(ident),
    });

    // A single token can be one of the above
    let token = str_
        .or(op)
        .or(ctrl)
        .or(ident)
        .recover_with(skip_then_retry_until([]));

    let comment = just("#").then(take_until(just('\n'))).padded();

    token
        .padded_by(comment.repeated())
        .map_with_span(|tok, span| (tok, span))
        .padded()
        .repeated()
}

pub fn expr_parser() -> impl Parser<Token, Expr, Error = Simple<Token>> + Clone {
    // A parser for label names
    let label_name = filter_map(|span, tok| match tok {
        Token::Ident(s) => Ok(s),
        _ => Err(Simple::expected_input_found(span, Vec::new(), Some(tok))),
    })
    .labelled("label_name");

    // a parser for matcher types
    let matcher_type = filter_map(|span, tok| match tok {
        Token::Op(s) => match s.as_str() {
            "=" => Ok(MatcherType::Eq),
            "!=" => Ok(MatcherType::Neq),
            "=~" => Ok(MatcherType::Re),
            "!~" => Ok(MatcherType::Nre),
            _ => Err(Simple::expected_input_found(
                span,
                Vec::new(),
                Some(Token::Op(s)),
            )),
        },
        _ => Err(Simple::expected_input_found(span, vec![], Some(tok))),
    })
    .labelled("matcher_type");

    // a parser for label values
    let label_value = filter_map(|span, tok| match tok {
        Token::Str(s) => Ok(s),
        _ => Err(Simple::expected_input_found(span, Vec::new(), Some(tok))),
    })
    .labelled("label_value");

    // a parser for label matchers
    let label_matcher = label_name
        .then(matcher_type)
        .then(label_value)
        .map(|((name, matcher_type), value)| LabelMatcher {
            name,
            value,
            matcher_type,
        })
        .labelled("label_matcher");

    // a parser for label matchers
    let label_matchers = label_matcher
        .separated_by(filter_map(|span, tok| match tok {
            Token::Ctrl(',') => Ok(()),
            _ => Err(Simple::expected_input_found(
                span,
                vec![Some(Token::Ctrl(','))],
                Some(tok),
            )),
        }))
        .delimited_by(
            filter_map(|span, tok| match tok {
                Token::Ctrl('{') => Ok(()),
                _ => Err(Simple::expected_input_found(
                    span,
                    vec![Some(Token::Ctrl('{'))],
                    Some(tok),
                )),
            }),
            filter_map(|span, tok| match tok {
                Token::Ctrl('}') => Ok(()),
                _ => Err(Simple::expected_input_found(
                    span,
                    vec![Some(Token::Ctrl('}'))],
                    Some(tok),
                )),
            }),
        )
        .map(|labels| Selector { labels })
        .labelled("label_matchers");

    // final parser
    label_matchers
        .then_ignore(end())
        .map(LogExpr::Selector)
        .map(Expr::LogExpr)
}
