use std::{collections::HashMap, fmt};

use chumsky::{prelude::*, Parser, Stream};
use tower_lsp::lsp_types::SemanticTokenType;

use crate::semantic_tokens::{CONTROL, IDENT, LEGEND_TYPE, OPERATOR, STRING};

pub type Span = std::ops::Range<usize>;
pub type Spanned<T> = (T, Span);

#[derive(Debug)]
pub struct InCompleteSemanticToken {
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
    LogExpr(Spanned<LogExpr>),
}

// LogExpr is a subset of the LogQL expression language.
// It takes a selector and a pipeline of operations.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum LogExpr {
    Selector(Spanned<Selector>),
    // Pipelined((Selector, Pipeline)),
}

// Selector is a set of label matchers.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Selector {
    pub labels: Vec<Spanned<LabelMatcher>>,
}

// LabelMatcher is a matcher for a label.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LabelMatcher {
    pub name: Spanned<String>,
    pub value: Spanned<String>,
    pub matcher_type: Spanned<MatcherType>,
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

pub fn expr_parser() -> impl Parser<Token, Spanned<Expr>, Error = Simple<Token>> + Clone {
    // A parser for label names
    let label_name = filter_map(|span, tok| match tok {
        Token::Ident(s) => Ok((s, span)),
        _ => Err(Simple::expected_input_found(span, Vec::new(), Some(tok))),
    })
    .labelled("label_name");

    // a parser for matcher types
    let matcher_type = filter_map(|span, tok| match tok {
        Token::Op(s) => match s.as_str() {
            "=" => Ok((MatcherType::Eq, span)),
            "!=" => Ok((MatcherType::Neq, span)),
            "=~" => Ok((MatcherType::Re, span)),
            "!~" => Ok((MatcherType::Nre, span)),
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
        Token::Str(s) => Ok((s, span)),
        _ => Err(Simple::expected_input_found(span, Vec::new(), Some(tok))),
    })
    .labelled("label_value");

    // a parser for label matchers
    let label_matcher = label_name
        .then(matcher_type)
        .then(label_value)
        .map(|((name, matcher_type), value)| {
            (
                LabelMatcher {
                    name: name.clone(),
                    value: value.clone(),
                    matcher_type,
                },
                name.1.start..value.1.end,
            )
        })
        .labelled("label_matcher");

    // a parser for label matchers
    let label_matchers = filter_map(|span, tok| match tok {
        // preserve the original span even though the item is discarded
        Token::Ctrl('{') => Ok(span),
        _ => Err(Simple::expected_input_found(
            span,
            vec![Some(Token::Ctrl('{'))],
            Some(tok),
        )),
    })
    .then(
        label_matcher.separated_by(filter_map(|span, tok| match tok {
            Token::Ctrl(',') => Ok(()),
            _ => Err(Simple::expected_input_found(
                span,
                vec![Some(Token::Ctrl(','))],
                Some(tok),
            )),
        })),
    )
    .then(filter_map(|span, tok| match tok {
        // preserve the original span even though the item is discarded
        Token::Ctrl('}') => Ok(span),
        _ => Err(Simple::expected_input_found(
            span,
            vec![Some(Token::Ctrl('}'))],
            Some(tok),
        )),
    }))
    .map(|((lparen_span, label_matchers), rparen_span)| {
        (
            Selector {
                labels: label_matchers,
            },
            lparen_span.start..rparen_span.end,
        )
    });

    // final parser
    label_matchers
        .then_ignore(end())
        .map(LogExpr::Selector)
        .map(|log_expr| match log_expr {
            // attach span info from underlying selector
            LogExpr::Selector(selector) => {
                let span = selector.clone().1;
                (
                    Expr::LogExpr((LogExpr::Selector(selector), span.clone())),
                    span,
                )
            }
        })
}

pub fn parse(
    src: &str,
) -> (
    Option<Expr>,
    Vec<Simple<String>>,
    Vec<InCompleteSemanticToken>,
) {
    let (tokens, errs) = lexer().parse_recovery(src);
    let (ast, tokenize_errors, semantic_tokens) = if let Some(tokens) = tokens {
        let semantic_tokens = tokens
            .iter()
            .filter_map(|(token, span)| match token {
                Token::Str(_) => Some(InCompleteSemanticToken {
                    start: span.start,
                    length: span.len(),
                    token_type: LEGEND_TYPE.iter().position(|item| item == &STRING).unwrap(),
                }),
                Token::Ident(_) => Some(InCompleteSemanticToken {
                    start: span.start,
                    length: span.len(),
                    token_type: LEGEND_TYPE.iter().position(|item| item == &IDENT).unwrap(),
                }),
                Token::Op(_) => Some(InCompleteSemanticToken {
                    start: span.start,
                    length: span.len(),
                    token_type: LEGEND_TYPE
                        .iter()
                        .position(|item| item == &OPERATOR)
                        .unwrap(),
                }),
                Token::Ctrl(_) => Some(InCompleteSemanticToken {
                    start: span.start,
                    length: span.len(),
                    token_type: LEGEND_TYPE
                        .iter()
                        .position(|item| item == &CONTROL)
                        .unwrap(),
                }),
            })
            .collect::<Vec<_>>();

        let len = src.chars().count();
        let (ast, parse_errs) =
            expr_parser().parse_recovery(Stream::from_iter(len..len + 1, tokens.into_iter()));

        (ast, parse_errs, semantic_tokens)
    } else {
        (None, Vec::new(), Vec::new())
    };

    let parse_errors = errs
        .into_iter()
        .map(|e| e.map(|c| c.to_string()))
        .chain(
            tokenize_errors
                .into_iter()
                .map(|e| e.map(|tok| tok.to_string())),
        )
        .collect::<Vec<_>>();

    (ast.map(|x| x.0), parse_errors, semantic_tokens)
}
