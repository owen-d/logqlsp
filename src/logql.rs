use chumsky::prelude::*;
use chumsky::Parser;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr {
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

pub fn parser() -> impl Parser<char, Expr, Error = Simple<char>> {
    // A parser for strings
    let str_ = just('"')
        .ignore_then(filter(|c| *c != '"').repeated())
        .then_ignore(just('"'))
        .collect::<String>();

    // A parser for label names
    let label_name = filter(|c: &char| c.is_alphanumeric() || *c == '_')
        .repeated()
        .collect::<String>();

    // a parser for matcher types
    let matcher_type = just("=")
        .or(just("!="))
        .or(just("=~"))
        .or(just("!~"))
        .map(|s| match s {
            "=" => MatcherType::Eq,
            "!=" => MatcherType::Neq,
            "=~" => MatcherType::Re,
            "!~" => MatcherType::Nre,
            _ => unreachable!(),
        });

    // a parser for label matchers
    let label_matcher =
        label_name
            .then(matcher_type)
            .then(str_)
            .map(|((name, matcher_type), value)| LabelMatcher {
                name,
                value,
                matcher_type,
            });

    // a parser for label matchers
    let label_matchers = label_matcher
        .padded()
        .separated_by(just(","))
        .delimited_by(just("{"), just("}"))
        .map(|labels| Selector { labels });

    // a parser for comments
    let maybe_comment = just("#").then(take_until(just('\n'))).padded().or_not();

    // final parser
    label_matchers
        .padded_by(maybe_comment)
        .then_ignore(end())
        .map(LogExpr::Selector)
        .map(Expr::LogExpr)
}
