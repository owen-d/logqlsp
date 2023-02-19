use super::logql::*;
use chumsky::{prelude::Simple, Error, Parser, Stream};
#[test]
fn parse_simple() {
    struct TestCase {
        input: &'static str,
        expected: Result<Expr, Vec<Simple<Token>>>,
    }
    for case in vec![
        TestCase {
            input: r#"{job="foo"}"#,
            expected: Ok(Expr::LogExpr(LogExpr::Selector(Selector {
                labels: vec![LabelMatcher {
                    name: "job".to_string(),
                    value: "foo".to_string(),
                    matcher_type: MatcherType::Eq,
                }],
            }))),
        },
        TestCase {
            input: r#"{job="foo", app="bar"}"#,
            expected: Ok(Expr::LogExpr(LogExpr::Selector(Selector {
                labels: vec![
                    LabelMatcher {
                        name: "job".to_string(),
                        value: "foo".to_string(),
                        matcher_type: MatcherType::Eq,
                    },
                    LabelMatcher {
                        name: "app".to_string(),
                        value: "bar".to_string(),
                        matcher_type: MatcherType::Eq,
                    },
                ],
            }))),
        },
        TestCase {
            input: r#"job="foo"}"#,
            expected: Err(vec![Simple::expected_input_found(
                0..3,
                vec![Some(Token::Ctrl('{'))],
                Some(Token::Ident("job".to_string())),
            )
            .with_label("label_matchers")]),
        },
        TestCase {
            input: r#"{job="foo"abc"#,
            expected: Err(vec![Simple::expected_input_found(
                10..13,
                vec![Some(Token::Ctrl(',')), Some(Token::Ctrl('}'))],
                Some(Token::Ident("abc".to_string())),
            )
            .with_label("label_matchers")]),
        },
    ] {
        let lexed = lexer().parse(case.input);
        assert!(lexed.is_ok(), "{:#?}", lexed.err());
        let len = case.input.chars().count();
        let stream = Stream::from_iter(len..len + 1, lexed.unwrap().into_iter());

        let parsed = expr_parser().parse(stream);
        if case.expected.is_err() {
            assert!(parsed.is_err());
            case.expected
                .err()
                .unwrap()
                .into_iter()
                .zip(parsed.err().unwrap().into_iter())
                .for_each(|(exp, got)| {
                    assert_eq!(exp, got);
                    // For some reason the error messages
                    // aren't checked in the assert_eq! macro
                    let (exp_msg, got_msg) = (format!("{}", exp), format!("{}", got));
                    assert_eq!(exp_msg, got_msg);
                });
        } else {
            assert!(parsed.is_ok(), "{:?}", parsed.err());
            assert_eq!(parsed.unwrap(), case.expected.unwrap());
        }
    }
}
