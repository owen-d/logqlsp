use super::logql::*;
use chumsky::{prelude::Simple, Error, Parser, Stream};

macro_rules! parse_tests {
    ($($name:ident: $case:expr,)*) => {
    $(
        #[test]
        fn $name() {
            let exp = $case.1;
            let lexed = lexer().parse($case.0);
            assert!(lexed.is_ok(), "{:#?}", lexed.err());
            let len = $case.0.chars().count();
            let stream = Stream::from_iter(len..len + 1, lexed.unwrap().into_iter());

            let parsed = expr_parser().parse(stream);
            if exp.is_err() {
                assert!(parsed.is_err());
                exp
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
                assert_eq!(parsed.unwrap(), exp.unwrap());
            }
        }
    )*
    }
}

parse_tests! {
    single_label: (
        r#"{job="foo"}"#,
        Ok::<Expr, Vec<Simple<Token, Span>>>(Expr::LogExpr(LogExpr::Selector(Selector {
            labels: vec![LabelMatcher {
                name: "job".to_string(),
                value: "foo".to_string(),
                matcher_type: MatcherType::Eq,
            }],
        }))),
    ),
    multi_label: (
        r#"{job="foo", app="bar"}"#,
        Ok::<Expr, Vec<Simple<Token, Span>>>(Expr::LogExpr(LogExpr::Selector(Selector {
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
    ),
    missing_l_paren: (
        r#"job="foo"}"#,
        Err::<Expr, Vec<Simple<Token, Span>>>(vec![Simple::expected_input_found(
            0..3,
            vec![Some(Token::Ctrl('{'))],
            Some(Token::Ident("job".to_string())),
        )
        .with_label("label_matchers")]),
    ),

    missing_r_paren: (
        r#"{job="foo"abc"#,
        Err::<Expr, Vec<Simple<Token, Span>>>(vec![Simple::expected_input_found(
            10..13,
            vec![Some(Token::Ctrl(',')), Some(Token::Ctrl('}'))],
            Some(Token::Ident("abc".to_string())),
        )
        .with_label("label_matchers")]),
    ),
}
