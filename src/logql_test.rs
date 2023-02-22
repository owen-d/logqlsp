use super::logql::*;
use chumsky::{prelude::Simple, Error};

macro_rules! parse_tests {
    ($($name:ident: $case:expr,)*) => {
    $(
        #[test]
        fn $name() {
            let (expr, errs, _) = parse($case.0);
            let exp = $case.1;
            if let Err(exp_errs) = exp {
                assert!(exp_errs.len() == errs.len());
                exp_errs.into_iter()
                    .zip(errs.into_iter())
                    .for_each(|(exp, got)| {
                        assert_eq!(exp, got, "err mismatch");
                        // For some reason the error messages
                        // aren't checked in the assert_eq! macro
                        let (exp_msg, got_msg) = (format!("{}", exp), format!("{}", got));
                        assert_eq!(exp_msg, got_msg, "err msg");
                    });
            }else {
                assert!(expr.is_some(), "{:?}", errs);
                assert_eq!(exp.unwrap(), expr.unwrap(), "expr mismatch")
            }
        }
    )*
    }
}

parse_tests! {
    single_label: (
        r#"{job="foo"}"#,
        Ok::<Expr, Vec<Simple<String, Span>>>(
            Expr::LogExpr((LogExpr::Selector((
                Selector {
                labels: vec![(
                    LabelMatcher {
                    name: (
                        "job".to_string(),
                    1..4,
                ),
                    value: (
                        "foo".to_string(),
                    5..10,
                ),
                    matcher_type: (
                        MatcherType::Eq,
                        4..5,
                    ),
                },
                1..10,
            )],
            },
            0..11,
        )),
            0..11,
)
        ),
    ),
),
    multi_label: (
        r#"{job="foo", app="bar"}"#,
        Ok::<Expr, Vec<Simple<String, Span>>>(
            Expr::LogExpr((
                LogExpr::Selector(
                    (
                        Selector {
                            labels: vec![
                                (
                                    LabelMatcher {
                                        name: ("job".to_string(), 1..4),
                                        value: ("foo".to_string(),5..10),
                                        matcher_type: (MatcherType::Eq, 4..5),
                                    },
                                    1..10,
                                ),
                                (
                                    LabelMatcher {
                                        name: ("app".to_string(), 12..16),
                                        value: ("bar".to_string(), 17..22),
                                        matcher_type: (MatcherType::Eq, 16..17),
                                    },
                                    12..22,
                                ),
                            ],
                        },
                        0..22,
                    ),
                ),
                0..22,
            )),
        ),
    ),
    missing_l_paren: (
        r#"job="foo"}"#,
        Err::<Expr, Vec<Simple<String, Span>>>(vec![Simple::expected_input_found(
            0..3,
            vec![Some("{".to_string())],
            Some("job".to_string()),
        )
        .with_label("label_matchers")]),
    ),

    missing_r_paren: (
        r#"{job="foo"abc"#,
        Err::<Expr, Vec<Simple<String, Span>>>(vec![Simple::expected_input_found(
            10..13,
            vec![Some(",".to_string()), Some("}".to_string())],
            Some("abc".to_string()),
        )
        .with_label("label_matchers")]),
    ),
}
