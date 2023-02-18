use super::logql::*;
use chumsky::Parser;
#[test]
fn parse_simple() {
    struct TestCase {
        input: &'static str,
        expected: Expr,
    }
    for case in vec![
        TestCase {
            input: r#"{job="foo"}"#,
            expected: Expr::LogExpr(LogExpr::Selector(Selector {
                labels: vec![LabelMatcher {
                    name: "job".to_string(),
                    value: "foo".to_string(),
                    matcher_type: MatcherType::Eq,
                }],
            })),
        },
        TestCase {
            input: r#"{job="foo", app="bar"}"#,
            expected: Expr::LogExpr(LogExpr::Selector(Selector {
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
            })),
        },
    ] {
        assert_eq!(Ok(case.expected), parser().parse(case.input));
    }
}
