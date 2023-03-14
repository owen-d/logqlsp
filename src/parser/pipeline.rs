use nom::{
    error::{convert_error, ContextError, ParseError, VerboseError},
    multi::fold_many1,
    Finish, IResult,
};
use nom_supreme::tag::TagError;

use super::{
    errors::Errorable,
    lexer::{Token, TokenStream},
    parser::{parse_filter, parse_string, Filter},
    utils::{RefSpanned, Span, Spanned},
};

#[derive(Debug, Clone, PartialEq)]
pub struct PipelineExpr<S> {
    pub stages: Vec<Spanned<S, PipelineStage<S>>>,
}

pub fn parse_pipeline_expr<'a, E>(
    input: TokenStream<'a>,
) -> IResult<TokenStream<'a>, RefSpanned<'a, PipelineExpr<Span<'a>>>, E>
where
    E: Errorable<TokenStream<'a>>,
{
    let (s, stages) = fold_many1(parse_pipeline_stage, Vec::new, |mut acc: Vec<_>, item| {
        acc.push(item);
        acc
    })(input)?;

    Ok((
        s,
        Spanned::from((stages[0].span.clone(), PipelineExpr { stages })),
    ))
}

#[derive(Debug, Clone, PartialEq)]
pub enum PipelineStage<S> {
    LineFilter(Spanned<S, LineFilter<S>>),
    // Placeholder for | logfmt, etc
}

pub fn parse_pipeline_stage<'a, E>(
    input: TokenStream<'a>,
) -> IResult<TokenStream<'a>, RefSpanned<'a, PipelineStage<Span<'a>>>, E>
where
    E: Errorable<TokenStream<'a>>,
{
    let (input, lf) = parse_line_filter(input)?;
    Ok((
        input,
        Spanned::from((lf.span.clone(), PipelineStage::LineFilter(lf))),
    ))
}

#[derive(Debug, Clone, PartialEq)]
pub struct LineFilter<S> {
    pub filter: Spanned<S, Filter>,
    pub s: Spanned<S, String>,
}

pub fn parse_line_filter<'a, E>(
    input: TokenStream<'a>,
) -> IResult<TokenStream<'a>, RefSpanned<'a, LineFilter<Span<'a>>>, E>
where
    E: Errorable<TokenStream<'a>>,
{
    let (input, filter) = parse_filter(input)?;
    let (input, s) = parse_string(input)?;
    let lf = Spanned::from((filter.span.clone(), LineFilter { filter, s }));
    Ok((input, lf))
}

#[test]
fn test_parse_pipeline_expr() {
    let input = r#"|= "foo" != "bar""#;
    let (_, toks) = super::lexer::lex::<VerboseError<Span>>(input).unwrap();

    let ts = TokenStream::new(input, &toks);

    let got = parse_pipeline_expr::<VerboseError<_>>(ts.clone()).finish();
    match got {
        Ok((_, xs)) => {
            assert_eq!(2, xs.value.stages.len());
        }
        Err(e) => {
            let s = convert_error(ts, e);
            assert_eq!("", s)
        }
    }
}
