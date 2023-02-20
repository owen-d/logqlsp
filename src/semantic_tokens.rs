use tower_lsp::lsp_types::SemanticTokenType;

use crate::logql::{Expr, InCompleteSemanticToken, LogExpr, Spanned};

pub const STRING: SemanticTokenType = SemanticTokenType::new("string");
pub const IDENT: SemanticTokenType = SemanticTokenType::new("ident");
pub const OPERATOR: SemanticTokenType = SemanticTokenType::new("operator");
pub const CONTROL: SemanticTokenType = SemanticTokenType::new("control");

pub const LEGEND_TYPE: &[SemanticTokenType] = &[STRING, IDENT, OPERATOR, CONTROL];

pub fn semantic_token_from_ast(ast: &Spanned<Expr>) -> Vec<InCompleteSemanticToken> {
    let mut semantic_tokens = vec![];
    match &ast.0 {
        Expr::Error => {}
        Expr::LogExpr((expr, _)) => match expr {
            LogExpr::Selector((sel, span)) => {
                // Push two control characters for the curly braces
                semantic_tokens.push(InCompleteSemanticToken {
                    start: span.start,
                    length: 1,
                    token_type: LEGEND_TYPE.iter().position(|t| t == &CONTROL).unwrap(),
                });
                semantic_tokens.push(InCompleteSemanticToken {
                    start: span.end - 1,
                    length: 1,
                    token_type: LEGEND_TYPE.iter().position(|t| t == &CONTROL).unwrap(),
                });

                sel.labels.iter().for_each(|(matcher, _)| {
                    semantic_tokens.push(InCompleteSemanticToken {
                        start: matcher.name.1.start,
                        length: matcher.name.1.end - matcher.name.1.start,
                        token_type: LEGEND_TYPE.iter().position(|t| t == &IDENT).unwrap(),
                    });
                    semantic_tokens.push(InCompleteSemanticToken {
                        start: matcher.matcher_type.1.start,
                        length: matcher.matcher_type.1.end - matcher.matcher_type.1.start,
                        token_type: LEGEND_TYPE.iter().position(|t| t == &OPERATOR).unwrap(),
                    });
                    semantic_tokens.push(InCompleteSemanticToken {
                        start: matcher.value.1.start,
                        length: matcher.value.1.end - matcher.value.1.start,
                        token_type: LEGEND_TYPE.iter().position(|t| t == &STRING).unwrap(),
                    });
                });
            }
        },
    };
    semantic_tokens
}
