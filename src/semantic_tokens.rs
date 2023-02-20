use tower_lsp::lsp_types::SemanticTokenType;

use crate::logql::{Expr, InCompleteSemanticToken, LogExpr, Spanned};

pub const STRING: SemanticTokenType = SemanticTokenType::new("string");
pub const IDENT: SemanticTokenType = SemanticTokenType::new("ident");
pub const OPERATOR: SemanticTokenType = SemanticTokenType::new("operator");
pub const CONTROL: SemanticTokenType = SemanticTokenType::new("control");

pub const LEGEND_TYPE: &[SemanticTokenType] = &[STRING, IDENT, OPERATOR, CONTROL];

pub fn semantic_token_from_ast(
    ast: &Spanned<Expr>,
    semantic_tokens: &mut Vec<InCompleteSemanticToken>,
) -> Vec<InCompleteSemanticToken> {
    match &ast.0 {
        Expr::Error => {}
        Expr::LogExpr(expr) => match expr {
            LogExpr::Selector(s) => {
              s.
            },
        },
    };
    todo!();
}
