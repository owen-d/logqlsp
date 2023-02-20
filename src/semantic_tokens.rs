use tower_lsp::lsp_types::SemanticTokenType;

pub const STRING: SemanticTokenType = SemanticTokenType::new("string");
pub const IDENT: SemanticTokenType = SemanticTokenType::new("ident");
pub const OPERATOR: SemanticTokenType = SemanticTokenType::new("operator");
pub const CONTROL: SemanticTokenType = SemanticTokenType::new("control");

pub const LEGEND_TYPE: &[SemanticTokenType] = &[STRING, IDENT, OPERATOR, CONTROL];
