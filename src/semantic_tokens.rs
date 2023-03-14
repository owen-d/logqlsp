use std::collections::HashMap;

use log::warn;
use nom::InputIter;
use nom_supreme::final_parser::Location;
use tower_lsp::lsp_types::{SemanticToken, SemanticTokenType};

use crate::parser::{
    lexer::{Head, Token, TokenStream},
    parser::LogExpr,
    utils::{Offset, Spanned},
};

pub const LEGEND_TYPE: &[SemanticTokenType] = &[
    SemanticTokenType::COMMENT,
    SemanticTokenType::STRING,
    SemanticTokenType::VARIABLE,
    SemanticTokenType::OPERATOR,
];

pub trait SemanticTokens {
    fn semantic_tokens(&self, tokens: &mut Vec<SemanticToken>);
}

impl SemanticTokens for Vec<Spanned<Location, Token>> {
    fn semantic_tokens(&self, tokens: &mut Vec<SemanticToken>) {
        let mut last_line = 1;
        let mut last_start = 1;

        for tok in self.iter() {
            // skip zero length tokens
            let content_ln = if let Some(ln) = tok.content_ln {
                ln
            } else {
                continue;
            };

            let sp = &tok.span;
            let tok = &tok.value;
            let token_type = LEGEND_TYPE
                .iter()
                .position(|item| item == &tok.semantic_token_type())
                .unwrap();

            let line_diff = sp.line - last_line;
            last_line = last_line + line_diff;

            // offset is line-relative
            let offset_diff = if line_diff == 0 {
                let res = sp.column - last_start;
                last_start = last_start + res;
                res
            } else {
                let curr = sp.column;
                last_start = curr;
                curr
            };

            tokens.push(SemanticToken {
                delta_line: line_diff as u32,
                delta_start: offset_diff as u32,
                length: content_ln as u32,
                token_type: token_type as u32,
                token_modifiers_bitset: 0,
            });
        }
    }
}
