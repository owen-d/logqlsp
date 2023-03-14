use std::collections::HashMap;

use log::warn;
use nom::InputIter;
use tower_lsp::lsp_types::{SemanticToken, SemanticTokenType};

use crate::parser::{
    lexer::{Token, TokenStream},
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

impl SemanticTokens for Vec<Spanned<Offset, Token>> {
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
                delta_line: line_diff,
                delta_start: offset_diff,
                length: content_ln as u32,
                token_type: token_type as u32,
                token_modifiers_bitset: 0,
            });
        }
    }
}

impl SemanticTokens for TokenStream<'_> {
    fn semantic_tokens(&self, tokens: &mut Vec<SemanticToken>) {
        // spans are 1-indexed (god knows why, probably b/c that's how editors usually show lines/columns?)
        let mut last_line = 1;
        let mut last_start = 1;
        for tok in self.iter_elements() {
            // skip zero length tokens
            let content_ln = if let Some(ln) = tok.content_ln {
                ln
            } else {
                continue;
            };

            let sp = tok.span;
            let tok = tok.value;
            let token_type = LEGEND_TYPE
                .iter()
                .position(|item| item == &tok.semantic_token_type())
                .unwrap();

            let line_diff = sp.location_line() - last_line;
            last_line = last_line + line_diff;

            // offset is line-relative
            let offset_diff = if line_diff == 0 {
                let curr = sp.get_utf8_column();
                let res = curr - last_start;
                last_start = last_start + curr;
                res
            } else {
                let curr = sp.get_utf8_column();
                last_start = curr;
                curr
            };

            tokens.push(SemanticToken {
                delta_line: line_diff,
                delta_start: offset_diff as u32,
                length: content_ln as u32,
                token_type: token_type as u32,
                token_modifiers_bitset: 0,
            });
        }
    }
}
