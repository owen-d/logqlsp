use std::sync::Arc;

use dashmap::DashMap;
use log::warn;
use logql_language_server::parser::errors::{entire_range, SuggestiveError};
use logql_language_server::parser::lexer::{lex, Token, TokenStream};
use logql_language_server::parser::parser::{parse, parse_log_expr, LogExpr};
use logql_language_server::parser::utils::{Offset, Span, Spanned};
use logql_language_server::semantic_tokens::{SemanticTokens, LEGEND_TYPE};
use nom::error::{convert_error, VerboseError, VerboseErrorKind};
use nom::Finish;
use nom_supreme::error::ErrorTree;
use nom_supreme::final_parser::final_parser;
use nom_supreme::final_parser::Location;
use ropey::Rope;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::{self, *};
use tower_lsp::{Client, LanguageServer, LspService, Server};

#[derive(Debug)]
struct Backend {
    client: Client,
    // stores text input
    document_map: DashMap<String, File>,
}

#[derive(Debug)]
struct File {
    uri: String,
    tokens: Option<Vec<Spanned<Location, Token>>>,
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            server_info: None,
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                completion_provider: Some(CompletionOptions {
                    resolve_provider: Some(false),
                    trigger_characters: Some(vec![".".to_string()]),
                    work_done_progress_options: Default::default(),
                    all_commit_characters: None,
                }),
                execute_command_provider: Some(ExecuteCommandOptions {
                    commands: vec!["dummy.do_something".to_string()],
                    work_done_progress_options: Default::default(),
                }),

                workspace: Some(WorkspaceServerCapabilities {
                    workspace_folders: Some(WorkspaceFoldersServerCapabilities {
                        supported: Some(true),
                        change_notifications: Some(OneOf::Left(true)),
                    }),
                    file_operations: None,
                }),
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensRegistrationOptions(
                        SemanticTokensRegistrationOptions {
                            text_document_registration_options: {
                                TextDocumentRegistrationOptions {
                                    document_selector: Some(vec![DocumentFilter {
                                        language: Some("logql".to_string()),
                                        scheme: Some("file".to_string()),
                                        // pattern: Some("*.logql".to_string()),
                                        pattern: None,
                                    }]),
                                }
                            },
                            semantic_tokens_options: SemanticTokensOptions {
                                work_done_progress_options: WorkDoneProgressOptions::default(),
                                legend: SemanticTokensLegend {
                                    token_types: LEGEND_TYPE.clone().into(),
                                    token_modifiers: vec![],
                                },
                                range: Some(true),
                                full: Some(SemanticTokensFullOptions::Bool(true)),
                            },
                            static_registration_options: StaticRegistrationOptions::default(),
                        },
                    ),
                ),
                references_provider: Some(OneOf::Left(false)),
                definition_provider: Some(OneOf::Left(false)),
                rename_provider: Some(OneOf::Left(false)),
                ..ServerCapabilities::default()
            },
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "initialized!")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_change_workspace_folders(&self, _: DidChangeWorkspaceFoldersParams) {
        self.client
            .log_message(MessageType::INFO, "workspace folders changed!")
            .await;
    }

    async fn did_change_configuration(&self, _: DidChangeConfigurationParams) {
        self.client
            .log_message(MessageType::INFO, "configuration changed!")
            .await;
    }

    async fn did_change_watched_files(&self, _: DidChangeWatchedFilesParams) {
        self.client
            .log_message(MessageType::INFO, "watched files have changed!")
            .await;
    }

    async fn execute_command(&self, _: ExecuteCommandParams) -> Result<Option<Value>> {
        self.client
            .log_message(MessageType::INFO, "command executed!")
            .await;

        match self.client.apply_edit(WorkspaceEdit::default()).await {
            Ok(res) if res.applied => self.client.log_message(MessageType::INFO, "applied").await,
            Ok(_) => self.client.log_message(MessageType::INFO, "rejected").await,
            Err(err) => self.client.log_message(MessageType::ERROR, err).await,
        }

        Ok(None)
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "file opened!")
            .await;
        self.on_change(TextDocumentItem {
            uri: params.text_document.uri,
            text: params.text_document.text,
            version: params.text_document.version,
        })
        .await
    }

    async fn did_change(&self, mut params: DidChangeTextDocumentParams) {
        self.on_change(TextDocumentItem {
            uri: params.text_document.uri,
            text: std::mem::take(&mut params.content_changes[0].text),
            version: params.text_document.version,
        })
        .await
    }

    async fn did_save(&self, _: DidSaveTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "file saved!")
            .await;
    }

    async fn did_close(&self, _: DidCloseTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "file closed!")
            .await;
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        self.client
            .log_message(MessageType::INFO, "completion")
            .await;
        let _ = params;
        Ok(None)
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let uri = params.text_document.uri.to_string();
        self.client
            .log_message(MessageType::INFO, "semantic_token_full")
            .await;

        match self.document_map.get(&uri) {
            Some(f) => match &f.tokens {
                Some(toks) => {
                    let mut tokens = Vec::new();
                    toks.semantic_tokens(&mut tokens);
                    return Ok(Some(SemanticTokensResult::Tokens(
                        lsp_types::SemanticTokens {
                            result_id: None,
                            data: tokens,
                        },
                    )));
                }
                None => Ok(None),
            },
            None => Ok(None),
        }
    }

    async fn semantic_tokens_range(
        &self,
        params: SemanticTokensRangeParams,
    ) -> Result<Option<SemanticTokensRangeResult>> {
        let uri = params.text_document.uri.to_string();
        self.client
            .log_message(MessageType::INFO, "semantic_token_range")
            .await;

        match self.document_map.get(&uri) {
            Some(f) => match &f.tokens {
                Some(toks) => {
                    let mut tokens = Vec::new();
                    toks.semantic_tokens(&mut tokens);
                    return Ok(Some(SemanticTokensRangeResult::Tokens(
                        lsp_types::SemanticTokens {
                            result_id: None,
                            data: tokens,
                        },
                    )));
                }
                None => Ok(None),
            },
            None => Ok(None),
        }
    }
}

impl Backend {
    async fn on_change(&self, params: TextDocumentItem) {
        self.client
            .log_message(MessageType::INFO, "did change")
            .await;
        let input = params.text.as_str();
        let lexed = lex::<ErrorTree<Span>>(input).finish();
        let mut f = File {
            uri: params.uri.to_string(),
            tokens: None,
        };

        let diagnostics = match lexed {
            Ok((_, toks)) => {
                // map spans into a non-referenced variant
                let mapped = toks
                    .clone()
                    .into_iter()
                    .map(|x| x.map_sp(|s| Location::locate_tail(input, s)))
                    .collect();
                f.tokens = Some(mapped);

                let x: core::result::Result<_, SuggestiveError<&str>> =
                    final_parser(parse::<SuggestiveError<_>>)(TokenStream::new(input, &toks));
                match x {
                    Ok(_expr) => {
                        // self.client
                        //     .log_message(MessageType::INFO, format!("expr: {:#?}", expr))
                        //     .await
                        vec![]
                    }
                    Err(e) => {
                        self.client
                            .log_message(MessageType::INFO, format!("parse error:\n{}", e))
                            .await;
                        e.diagnostics(input)
                    }
                }
            }
            Err(e) => {
                vec![Diagnostic {
                    severity: Some(DiagnosticSeverity::WARNING),
                    range: entire_range(input),
                    message: format!("lexing error:\n{}", e),
                    ..Default::default()
                }]
            }
        };

        self.document_map.insert(f.uri.clone(), f);
        self.client
            .publish_diagnostics(params.uri.clone(), diagnostics, Some(params.version))
            .await;

        // TODO: add diagnostics
    }
}

struct TextDocumentItem {
    uri: Url,
    text: String,
    version: i32,
}

#[tokio::main]
async fn main() {
    env_logger::init();

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::build(|client| Backend {
        client,
        document_map: DashMap::new(),
    })
    .finish();
    Server::new(stdin, stdout, socket).serve(service).await;
}
