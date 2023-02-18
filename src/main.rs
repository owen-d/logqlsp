use std::collections::HashMap;

use dashmap::DashMap;
use ropey::Rope;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::notification::Notification;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};
#[derive(Debug)]
struct Backend {
    client: Client,
    ast_map: DashMap<String, HashMap<String, Func>>,
    document_map: DashMap<String, Rope>,
    semantic_token_map: DashMap<String, Vec<ImCompleteSemanticToken>>,
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
                                        pattern: Some("*.logql".to_string()),
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
                // definition: Some(GotoCapability::default()),
                definition_provider: Some(OneOf::Left(true)),
                references_provider: Some(OneOf::Left(true)),
                rename_provider: Some(OneOf::Left(true)),
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
}
#[derive(Debug, Deserialize, Serialize)]
struct InlayHintParams {
    path: String,
}

enum CustomNotification {}
impl Notification for CustomNotification {
    type Params = InlayHintParams;
    const METHOD: &'static str = "custom/notification";
}
struct TextDocumentItem {
    uri: Url,
    text: String,
    version: i32,
}
impl Backend {
    async fn inlay_hint(&self, params: InlayHintParams) -> Result<Vec<(usize, usize, String)>> {
        let mut hashmap = HashMap::new();
        if let Some(ast) = self.ast_map.get(&params.path) {
            ast.iter().for_each(|(_, v)| {
                type_inference(&v.body, &mut hashmap);
            });
        }

        let inlay_hint_list = hashmap
            .into_iter()
            .map(|(k, v)| {
                (
                    k.start,
                    k.end,
                    match v {
                        nrs_language_server::chumsky::Value::Null => "null".to_string(),
                        nrs_language_server::chumsky::Value::Bool(_) => "bool".to_string(),
                        nrs_language_server::chumsky::Value::Num(_) => "number".to_string(),
                        nrs_language_server::chumsky::Value::Str(_) => "string".to_string(),
                        nrs_language_server::chumsky::Value::List(_) => "[]".to_string(),
                        nrs_language_server::chumsky::Value::Func(_) => v.to_string(),
                    },
                )
            })
            .collect::<Vec<_>>();
        Ok(inlay_hint_list)
    }
    async fn on_change(&self, params: TextDocumentItem) {
        let rope = ropey::Rope::from_str(&params.text);
        self.document_map
            .insert(params.uri.to_string(), rope.clone());
        let (ast, errors, semantic_tokens) = parse(&params.text);
        self.client
            .log_message(MessageType::INFO, format!("{:?}", errors))
            .await;
        let diagnostics = errors
            .into_iter()
            .filter_map(|item| {
                let (message, span) = match item.reason() {
                    chumsky::error::SimpleReason::Unclosed { span, delimiter } => {
                        (format!("Unclosed delimiter {}", delimiter), span.clone())
                    }
                    chumsky::error::SimpleReason::Unexpected => (
                        format!(
                            "{}, expected {}",
                            if item.found().is_some() {
                                "Unexpected token in input"
                            } else {
                                "Unexpected end of input"
                            },
                            if item.expected().len() == 0 {
                                "something else".to_string()
                            } else {
                                item.expected()
                                    .map(|expected| match expected {
                                        Some(expected) => expected.to_string(),
                                        None => "end of input".to_string(),
                                    })
                                    .collect::<Vec<_>>()
                                    .join(", ")
                            }
                        ),
                        item.span(),
                    ),
                    chumsky::error::SimpleReason::Custom(msg) => (msg.to_string(), item.span()),
                };

                let diagnostic = || -> Option<Diagnostic> {
                    // let start_line = rope.try_char_to_line(span.start)?;
                    // let first_char = rope.try_line_to_char(start_line)?;
                    // let start_column = span.start - first_char;
                    let start_position = offset_to_position(span.start, &rope)?;
                    let end_position = offset_to_position(span.end, &rope)?;
                    // let end_line = rope.try_char_to_line(span.end)?;
                    // let first_char = rope.try_line_to_char(end_line)?;
                    // let end_column = span.end - first_char;
                    Some(Diagnostic::new_simple(
                        Range::new(start_position, end_position),
                        message,
                    ))
                }();
                diagnostic
            })
            .collect::<Vec<_>>();

        self.client
            .publish_diagnostics(params.uri.clone(), diagnostics, Some(params.version))
            .await;

        if let Some(ast) = ast {
            self.ast_map.insert(params.uri.to_string(), ast);
        }
        self.client
            .log_message(MessageType::INFO, &format!("{:?}", semantic_tokens))
            .await;
        self.semantic_token_map
            .insert(params.uri.to_string(), semantic_tokens);
    }
}

#[tokio::main]
async fn main() {
    env_logger::init();

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::build(|client| Backend {
        client,
        ast_map: DashMap::new(),
        document_map: DashMap::new(),
        semantic_token_map: DashMap::new(),
    })
    .custom_method("custom/inlay_hint", Backend::inlay_hint)
    .finish();
    Server::new(stdin, stdout, socket).serve(service).await;
}

fn offset_to_position(offset: usize, rope: &Rope) -> Option<Position> {
    let line = rope.try_char_to_line(offset).ok()?;
    let first_char = rope.try_line_to_char(line).ok()?;
    let column = offset - first_char;
    Some(Position::new(line as u32, column as u32))
}
