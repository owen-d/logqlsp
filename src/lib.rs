// use chumsky::{prelude::Simple, Parser};

use chumsky::{Parser, Stream};
use logql::lexer;

pub mod logql;

#[cfg(test)]
mod logql_test;

pub fn libmain() {
    let src = std::fs::read_to_string(std::env::args().nth(1).unwrap()).unwrap();
    let len = src.chars().count();
    let lexed = lexer().parse(src);
    match lexed {
        Ok(tokens) => {
            let stream = Stream::from_iter(len..len + 1, tokens.into_iter());
            match logql::expr_parser().parse(stream) {
                Ok(output) => println!("{:#?}", output),
                Err(eval_errs) => eval_errs
                    .into_iter()
                    .for_each(|e| println!("Evaluation error: {}", e)),
            }
        }
        Err(parse_errs) => parse_errs
            .into_iter()
            .for_each(|e| println!("Parse error: {}", e)),
    }
}
