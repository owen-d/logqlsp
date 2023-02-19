// use chumsky::{prelude::Simple, Parser};

pub mod logql;

#[cfg(test)]
mod logql_test;

// pub fn libmain() {
//     let src = std::fs::read_to_string(std::env::args().nth(1).unwrap()).unwrap();

//     match parser().parse(src) {
//         Ok(ast) => match eval(ast) {
//             Ok(output) => println!("{:#?}", output),
//             Err(eval_errs) => eval_errs
//                 .into_iter()
//                 .for_each(|e| println!("Evaluation error: {}", e)),
//         },
//         Err(parse_errs) => parse_errs
//             .into_iter()
//             .for_each(|e| println!("Parse error: {}", e)),
//     }
// }

// fn eval(ast: logql::Expr) -> Result<logql::Expr, Vec<Simple<char>>> {
//     Ok(ast)
// }
