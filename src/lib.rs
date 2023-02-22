pub mod completion;
pub mod logql;
pub mod semantic_tokens;

#[cfg(test)]
mod logql_test;

pub fn libmain() {
    let src = std::fs::read_to_string(std::env::args().nth(1).unwrap()).unwrap();

    let (expr, errs, _semantic_tokens) = logql::parse(&src);
    if let Some(expr) = expr {
        println!("{:#?}", expr)
    } else {
        errs.into_iter()
            .for_each(|e| println!("Parse error: {}", e))
    }
}
