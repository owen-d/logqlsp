use std::collections::HashMap;

use im_rc::Vector;
use logql_language_server::logql::parse;

fn main() {
    let source = include_str!("./test.logql");
    // let source = r#"
    // test
    // println!("{:?}", &source[10..11]);
    let (ast, errors, semantic_tokens) = parse(source);
    println!("{:?}", errors);
    // if let Some(ref ast) = ast {
    //     println!("{:#?}", ast);
    // } else {
    //     println!("{:?}", errors);
    // }
    // println!("{:?}", semantic_tokens);
}
