extern crate clap;
extern crate nixrs;

use clap::{Arg, App};
use nixrs::parse;
use std::fs::File;
use std::io;
use std::io::prelude::*;

fn main() {
    result_main().unwrap();
}

fn result_main() -> io::Result<()> {
    let matches = App::new("nixrs")
        .version("0.1.0")
        .author("Scott Olson <scott@solson.me>")
        .about("Nix evaluator")
        .arg(Arg::with_name("INPUT")
             .help("The input file to use")
             .required(true))
        .get_matches();

    let input_file = matches.value_of("INPUT").unwrap();
    let mut source = String::new();
    File::open(input_file)?.read_to_string(&mut source)?;

    // let ctx = EvalContext::new();
    // let lexer = lex::Lexer::new(&ctx, input_file, &source);
    // let lalrpop_lexer = lex::lalrpop::Lexer(lexer);
    // let parser = parse::ExprParser::new();
    // let ast = parser.parse(lalrpop_lexer).unwrap();
    let parser = parse::ExprParser::new();
    let ast = parser.parse(&source).unwrap();
    println!("{:?}", ast);

    // let mut parser = Parser::new(lexer);
    // println!("{:?}", parser.parse_expr());
    // println!("{}", lexer.debug_string());

    Ok(())
}
