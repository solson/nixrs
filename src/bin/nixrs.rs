extern crate clap;
extern crate nixrs;

use clap::{Arg, App};
use nixrs::context::EvalContext;
use nixrs::lex::Lexer;
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

    let ctx = EvalContext::new();
    let lexer = Lexer::new(&ctx, input_file, &source);
    println!("{}", lexer.debug_string());

    Ok(())
}
