extern crate glob;
extern crate nixrs;
#[macro_use]
extern crate pretty_assertions;

use nixrs::context::EvalContext;
use nixrs::lex::Lexer;
use std::io::{self, Read};
use std::fmt;
use std::fs::File;
use std::path::Path;

#[derive(Eq, Ord, PartialEq, PartialOrd)]
struct DebugAsDisplay<T: fmt::Display>(T);

impl<T: fmt::Display> fmt::Debug for DebugAsDisplay<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.0, f)
    }
}

#[test]
fn lexer_tests() {
    for glob_result in glob::glob("tests/lexer/*.in").expect("Couldn't parse glob") {
        let input_file = glob_result.expect("Couldn't access path in glob");
        let output_file = input_file.with_extension("out");

        let source = read_file(&input_file).expect("Couldn't read input file");
        let expected = read_file(&output_file).expect("Couldn't read expected output file");

        let ctx = EvalContext::new();
        let lexer = Lexer::new(&ctx, input_file.to_str().unwrap(), &source);
        let output = lexer.debug_string();

        println!("testing {}", input_file.display());
        assert_eq!(DebugAsDisplay(output), DebugAsDisplay(expected));
    }
}

fn read_file(input_file: &Path) -> io::Result<String> {
    let mut source = String::new();
    File::open(input_file)?.read_to_string(&mut source)?;
    Ok(source)
}
