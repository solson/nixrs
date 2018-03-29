use symbol::Symbol;

#[derive(Debug)]
pub enum Expr {
    Identifier(Symbol),
    IntLiteral(u64), // TODO(solson): What type should we use?
    FloatLiteral(f64), // TODO(solson): What type should we use?
    String(Symbol),
}


// #[derive(Debug)]
// pub enum ParseError {
//     UnexpectedEnd,
//     InvalidIntLiteral,
//     InvalidFloatLiteral,
// }

// pub type ParseResult<T> = Result<T, ParseError>;
