#[derive(Clone, Debug)]
enum TokenKind {
    Id(Symbol),
    Int(i64),
    Float(f64),
    Path(Symbol),

    Bang, // !
    At,   // @
    Star, // *
    Minus,
    Plus,
    Equals,

    Colon,
    Semicolon,
    Comma,
    DollarBrace, // `${` can't be written as `$ {`

    ParenL,
    ParenR,
    BracketL,
    BracketR,
    BraceL,
    BraceR,
}
