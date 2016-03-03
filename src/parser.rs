use symbol_table::{Symbol, SymbolTable};

#[derive(Clone, Debug, PartialEq)]
enum TokenKind {
    // Basic
    Id(Symbol),
    Int(i64),
    Float(f64),
    Path(Symbol),

    // String-related
    Uri(String),
    StrPart(String),
    IndentStrPart(String),
    Quote,          // "
    IndentQuote,    // ''
    DollarBrace,    // ${

    // Operators
    Bang,       // !
    Star,       // *
    Minus,      // -
    Plus,       // +
    Divide,     // /
    Assign,     // =
    Equals,     // ==
    NotEquals,  // !=
    And,        // &&
    Or,         // ||
    Implies,    // ->
    Concat,     // ++

    // Other syntax
    At,         // @
    Comma,      // ,
    Dot,        // .
    Ellipsis,   // ...
    Question,   // ?
    Colon,      // :
    Semicolon,  // ;

    // Delimiters
    ParenL,     // (
    ParenR,     // )
    BracketL,   // [
    BracketR,   // ]
    BraceL,     // {
    BraceR,     // }
}
