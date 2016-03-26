use itertools::Itertools;
use std::str::Chars;

use context::EvalContext;
use symbol_table::{Symbol, SymbolTable};

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Pos {
    column: usize,
    line: usize,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Span {
    filename: Symbol,
    start: Pos,
    end: Pos,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Spanned<T> {
    pub val: T,
    pub span: Span,
}

pub type Token = Spanned<TokenKind>;

#[derive(Clone, Debug, PartialEq)]
pub enum TokenKind {
    Unknown,

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
    Mult,       // *
    Minus,      // -
    Plus,       // +
    Divide,     // /
    Less,       // <
    Greater,    // >
    LessEq,     // <=
    GreaterEq,  // >=
    Assign,     // =
    Equals,     // ==
    NotEquals,  // !=
    And,        // &&
    Or,         // ||
    Implies,    // ->
    Not,        // !
    Update,     // //
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

pub struct Lexer<'ctx, 'src> {
    ectx: &'ctx EvalContext,
    source: &'src str,
    chars: Chars<'src>,
    filename: Symbol,
    pos: Pos,
}

impl<'ctx, 'src> Lexer<'ctx, 'src> {
    fn new(ectx: &'ctx EvalContext, filename: &str, source: &'src str) -> Self {
        Lexer {
            ectx: ectx,
            source: source,
            chars: source.chars(),
            filename: ectx.intern(filename),
            pos: Pos { line: 1, column: 1 },
        }
    }

    fn lex_int(&mut self) -> Token {
        let start = self.pos;
        let digits: String = self.chars.take_while_ref(|c| c.is_digit(10)).collect();
        self.spanned(start, self.pos, TokenKind::Int(digits.parse::<i64>().unwrap()))
    }

    fn peek(&self) -> Option<char> {
        self.chars.clone().next()
    }

    fn spanned<T>(&self, start: Pos, end: Pos, val: T) -> Spanned<T> {
        Spanned {
            val: val,
            span: Span { filename: self.filename, start: start, end: end },
        }
    }
}

impl<'ctx, 'src> Iterator for Lexer<'ctx, 'src> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        match self.peek() {
            Some(c) if c.is_digit(10) => Some(self.lex_int()),
            Some(c) => panic!("unhandled char: {}", c),
            None => None,
        }
    }
}

pub fn lex(ectx: &EvalContext, filename: &str, source: &str) -> Vec<Token> {
    Lexer::new(ectx, filename, source).collect()
}

#[cfg(test)]
mod test {
    use super::lex;

    #[test]
    fn test_lex() {
        assert_eq!(lex(""), []);
        assert_eq!(lex("0"), []);
    }
}
