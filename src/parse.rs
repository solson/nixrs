use itertools::Itertools;
use std::fmt;
use std::str::Chars;

use context::EvalContext;
use symbol_table::Symbol;

////////////////////////////////////////////////////////////////////////////////
// Token positions and spans
////////////////////////////////////////////////////////////////////////////////

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Pos {
    column: usize,
    line: usize,
}

impl fmt::Display for Pos {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
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

////////////////////////////////////////////////////////////////////////////////
// Token kinds
////////////////////////////////////////////////////////////////////////////////

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

////////////////////////////////////////////////////////////////////////////////
// CharsPos - A &str iterator which tracks line/column positions.
////////////////////////////////////////////////////////////////////////////////

/// An iterator wrapping a `std::str::Chars` iterator which also keeps track of the current line
/// and column position.
#[derive(Clone)]
struct CharsPos<'a> {
    chars: Chars<'a>,
    pos: Pos,
}

impl<'a> CharsPos<'a> {
    fn new(chars: Chars<'a>) -> Self {
        CharsPos { chars: chars, pos: Pos { line: 1, column: 1 } }
    }

    fn as_str(&self) -> &'a str {
        self.chars.as_str()
    }
}

impl<'a> Iterator for CharsPos<'a> {
    type Item = char;

    fn next(&mut self) -> Option<char> {
        let opt_c = self.chars.next();
        match opt_c {
            Some('\n') => { self.pos.line += 1; self.pos.column = 1; }
            Some(_) => { self.pos.column += 1; }
            None => {}
        }
        opt_c
    }
}

////////////////////////////////////////////////////////////////////////////////
// Character classification
////////////////////////////////////////////////////////////////////////////////

fn is_whitespace(c: char) -> bool {
    match c { ' ' | '\t' | '\r' | '\n' => true, _ => false }
}

////////////////////////////////////////////////////////////////////////////////
// Lexer
////////////////////////////////////////////////////////////////////////////////

pub struct Lexer<'ctx, 'src> {
    ectx: &'ctx EvalContext,
    source: &'src str,
    chars: CharsPos<'src>,
    filename: Symbol,
}

impl<'ctx, 'src> Iterator for Lexer<'ctx, 'src> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        match self.peek() {
            Some(c) if c.is_digit(10) => Some(self.lex_int()),
            Some(c) if is_whitespace(c) || c == '#' => {
                self.skip_whitespace_and_comments();
                self.next()
            }
            Some(c) => panic!("unhandled char: {}", c),
            None => None,
        }
    }
}

impl<'ctx, 'src> Lexer<'ctx, 'src> {
    pub fn new(ectx: &'ctx EvalContext, filename: &str, source: &'src str) -> Self {
        Lexer {
            ectx: ectx,
            source: source,
            chars: CharsPos::new(source.chars()),
            filename: ectx.intern(filename),
        }
    }

    fn lex_int(&mut self) -> Token {
        let start = self.pos();
        let chars = self.chars.as_str();
        let num_digits = self.chars.take_while_ref(|c| c.is_digit(10)).count();
        let digits = &chars[..num_digits];

        // TODO(tsion): Detect and diagnose integer overflow.
        self.spanned(start, self.pos(), TokenKind::Int(digits.parse::<i64>().unwrap()))
    }

    // TODO(tsion): Long comments of the form /* foo */
    fn skip_whitespace_and_comments(&mut self) {
        while let Some(c) = self.peek() {
            if c == '#' {
                for _ in self.chars.take_while_ref(|&d| d != '\n') {}
            } else if is_whitespace(c) {
                self.chars.next();
            } else {
                break;
            }
        }
    }

    fn peek(&self) -> Option<char> {
        self.chars.clone().next()
    }

    fn pos(&self) -> Pos {
        self.chars.pos
    }

    fn spanned<T>(&self, start: Pos, end: Pos, val: T) -> Spanned<T> {
        Spanned {
            val: val,
            span: Span { filename: self.filename, start: start, end: end },
        }
    }
}

pub fn lex(ectx: &EvalContext, filename: &str, source: &str) -> Vec<Token> {
    Lexer::new(ectx, filename, source).collect()
}

////////////////////////////////////////////////////////////////////////////////
// Tests
////////////////////////////////////////////////////////////////////////////////

#[cfg(test)]
mod test {
    use context::EvalContext;
    use parse::{Lexer, TokenKind};
    use parse::TokenKind::*;

    macro_rules! assert_lex {
        ($src:expr => [ $($span:expr => $token:expr),* $(,)* ]) => ({
            let expected = [ $(($token, String::from($span))),* ];
            assert_eq!(lex($src), expected);
        })
    }

    fn lex(src: &str) -> Vec<(TokenKind, String)> {
        Lexer::new(&EvalContext::new(), "<test>", src)
            .map(|t| (t.val, format!("{}-{}", t.span.start, t.span.end)))
            .collect()
    }

    #[test]
    fn test_ints() {
        assert_lex!("0" => ["1:1-1:2" => Int(0)]);
        assert_lex!("42" => ["1:1-1:3" => Int(42)]);
        assert_lex!("1 2" => ["1:1-1:2" => Int(1), "1:3-1:4" => Int(2)]);
    }

    #[test]
    fn test_whitespace_and_comments() {
        assert_lex!("" => []);
        assert_lex!("\n\n\t\r\n # comments and whitespace don't mean a thing\n\n\n" => []);
        assert_lex!("\n  1\n13 \n 42\n" => [
            "2:3-2:4" => Int(1),
            "3:1-3:3" => Int(13),
            "4:2-4:4" => Int(42),
        ]);
        assert_lex!("# line comment\n  1 ### lex this please\n13 \n 42\n# end of file" => [
            "2:3-2:4" => Int(1),
            "3:1-3:3" => Int(13),
            "4:2-4:4" => Int(42),
        ]);
    }
}
