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

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}-{}", self.start, self.end)
    }
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

fn is_identifier_start(c: char) -> bool {
    match c { 'a' ... 'z' | 'A' ... 'Z' | '_' => true, _ => false }
}

fn is_identifier_continue(c: char) -> bool {
    match c { 'a' ... 'z' | 'A' ... 'Z' | '_' | '0' ... '9' | '\'' | '-' => true, _ => false }
}

fn is_whitespace(c: char) -> bool {
    match c { ' ' | '\t' | '\r' | '\n' => true, _ => false }
}

////////////////////////////////////////////////////////////////////////////////
// Lexer
////////////////////////////////////////////////////////////////////////////////

pub struct Lexer<'ctx, 'src> {
    ctx: &'ctx EvalContext,
    chars: CharsPos<'src>,
    filename: Symbol,
}

impl<'ctx, 'src> Iterator for Lexer<'ctx, 'src> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        let start = self.pos();

        macro_rules! simple { ($kind:ident, $len:expr) => ({
            self.skip($len);
            Some(self.spanned(start, self.pos(), TokenKind::$kind))
        })}

        let c1 = match self.peek(0) { Some(c) => c, None => return None };
        let c2 = self.peek(1);

        match (c1, c2) {
            ('#', _) => {
                self.skip_line_comment();
                self.next()
            }

            ('/', Some('*')) => {
                self.skip_long_comment();
                self.next()
            }
            ('/', Some('/')) => simple!(Update, 2),
            ('/', _) => simple!(Divide, 1),

            ('*', _) => simple!(Mult, 1),
            ('@', _) => simple!(At, 1),
            (',', _) => simple!(Comma, 1),
            ('?', _) => simple!(Question, 1),
            (':', _) => simple!(Colon, 1),
            (';', _) => simple!(Semicolon, 1),
            ('(', _) => simple!(ParenL, 1),
            (')', _) => simple!(ParenR, 1),
            ('[', _) => simple!(BracketL, 1),
            (']', _) => simple!(BracketR, 1),
            ('{', _) => simple!(BraceL, 1),
            ('}', _) => simple!(BraceR, 1),
            ('-', Some('>')) => simple!(Implies, 2),
            ('+', Some('+')) => simple!(Concat, 2),
            ('<', Some('=')) => simple!(LessEq, 2),
            ('>', Some('=')) => simple!(GreaterEq, 2),
            ('=', Some('=')) => simple!(Equals, 2),
            ('!', Some('=')) => simple!(NotEquals, 2),
            ('&', Some('&')) => simple!(And, 2),
            ('|', Some('|')) => simple!(Or, 2),
            ('-', _) => simple!(Minus, 1),
            ('+', _) => simple!(Plus, 1),
            ('<', _) => simple!(Less, 1),
            ('>', _) => simple!(Greater, 1),
            ('=', _) => simple!(Assign, 1),
            ('!', _) => simple!(Not, 1),
            ('&', _) => simple!(Unknown, 1),
            ('|', _) => simple!(Unknown, 1),
            ('.', Some('.')) if self.peek(2) == Some('.') => simple!(Ellipsis, 3),
            ('.', _) => simple!(Dot, 1),

            (c, _) if is_whitespace(c) => {
                self.skip_whitespace();
                self.next()
            }

            (c, _) if is_identifier_start(c) => Some(self.lex_identifier()),
            (c, _) if c.is_digit(10) => Some(self.lex_int()),
            (c, _) => panic!("unhandled char: {}", c),
        }
    }
}

impl<'ctx, 'src> Lexer<'ctx, 'src> {
    pub fn new(ctx: &'ctx EvalContext, filename: &str, source: &'src str) -> Self {
        Lexer {
            ctx: ctx,
            chars: CharsPos::new(source.chars()),
            filename: ctx.intern(filename),
        }
    }

    fn lex_identifier(&mut self) -> Token {
        let start = self.pos();
        let chars = self.chars.as_str();
        let len = self.chars.take_while_ref(|&c| is_identifier_continue(c)).count();
        let identifier = &chars[..len];
        self.spanned(start, self.pos(), TokenKind::Id(self.ctx.intern(identifier)))
    }

    fn lex_int(&mut self) -> Token {
        let start = self.pos();
        let chars = self.chars.as_str();
        let num_digits = self.chars.take_while_ref(|c| c.is_digit(10)).count();
        let digits = &chars[..num_digits];

        // TODO(tsion): Detect and diagnose integer overflow.
        self.spanned(start, self.pos(), TokenKind::Int(digits.parse::<i64>().unwrap()))
    }

    /// Regexp from the Nix lexer: `[ \t\r\n]+`
    fn skip_whitespace(&mut self) {
        debug_assert!(self.peek(0).map(is_whitespace).unwrap_or(false));
        self.skip_while(is_whitespace);
    }

    /// Regexp from the Nix lexer: `#[^\r\n]*`
    fn skip_line_comment(&mut self) {
        debug_assert_eq!(self.peek(0), Some('#'));
        self.skip_while(|c| c != '\n' && c != '\r');
    }

    /// Regexp from the Nix lexer: `\/\*([^*]|\*[^\/])*\*\/`
    fn skip_long_comment(&mut self) {
        debug_assert!(self.peek_starts_with("/*"));
        self.skip(2);
        while !self.peek_starts_with("*/") {
            if self.peek(0).is_none() {
                // TODO(tsion): Report unterminated comment meeting end of file.
                return;
            }
            self.skip(1);
        }
        self.skip(2);
    }

    fn skip_while<F>(&mut self, mut f: F) where F: FnMut(char) -> bool {
        for _ in self.chars.take_while_ref(|&c| f(c)) {}
    }

    fn skip(&mut self, count: usize) {
        for _ in 0..count {
            let _c = self.chars.next();
            debug_assert!(_c.is_some());
        }
    }

    fn peek_starts_with(&self, s: &str) -> bool {
        self.chars.as_str().starts_with(s)
    }

    fn peek(&self, skip: usize) -> Option<char> {
        self.chars.clone().skip(skip).next()
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

pub fn lex(ctx: &EvalContext, filename: &str, source: &str) -> Vec<Token> {
    Lexer::new(ctx, filename, source).collect()
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
            .map(|t| (t.val, t.span.to_string()))
            .collect()
    }

    #[test]
    fn test_ints() {
        assert_lex!("0" => ["1:1-1:2" => Int(0)]);
        assert_lex!("42" => ["1:1-1:3" => Int(42)]);
        assert_lex!("1 2" => ["1:1-1:2" => Int(1), "1:3-1:4" => Int(2)]);
    }

    // TODO(tsion): Figure out how to handle checking Symbols and then add identifier tests.

    #[test]
    fn test_whitespace_and_comments() {
        assert_lex!("" => []);
        assert_lex!("/* just a comment */" => []);
        assert_lex!("\n\n\t\r\n # comments and\n /* whitespace\n don't /* matter */\n\n\n" => []);
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
