use itertools::Itertools;
use std::fmt;
use std::str::Chars;

use context::EvalContext;
use symbol::Symbol;

////////////////////////////////////////////////////////////////////////////////
// Token positions and spans
////////////////////////////////////////////////////////////////////////////////

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Pos {
    pub column: usize,
    pub line: usize,
}

impl fmt::Display for Pos {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Span {
    pub filename: Symbol,
    pub start: Pos,
    pub end: Pos,
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
    Quote,        // "
    IndentQuote,  // ''
    DollarBraceL, // ${

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

    // Keywords
    KeywordIf,
    KeywordThen,
    KeywordElse,
    KeywordAssert,
    KeywordWith,
    KeywordLet,
    KeywordIn,
    KeywordRec,
    KeywordInherit,
    KeywordOr,
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

    /// A record of the levels of nesting the lexer is currently in. The last state is the most
    /// deeply nested. An empty stack implies the lexer is at the normal top-level.
    state_stack: Vec<LexerState>,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum LexerState {
    /// When the lexer is at the top-level, not nested inside strings.
    Normal,

    /// When the lexer is inside a string.
    String,

    /// When the lexer is in a `${}` interpolation inside a string.
    Interpolation,
}

impl<'ctx, 'src> Iterator for Lexer<'ctx, 'src> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        match self.state() {
            LexerState::Normal | LexerState::Interpolation => self.lex_normal(),
            LexerState::String => self.lex_string_part(),
        }
    }
}

impl<'ctx, 'src> Lexer<'ctx, 'src> {
    pub fn new(ctx: &'ctx EvalContext, filename: &str, source: &'src str) -> Self {
        Lexer {
            ctx: ctx,
            chars: CharsPos::new(source.chars()),
            filename: Symbol::new(filename),
            state_stack: Vec::new(),
        }
    }

    fn state(&self) -> LexerState {
        *self.state_stack.last().unwrap_or(&LexerState::Normal)
    }

    fn lex_normal(&mut self) -> Option<Token> {
        debug_assert!(self.state() == LexerState::Normal ||
                      self.state() == LexerState::Interpolation);
        let start = self.pos();

        macro_rules! simple { ($kind:ident, $len:expr) => ({
            self.skip($len);
            Some(self.spanned(start, self.pos(), TokenKind::$kind))
        })}

        let c1 = match self.peek(0) {
            Some(c) => c,
            None => {
                if self.state() == LexerState::Interpolation {
                    // TODO(solson): Report unterminated string hitting end of file.
                    panic!("unterminated string hit end of file (inside Interpolation)");
                } else {
                    return None;
                }
            }
        };
        let c2 = self.peek(1);

        match (c1, c2) {
            // Whitespace and comments.

            (c, _) if is_whitespace(c) => {
                self.skip_whitespace();
                self.lex_normal()
            }

            ('#', _) => {
                self.skip_line_comment();
                self.lex_normal()
            }

            ('/', Some('*')) => {
                self.skip_long_comment();
                self.lex_normal()
            }

            // Various fixed-length symbol tokens.

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

            ('.', Some('.')) if self.peek(2) == Some('.') => simple!(Ellipsis, 3),
            ('.', _) => simple!(Dot, 1),

            // The beginning of a string.

            ('"', _) => {
                self.state_stack.push(LexerState::String);
                simple!(Quote, 1)
            }

            // If we're lexing inside of a string interpolation, we need to keep track of our depth
            // of nested curly braces (because when we need to end up back in `String` lexing mode
            // after the correct matching '}').
            //
            // We simply push and pop on the `state_stack` for this. Since deep nesting is rare, it
            // shouldn't be a performance problem.

            ('$', Some('{')) => {
                if self.state() == LexerState::Interpolation {
                    self.state_stack.push(LexerState::Interpolation);
                }
                simple!(DollarBraceL, 2)
            }

            ('{', _) => {
                if self.state() == LexerState::Interpolation {
                    self.state_stack.push(LexerState::Interpolation);
                }
                simple!(BraceL, 1)
            }

            ('}', _) => {
                if self.state() == LexerState::Interpolation {
                    self.state_stack.pop();
                }
                simple!(BraceR, 1)
            }

            // Identifier and integer tokens.

            (c, _) if is_identifier_start(c) => Some(self.lex_ident_or_keyword()),
            (c, _) if c.is_digit(10) => Some(self.lex_int()),
            (c, _) => panic!("unhandled char: {}", c),
        }
    }

    fn lex_string_part(&mut self) -> Option<Token> {
        debug_assert_eq!(self.state(), LexerState::String);
        let start = self.pos();

        if let Some('"') = self.peek(0) {
            self.skip(1); // Skip over the quote.
            let _state = self.state_stack.pop(); // Pop the string state.
            debug_assert_eq!(_state, Some(LexerState::String));
            return Some(self.spanned(start, self.pos(), TokenKind::Quote));
        }

        let mut chars = String::new();

        loop {
            let c1 = match self.peek(0) {
                Some(c) => c,
                None => {
                    // TODO(solson): Report string meeting end of file.
                    panic!("unclosed string hit end of file");
                }
            };
            let c2 = self.peek(1);

            match (c1, c2) {
                ('\\', Some(c)) => {
                    self.skip(2);
                    chars.push(match c {
                        'n' => '\n',
                        'r' => '\r',
                        't' => '\t',
                        _ => c,
                    });
                }
                ('\\', None) => {
                    // TODO(solson): Report character escape (or string?) meeting end of file.
                    panic!("character escape hit end of file");
                }

                ('$', Some('{')) => {
                    if chars.is_empty() {
                        self.skip(2); // Skip over the '${'.
                        self.state_stack.push(LexerState::Interpolation);
                        return Some(self.spanned(start, self.pos(), TokenKind::DollarBraceL));
                    } else {
                        break;
                    }
                }

                ('"', _) => {
                    if chars.is_empty() {
                        self.skip(1); // Skip over the quote.
                        let _state = self.state_stack.pop(); // Pop the string state.
                        debug_assert_eq!(_state, Some(LexerState::String));
                        return Some(self.spanned(start, self.pos(), TokenKind::Quote));
                    } else {
                        break;
                    }
                }

                // Replace literal \r and \r\n character sequences in multiline strings with \n.
                ('\r', Some('\n')) => { self.skip(2); chars.push('\n'); }
                ('\r', _)          => { self.skip(1); chars.push('\n'); }

                _ => { self.skip(1); chars.push(c1); }
            };
        }

        Some(self.spanned(start, self.pos(), TokenKind::StrPart(chars)))
    }

    fn lex_ident_or_keyword(&mut self) -> Token {
        let start = self.pos();
        let chars = self.chars.as_str();
        let len = self.chars.take_while_ref(|&c| is_identifier_continue(c)).count();
        let identifier = &chars[..len];

        let kind = match identifier {
            "if"      => TokenKind::KeywordIf,
            "then"    => TokenKind::KeywordThen,
            "else"    => TokenKind::KeywordElse,
            "assert"  => TokenKind::KeywordAssert,
            "with"    => TokenKind::KeywordWith,
            "let"     => TokenKind::KeywordLet,
            "in"      => TokenKind::KeywordIn,
            "rec"     => TokenKind::KeywordRec,
            "inherit" => TokenKind::KeywordInherit,
            "or"      => TokenKind::KeywordOr,
            _         => TokenKind::Id(Symbol::new(identifier)),
        };

        self.spanned(start, self.pos(), kind)
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
    use symbol::Symbol;

    ////////////////////////////////////////////////////////////////////////////////
    // Helper functions
    ////////////////////////////////////////////////////////////////////////////////

    macro_rules! assert_lex {
        ($src:expr, [ $($span:expr => $token:expr),* $(,)* ]) => ({
            let expected = [ $(($token, String::from($span))),* ];
            assert_eq!(lex($src), expected);
        })
    }

    fn lex(src: &str) -> Vec<(TokenKind, String)> {
        Lexer::new(&EvalContext::new(), "<test>", src)
            .map(|t| (t.val, t.span.to_string()))
            .collect()
    }

    fn id(name: &str) -> TokenKind {
        Id(Symbol::new(name))
    }

    fn str_part(s: &str) -> TokenKind {
        StrPart(String::from(s))
    }

    ////////////////////////////////////////////////////////////////////////////////
    // Lexer tests
    ////////////////////////////////////////////////////////////////////////////////

    #[test]
    fn lex_ints() {
        assert_lex!("0",   ["1:1-1:2" => Int(0)]);
        assert_lex!("42",  ["1:1-1:3" => Int(42)]);
        assert_lex!("1 2", ["1:1-1:2" => Int(1), "1:3-1:4" => Int(2)]);
    }

    #[test]
    fn lex_idents() {
        assert_lex!("a", ["1:1-1:2" => id("a")]);
        assert_lex!("b", ["1:1-1:2" => id("b")]);
        assert_lex!("a a b", [
            "1:1-1:2" => id("a"),
            "1:3-1:4" => id("a"),
            "1:5-1:6" => id("b"),
        ]);
        assert_lex!("foobar", ["1:1-1:7" => id("foobar")]);
    }

    #[test]
    fn lex_ints_vs_idents() {
        assert_lex!("a1", ["1:1-1:3" => id("a1")]);
        assert_lex!("1a", ["1:1-1:2" => Int(1), "1:2-1:3" => id("a")]);
    }

    #[test]
    fn lex_double_quote() {
        assert_lex!(r#""""#, [
            "1:1-1:2" => Quote,
            "1:2-1:3" => Quote,
        ]);
        assert_lex!(r#"" ""#, [
            "1:1-1:2" => Quote,
            "1:2-1:3" => str_part(" "),
            "1:3-1:4" => Quote,
        ]);
        assert_lex!(r#""\a\b\c\\\"\n\r\t\${}""#, [
            "1:1-1:2"   => Quote,
            "1:2-1:22"  => str_part("abc\\\"\n\r\t${}"),
            "1:22-1:23" => Quote,
        ]);
        assert_lex!(r#""foobar""#, [
            "1:1-1:2" => Quote,
            "1:2-1:8" => str_part("foobar"),
            "1:8-1:9" => Quote,
        ]);

        // Literal newlines and carriage returns to test normalization to newlines.
        // FIXME(solson): Should the stray \r get counted as a line break for spans? That would
        // make this end up on line 5 rather than line 4.
        assert_lex!("\"foo\n \r \n \r\n bar\"", [
            "1:1-1:2" => Quote,
            "1:2-4:5" => str_part("foo\n \n \n \n bar"),
            "4:5-4:6" => Quote,
        ]);
    }

    #[test]
    fn lex_double_quote_interpolation() {
        assert_lex!(r#""${}""#, [
            "1:1-1:2" => Quote,
            "1:2-1:4" => DollarBraceL,
            "1:4-1:5" => BraceR,
            "1:5-1:6" => Quote,
        ]);
        assert_lex!(r#""foo${}""#, [
            "1:1-1:2" => Quote,
            "1:2-1:5" => str_part("foo"),
            "1:5-1:7" => DollarBraceL,
            "1:7-1:8" => BraceR,
            "1:8-1:9" => Quote,
        ]);
        assert_lex!(r#""${}bar""#, [
            "1:1-1:2" => Quote,
            "1:2-1:4" => DollarBraceL,
            "1:4-1:5" => BraceR,
            "1:5-1:8" => str_part("bar"),
            "1:8-1:9" => Quote,
        ]);
        assert_lex!(r#""foo${}bar""#, [
            "1:1-1:2" => Quote,
            "1:2-1:5" => str_part("foo"),
            "1:5-1:7" => DollarBraceL,
            "1:7-1:8" => BraceR,
            "1:8-1:11" => str_part("bar"),
            "1:11-1:12" => Quote,
        ]);
    }

    #[test]
    fn lex_whitespace_and_comments() {
        assert_lex!("", []);
        assert_lex!("/* just a comment */", []);
        assert_lex!("\n\n\t\r\n # comments and\n /* whitespace\n don't /* matter */\n\n\n", []);
        assert_lex!("\n  1\n13 \n 42\n", [
            "2:3-2:4" => Int(1),
            "3:1-3:3" => Int(13),
            "4:2-4:4" => Int(42),
        ]);
        assert_lex!("# line comment\n  1 ### lex this please\n13 \n 42\n# end of file", [
            "2:3-2:4" => Int(1),
            "3:1-3:3" => Int(13),
            "4:2-4:4" => Int(42),
        ]);
    }
}
