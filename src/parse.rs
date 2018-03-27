use itertools::Itertools;
use regex::{Regex, RegexSet};
use std::fmt::{self, Write};
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
pub struct Token<'src> {
    pub kind: TokenKind,
    pub span: Span,
    pub source: &'src str,
}

////////////////////////////////////////////////////////////////////////////////
// Token kinds
////////////////////////////////////////////////////////////////////////////////

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum TokenKind {
    Comment,
    Identifier,
    Integer,
    Float,
    Path,
    Uri,
    StrPart(StringStyle),
    Quote(StringStyle), // " or ''
    DollarBraceL,       // ${
    Mult,               // *
    Minus,              // -
    Plus,               // +
    Divide,             // /
    Less,               // <
    Greater,            // >
    LessEq,             // <=
    GreaterEq,          // >=
    Assign,             // =
    Equals,             // ==
    NotEquals,          // !=
    And,                // &&
    Or,                 // ||
    Implies,            // ->
    Not,                // !
    Update,             // //
    Concat,             // ++
    At,                 // @
    Comma,              // ,
    Dot,                // .
    Ellipsis,           // ...
    Question,           // ?
    Colon,              // :
    Semicolon,          // ;
    ParenL,             // (
    ParenR,             // )
    BracketL,           // [
    BracketR,           // ]
    BraceL,             // {
    BraceR,             // }
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

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum StringStyle {
    /// A `"`-delimited string.
    Normal,

    /// A `''`-delimited string, which ignores indentation and leading and trailing whitespace.
    Indent,
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
    match c {
        ' ' | '\t' | '\r' | '\n' => true,
        _ => false,
    }
}

////////////////////////////////////////////////////////////////////////////////
// Lexer
////////////////////////////////////////////////////////////////////////////////

pub struct Lexer<'ctx, 'src> {
    _ctx: &'ctx EvalContext,
    chars: CharsPos<'src>,
    filename: Symbol,

    /// A record of the levels of nesting the lexer is currently in. The last state is the most
    /// deeply nested. An empty stack implies the lexer is at the normal top-level.
    state_stack: Vec<LexerState>,

    /// The line/column location of the start of the token currently being scanned.
    token_start_pos: Pos,

    /// The position in the source string of the token currently being scanned, as a slice
    /// extending to the end of the source string. Once the end of the token is found, this is
    /// sliced off at that point to give just the slice of the string containing the token.
    token_start_str: &'src str,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum LexerState {
    /// When the lexer is at the top-level, not nested inside strings.
    Normal,

    /// When the lexer is inside a string.
    String(StringStyle),

    /// When the lexer is in a `${}` interpolation inside a string.
    Interpolation,
}

impl StringStyle {
    fn delimiter(self) -> &'static str {
        match self {
            StringStyle::Normal => "\"",
            StringStyle::Indent => "''",
        }
    }
}

impl<'ctx, 'src> Iterator for Lexer<'ctx, 'src> {
    type Item = Token<'src>;

    fn next(&mut self) -> Option<Token<'src>> {
        match self.state() {
            LexerState::Normal | LexerState::Interpolation => self.lex_normal(),
            LexerState::String(_) => self.lex_string_part(),
        }
    }
}

const TOKEN_REGEX_SOURCES: [&str; 7] = [
    // 0 = identifier
    r"\A[a-zA-Z_][a-zA-Z0-9_'-]*",

    // 1 = integer
    r"\A[0-9]+",

    // 2 = float
    r"\A(([1-9][0-9]*\.[0-9]*)|(0?\.[0-9]+))([Ee][+-]?[0-9]+)?",

    // 3 = regular path
    r"\A[a-zA-Z0-9._+-]*(/[a-zA-Z0-9._+-]+)+/?",

    // 4 = home path (starts with `~`)
    r"\A~(/[a-zA-Z0-9._+-]+)+/?",

    // 5 = special path (surrounded by `<>`)
    r"\A<[a-zA-Z0-9._+-]+(/[a-zA-Z0-9._+-]+)*>",

    // 6 = URI
    r"\A[a-zA-Z][a-zA-Z0-9+.-]*:[a-zA-Z0-9%/?:@&=+$,_.!~*'-]+",
];

lazy_static! {
    static ref TOKEN_REGEX_SET: RegexSet = RegexSet::new(&TOKEN_REGEX_SOURCES).unwrap();
    static ref TOKEN_REGEXES: Vec<Regex> = TOKEN_REGEX_SOURCES.iter().map(|src| {
        Regex::new(src).unwrap()
    }).collect();
}

impl<'ctx, 'src> Lexer<'ctx, 'src> {
    pub fn new(ctx: &'ctx EvalContext, filename: &str, source: &'src str) -> Self {
        let chars = CharsPos::new(source.chars());
        Lexer {
            _ctx: ctx,
            token_start_pos: chars.pos,
            token_start_str: source,
            chars,
            filename: Symbol::new(filename),
            state_stack: Vec::new(),
        }
    }

    fn state(&self) -> LexerState {
        *self.state_stack.last().unwrap_or(&LexerState::Normal)
    }

    fn lex_normal(&mut self) -> Option<Token<'src>> {
        debug_assert!(self.state() == LexerState::Normal ||
                      self.state() == LexerState::Interpolation);
        self.start_token();

        macro_rules! simple {
            ($kind:ident, $len:expr) => ({
                self.skip($len);
                Some(self.finish_token(TokenKind::$kind))
            })
        }

        let mut matches: Vec<(usize, &'src str)> = TOKEN_REGEX_SET
            .matches(self.peek_rest())
            .iter()
            .map(|match_index| {
                let token_regex = &TOKEN_REGEXES[match_index];
                let token_str = token_regex.find(self.peek_rest()).unwrap().as_str();
                (match_index, token_str)
            })
            .collect();

        // Use the longest match.
        matches.sort_by_key(|&(_, s)| s.len());
        if let Some(&(match_index, token_str)) = matches.last() {
            let token_kind = match match_index {
                0 => ident_or_keyword_from_str(token_str),
                1 => TokenKind::Integer,
                2 => TokenKind::Float,
                3 | 4 | 5 => TokenKind::Path,
                6 => TokenKind::Uri,
                _ => unreachable!(),
            };

            self.skip(token_str.len());
            return Some(self.finish_token(token_kind));
        }

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
            (c, _) if is_whitespace(c) => {
                self.skip_whitespace();
                self.lex_normal()
            }

            ('#', _) => Some(self.lex_line_comment()),
            ('/', Some('*')) => Some(self.lex_long_comment()),

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
                self.state_stack.push(LexerState::String(StringStyle::Normal));
                self.skip(1);
                Some(self.finish_token(TokenKind::Quote(StringStyle::Normal)))
            }

            // The beginning of an indent string.
            ('\'', Some('\'')) => {
                self.state_stack.push(LexerState::String(StringStyle::Indent));
                self.skip(2);
                Some(self.finish_token(TokenKind::Quote(StringStyle::Indent)))
            }

            // If we're lexing inside of a string interpolation, we need to keep track of our depth
            // of nested curly braces (because we need to end up back in `String` lexing mode after
            // the correct matching '}').
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

            (c, _) => panic!("unhandled char: {}, at: {}", c, self.pos()),
        }
    }

    fn lex_string_part(&mut self) -> Option<Token<'src>> {
        let string_style = match self.state() {
            LexerState::String(style) => style,
            s => panic!("entered lex_string_part in a non-string lexing state: {:?}", s),
        };
        self.start_token();
        let delimiter = string_style.delimiter();

        loop {
            // Check if we've hit the end of string.
            if self.peek_starts_with(delimiter) {
                if string_style == StringStyle::Indent {
                    match (self.peek(2), self.peek(3)) {
                        (Some('$'), _) => { self.skip(3); continue; }
                        (Some('\''), _) => { self.skip(3); continue; }
                        (Some('\\'), Some(_)) => { self.skip(4); continue; }
                        (Some('\\'), None) => {
                            // TODO(solson): Report character escape in string meeting end of file.
                            panic!("character escape hit end of file");
                        }
                        _ => {}
                    }
                }

                // If we lexed some chars before hitting end of string, we'll emit a `StrPart`
                // token before re-entering this function to emit the closing `Quote` token.
                if self.token_start_pos != self.pos() {
                    break;
                }

                self.skip(delimiter.len());
                self.state_stack.pop(); // Pop the string state.
                return Some(self.finish_token(TokenKind::Quote(string_style)));
            }

            let c1 = match self.peek(0) {
                Some(c) => c,
                None => {
                    // TODO(solson): Report string meeting end of file.
                    panic!("unclosed string hit end of file");
                }
            };
            let c2 = self.peek(1);

            match (c1, c2) {
                ('\\', Some(_)) if string_style == StringStyle::Normal => self.skip(2),

                ('\\', None) if string_style == StringStyle::Normal => {
                    // TODO(solson): Report character escape (or string?) meeting end of file.
                    panic!("character escape hit end of file");
                }

                ('\'', Some('\''))
                if string_style == StringStyle::Indent && self.peek(2) == Some('$') => {
                    self.skip(3);
                }

                ('$', Some('{')) => {
                    if self.token_start_pos == self.pos() {
                        self.skip(2); // Skip over the '${'.
                        self.state_stack.push(LexerState::Interpolation);
                        return Some(self.finish_token(TokenKind::DollarBraceL));
                    } else {
                        break;
                    }
                }

                // Replace literal \r and \r\n character sequences in multiline strings with \n.
                ('\r', Some('\n')) => self.skip(2),
                ('\r', _)          => self.skip(1),

                _ => self.skip(1),
            };
        }

        Some(self.finish_token(TokenKind::StrPart(string_style)))
    }

    /// Regexp from the Nix lexer: `[ \t\r\n]+`
    fn skip_whitespace(&mut self) {
        debug_assert!(self.peek(0).map(is_whitespace).unwrap_or(false));
        self.skip_while(is_whitespace);
    }

    /// Regexp from the Nix lexer: `#[^\r\n]*`
    fn lex_line_comment(&mut self) -> Token<'src> {
        debug_assert_eq!(self.peek(0), Some('#'));
        self.start_token();
        self.skip_while(|c| c != '\n' && c != '\r');
        self.finish_token(TokenKind::Comment)
    }

    /// Regexp from the Nix lexer: `\/\*([^*]|\*[^\/])*\*\/`
    fn lex_long_comment(&mut self) -> Token<'src> {
        debug_assert!(self.peek_starts_with("/*"));
        self.start_token();
        self.skip(2);

        while !self.peek_starts_with("*/") {
            if self.peek(0).is_none() {
                // TODO(tsion): Report unterminated comment meeting end of file.
                panic!("unclosed string hit end of file");
            }
            self.skip(1);
        }
        self.skip(2);

        self.finish_token(TokenKind::Comment)
    }

    fn skip_while<F>(&mut self, mut f: F) -> usize where F: FnMut(char) -> bool {
        self.chars.take_while_ref(|&c| f(c)).count()
    }

    fn skip(&mut self, count: usize) {
        for _ in 0..count {
            let _c = self.chars.next();
            debug_assert!(_c.is_some());
        }
    }

    fn peek_rest(&self) -> &'src str {
        self.chars.as_str()
    }

    fn peek_starts_with(&self, s: &str) -> bool {
        self.peek_rest().starts_with(s)
    }

    fn peek(&self, skip: usize) -> Option<char> {
        self.chars.clone().skip(skip).next()
    }

    fn pos(&self) -> Pos {
        self.chars.pos
    }

    /// Record the start position of a token, to be paired with `finish_token`.
    fn start_token(&mut self) {
        self.token_start_pos = self.pos();
        self.token_start_str = self.chars.as_str();
    }

    /// Generate a token starting at the start position from the most recent `start_token` call and
    /// ending at the current position.
    fn finish_token(&self, kind: TokenKind) -> Token<'src> {
        // This slightly convoluted code takes two slices from certain positions in the source to
        // the end of the source and figures out the distance between their starting positions by
        // subtracting their distances to the end of the source.
        let start_len = self.token_start_str.len();
        let end_len = self.chars.as_str().len();
        let len = start_len - end_len;
        let source = &self.token_start_str[..len];

        Token {
            kind,
            span: Span {
                filename: self.filename,
                start: self.token_start_pos,
                end: self.pos(),
            },
            source,
        }
    }

    // This function is used for lexer tests, so if you change the format of it, you must change
    // the format of all the lexer tests (in tests/lexer/*.out).
    pub fn debug_string(self) -> String {
        let mut out = String::new();
        for Token { kind, span, source } in self {
            let Span { start: s, end: e, .. } = span;
            write!(out, "[{}:{}-{}:{}] ({:?}) {:?}\n",
                s.line, s.column, e.line, e.column, kind, source).unwrap();
        }
        out
    }
}

fn ident_or_keyword_from_str(str: &str) -> TokenKind {
    match str {
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
        _         => TokenKind::Identifier,
    }
}
