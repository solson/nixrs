use std::iter::Peekable;

use lex::{Lexer, StringStyle, TokenKind};
use symbol::Symbol;

#[derive(Debug)]
pub enum Expr {
    Identifier(Symbol),
    IntLiteral(usize), // TODO(solson): What type should we use?
    FloatLiteral(f64), // TODO(solson): What type should we use?
    String(Symbol),
}

#[derive(Debug)]
pub enum ParseError {
    UnexpectedEnd,
    InvalidIntLiteral,
    InvalidFloatLiteral,
}

pub type ParseResult<T> = Result<T, ParseError>;

pub struct Parser<'ctx, 'src> {
    lexer: Peekable<Lexer<'ctx, 'src>>,
}

impl<'ctx, 'src> Parser<'ctx, 'src> {
    pub fn new(lexer: Lexer<'ctx, 'src>) -> Self {
        Parser {
            lexer: lexer.peekable(),
        }
    }

    pub fn parse_expr(&mut self) -> ParseResult<Expr> {
        let token = self.lexer.next().ok_or(ParseError::UnexpectedEnd)?;
        let expr = match token.kind {
            TokenKind::Identifier => Expr::Identifier(Symbol::new(token.source)),

            TokenKind::Integer => {
                let n = token.source.parse::<usize>().or(Err(ParseError::InvalidIntLiteral))?;
                Expr::IntLiteral(n)
            }

            TokenKind::Float => {
                let n = token.source.parse::<f64>().or(Err(ParseError::InvalidFloatLiteral))?;
                Expr::FloatLiteral(n)
            }

            TokenKind::Uri => Expr::String(Symbol::new(token.source)),

            // TODO(solson): Add context for inner error *here*, using failure crate?
            TokenKind::Quote(string_style) => self.parse_string(string_style)?,

            _ => unimplemented!(),
        };
        Ok(expr)
    }

    fn parse_string(&mut self, string_style: StringStyle) -> ParseResult<Expr> {
        let mut str_contents = None;

        loop {
            {
                // TODO(solson): More specific error - unexpected end *in string*.
                let token = self.lexer.peek().ok_or(ParseError::UnexpectedEnd)?;
                match token.kind {
                    TokenKind::StrPart(style) if style == string_style => {
                        if str_contents.is_some() {
                            unimplemented!()
                        }

                        // TODO(solson): Unescape the string source.
                        str_contents = Some(token.source);
                    }

                    TokenKind::Quote(style) if style == string_style => {
                        return Ok(Expr::String(Symbol::new(str_contents.unwrap_or(""))));
                    }

                    _ => unimplemented!(),
                }
            }
            self.lexer.next();
        }
    }
}
