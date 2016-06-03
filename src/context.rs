use parse::{Token, TokenKind};

pub struct EvalContext {}

impl EvalContext {
    pub fn new() -> Self {
        EvalContext {}
    }

    ////////////////////////////////////////////////////////////////////////////////
    // Debug helper functions
    ////////////////////////////////////////////////////////////////////////////////

    pub fn dump_tokens<I>(&self, tokens: I) where I: IntoIterator<Item = Token> {
        let mut first = true;
        for token in tokens {
            if first { first = false; } else { print!(" "); }
            print!("{:?}@{}", token.val, token.span);
        }
        println!("");
    }

    pub fn dump_token_kinds<I>(&self, tokens: I) where I: IntoIterator<Item = TokenKind> {
        let mut first = true;
        for token in tokens {
            if first { first = false; } else { print!(" "); }
            print!("{:?}", token);
        }
        println!("");
    }
}
