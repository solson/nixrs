use std::cell::RefCell;

use parse::{Token, TokenKind};
use symbol_table::{Symbol, SymbolTable};

pub struct EvalContext {
    symbol_table: RefCell<SymbolTable>,
}

impl EvalContext {
    pub fn new() -> Self {
        EvalContext {
            symbol_table: RefCell::new(SymbolTable::new()),
        }
    }

    pub fn intern(&self, s: &str) -> Symbol {
        self.symbol_table.borrow_mut().create(s)
    }

    ////////////////////////////////////////////////////////////////////////////////
    // Debug helper functions
    ////////////////////////////////////////////////////////////////////////////////

    fn dump_token_kind(&self, token: TokenKind) {
        if let TokenKind::Id(sym) = token {
            print!("Id({})", self.symbol_table.borrow().get_string(sym));
        } else {
            print!("{:?}", token);
        }
    }

    pub fn dump_tokens<I>(&self, tokens: I) where I: IntoIterator<Item = Token> {
        let mut first = true;
        for token in tokens {
            if first { first = false; } else { print!(" "); }
            self.dump_token_kind(token.val);
            print!("@{}", token.span);
        }
        println!("");
    }

    pub fn dump_token_kinds<I>(&self, tokens: I) where I: IntoIterator<Item = TokenKind> {
        let mut first = true;
        for token in tokens {
            if first { first = false; } else { print!(" "); }
            self.dump_token_kind(token);
        }
        println!("");
    }
}
