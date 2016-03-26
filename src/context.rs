use std::cell::RefCell;
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
}
