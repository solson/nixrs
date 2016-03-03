use std::collections::HashMap;

pub struct SymbolTable {
    str_to_sym: HashMap<Box<str>, Symbol>,
    sym_to_str: Vec<*const str>,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Symbol {
    index: u32,
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            str_to_sym: HashMap::new(),
            sym_to_str: Vec::new(),
        }
    }

    pub fn create<S: Into<String>>(&mut self, string: S) -> Symbol {
        let string = string.into().into_boxed_str();

        if let Some(&sym) = self.str_to_sym.get(&string) {
            return sym;
        }

        let new_sym = Symbol { index: self.sym_to_str.len() as u32 };
        self.sym_to_str.push(&string[..]);
        self.str_to_sym.insert(string, new_sym);
        new_sym
    }

    pub fn get_string(&self, sym: Symbol) -> &str {
        // SAFE: We ensure all *const str in the Vec point into strings kept alive by the HashMap,
        // and we never remove from the HashMap or mutate its key strings.
        unsafe {
            &*self.sym_to_str[sym.index as usize]
        }
    }
}
