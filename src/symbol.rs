use std::collections::HashMap;
use std::fmt;
use std::sync::Mutex;

#[derive(Copy, Clone, Eq, PartialEq)]
pub struct Symbol {
    index: u32,
}

struct SymbolTable {
    str_to_sym: HashMap<Box<str>, Symbol>,
    sym_to_str: Vec<*const str>,
}

// SAFE: It's not auto-implemented because of the raw `*const str` pointers, but our use of those
// can't cause any issues.
unsafe impl Send for SymbolTable {}

lazy_static! {
    static ref SYMBOL_TABLE: Mutex<SymbolTable> = Mutex::new(SymbolTable {
        str_to_sym: HashMap::new(),
        sym_to_str: Vec::new(),
    });
}

impl Symbol {
    pub fn new<S: AsRef<str> + Into<String>>(string: S) -> Symbol {
        let mut table = SYMBOL_TABLE.lock().unwrap();

        if let Some(&sym) = table.str_to_sym.get(string.as_ref()) {
            return sym;
        }

        let str_box = string.into().into_boxed_str();
        let new_sym = Symbol { index: table.sym_to_str.len() as u32 };
        table.sym_to_str.push(&str_box[..]);
        table.str_to_sym.insert(str_box, new_sym);
        new_sym
    }

    pub fn as_str(self) -> &'static str {
        let table = SYMBOL_TABLE.lock().unwrap();

        // SAFE: We ensure all *const str in the Vec point into strings kept alive by the HashMap
        // and we never remove from the HashMap. The HashMap itself lives in static memory and the
        // strings' heap allocations never move so the lifetime of the contained strings may also
        // be 'static.
        unsafe {
            &*table.sym_to_str[self.index as usize]
        }
    }
}

impl fmt::Debug for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.as_str(), self.index)
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}
