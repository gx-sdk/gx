// gx language implementation
// Copyright (C) 2015 Alex Iadicicco <http://ajitek.net>
//
// For licensing information, refer to the COPYING file
// in the project root

//! A symbol is a named object, and a symbol table is a namespace for these
//! objects. Since `gx` is lexically scoped, symbol tables correspond exactly to
//! some lexical element, often units. The `use` declaration can be used to
//! import symbols from a different namespace into the current one.
//!
//! Most `Path` resolution will only use the tree of `SymbolTable`s

use std::collections::HashMap;

use semantic::Path;
use semantic::unit::Unit;
use semantic::types::TypeRef;

pub struct Symbol<'a> {
    pub name:     String,
    pub is_pub:   bool,
    pub body:     SymbolBody<'a>
}

pub enum SymbolBody<'a> {
    Unit    (&'a Unit<'a>),
    Type    (TypeRef<'a>),
}

pub struct SymbolTable<'a> {
    pub up:       Option<&'a SymbolTable<'a>>,
    pub tab:      HashMap<String, Symbol<'a>>,
}

impl<'a> SymbolTable<'a> {
    pub fn empty() -> SymbolTable<'a> {
        SymbolTable {
            up:    None,
            tab:   HashMap::new()
        }
    }

    pub fn lookup_at(&'a self, p: &'a Path, idx: usize) -> Option<&'a Symbol<'a>> {
        if idx >= p.components.len() {
            return None;
        }

        match self.tab.get(&p.components[idx]) {
            None => None,

            Some(s) => {
                if idx < p.components.len() - 1 {
                    match s.body {
                        SymbolBody::Unit(u) => u.ns.lookup_at(p, idx + 1),
                        _ => None
                    }
                } else {
                    Some(s)
                }
            },
        }
    }

    /// Performs lookup of the named symbol
    pub fn lookup(&'a self, p: &'a Path) -> Option<&'a Symbol<'a>> {
        self.lookup_at(p, 0)
    }
}
