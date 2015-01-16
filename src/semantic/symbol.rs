// gx language implementation
// Copyright (C) 2014-present Alex Iadicicco <http://ajitek.net>
//
// For licensing information, refer to the COPYING file
// in the project root

//! Semantic structures for managing information about symbols in the
//! program. 'Symbol' is a catch-all term for named, identifiable pieces
//! of data. Symbols may or may not have a location, which may or may
//! not be static. They may or may not have values associated with them,
//! and values may or may not be writable.
//!
//! The code here aims to make working with symbols and managing the
//! global symbol graph significantly simpler. The main symbol type is
//! `Symbol`. Every symbol is owned by a `SymbolTable`. Symbol tables
//! are nested in a tree structure, corresponding to the pattern in which
//! lookups are to be perfomed.

use std::collections::HashMap;
use semantic::program::*;

/// The main symbol type.
pub struct Symbol<'a> {
    pub name:          String,
    pub line:          Option<usize>,
    pub typ:           SymbolType<'a>,
}

/// Enumeration of the different types a symbol can have.
pub enum SymbolType<'a> {
    Storage            (&'a Storage<'a>),
    Function           (&'a Function<'a>),
    FunctionArg        (&'a FunctionArg<'a>),
    // TODO: the rest
}

impl<'a> Symbol<'a> {
    /// Create a new symbol from the given storage declaration
    pub fn for_storage(stor: &'a Storage<'a>) -> Self {
        Symbol {
            name:  stor.name.clone(),
            line:  Some(stor.line),
            typ:   SymbolType::Storage(stor),
        }
    }

    /// Create a new symbol from the given function declaration
    pub fn for_function(func: &'a Function<'a>) -> Self {
        Symbol {
            name:  func.name.clone(),
            line:  Some(func.line),
            typ:   SymbolType::Function(func),
        }
    }

    /// Create a new symbol from the given function argument
    pub fn for_function_arg(arg: &'a FunctionArg<'a>) -> Self {
        Symbol {
            name:  arg.name.clone(),
            line:  None,
            typ:   SymbolType::FunctionArg(arg),
        }
    }
}

/// Symbol table. These can be arranged into a tree-like structure where
/// lookups proceed upward in the tree.
pub struct SymbolTable<'a> {
    pub tab:           HashMap<&'a str, Symbol<'a>>,
    pub up:            Option<&'a SymbolTable<'a>>,
}

impl<'a> SymbolTable<'a> {
    /// Create a new parent-less symbol table
    pub fn new() -> Self {
        SymbolTable {
            tab:   HashMap::new(),
            up:    None,
        }
    }

    /// Create a new symbol table with the given table as a fallback
    pub fn new_child(up: &'a SymbolTable<'a>) -> Self {
        SymbolTable {
            tab:   HashMap::new(),
            up:    Some(up),
        }
    }

    /// Traverses the tree upward looking for a symbol with the given name
    pub fn get(&'a self, k: &str) -> Option<&'a Symbol<'a>> {
        match self.tab.get(k) {
            Some(sym) => Some(sym),
            None => match self.up {
                Some(up) => up.get(k),
                None => None,
            },
        }
    }

    /// Performs lookup only in this table
    pub fn get_here(&'a self, k: &str) -> Option<&'a Symbol<'a>> {
        self.tab.get(k)
    }
}
