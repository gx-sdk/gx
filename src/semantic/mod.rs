// gx language implementation
// Copyright (C) 2014-present Alex Iadicicco <http://ajitek.net>
//
// For licensing information, refer to the COPYING file
// in the project root

//! The code in this module is responsible for representing and managing the
//! semantic graph of a program. The semantic graph is built from a parse
//! tree, and has all aliases and references resolved. The process of
//! transforming a parse tree in this way is called semantic analysis, and
//! is where a lot of error checking and early optimization is performed,
//! such as detecting undeclared identifiers and mismatched types, and doing
//! basic constant folding.
//!
//! The various submodules correspond to the different portions of the
//! semantic graphs, however there is significant interdependence between
//! them.

use std::cell::RefCell;
use std::fmt;

use msg;
use frontend::tree;

pub type SemResult<T> = Result<T, msg::Message>;

pub struct Path {
    pub components: Vec<String>
}

impl Path {
    pub fn from_tree(t: &tree::Path) -> Path {
        Path { components: t.0.clone() }
    }
}

impl Clone for Path {
    fn clone(&self) -> Path {
        Path { components: self.components.clone() }
    }
}

impl fmt::Debug for Path {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.components.len() == 0 {
            write!(f, "(empty)")
        } else {
            try!(write!(f, "{}", self.components[0]));
            for x in self.components[1..].iter() {
                try!(write!(f, "::{}", x))
            }
            Ok(())
        }
    }
}

pub enum RefBody<T> {
    Named  (Path),
    Res    (T),
}

pub type Ref<T> = RefCell<RefBody<T>>;

pub mod types;
pub mod unit;
