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

pub mod types;
pub mod program;
pub mod symbol;
