// gx language implementation
// Copyright (C) 2014-present Alex Iadicicco <http://ajitek.net>
//
// For licensing information, refer to the COPYING file
// in the project root

//! This module contains everything relevant to the compiler frontend. By
//! itself, it can be used for lexing and parsing `gx` code, such as for
//! syntax highlighting and other such analysis. Attempts were made at
//! sectioning off code for such purposes, but the most important purpose
//! is ransforming text input into the semantic structures over in the
//! `gx::semantic` module.
//!
//! The `token` and `tree` submodules are collections of structs and enums
//! representing lexer tokens and parse tree structures. The `lexer` and
//! `parser` submodules are the actual implementations of the respective
//! components.

pub mod token;
pub mod tree;

pub mod lexer;
pub mod parser;
