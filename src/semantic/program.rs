// gx language implementation
// Copyright (C) 2014-present Alex Iadicicco <http://ajitek.net>
//
// For licensing information, refer to the COPYING file
// in the project root

//! Semantic structures corresponding to top-level program info.

use semantic::types::Type;
use std::collections::HashMap;

/// The top level program structure. Identical to a Unit at the moment, since
/// Units have a 1-1 correspondence with namespaces. A Program is simply the top
/// namespace, so there is no distinction atm. The exception is the 'name'
/// field, which for now contains some meaningless value.
type Program<'a> = Unit<'a>;

/// A unit within the program
pub struct Unit<'a> {
    pub name:          String,
    pub units:         HashMap<&'a str, Unit<'a>>,
    pub types:         HashMap<&'a str, TypeAlias<'a>>,
    pub vars:          HashMap<&'a str, Storage<'a>>,
    pub fns:           HashMap<&'a str, Function<'a>>,
}

/// A type alias record. Note that this struct isn't used anywhere other
/// than as a value in the `Unit` type alias hash map. `Type` is used
/// everywhere a type is needed to both eliminate excess lookups (which
/// introduce failure potential) and to better relate to the semantics
/// of a type system.
pub struct TypeAlias<'a> {
    pub name:          String,
    pub line:          usize,
    pub typ:           &'a Type<'a>,
}

/// Global variable storage
pub struct Storage<'a> {
    pub name:          String,
    pub line:          usize,
    pub typ:           &'a Type<'a>,
}

/// A function
pub struct Function<'a> {
    pub name:          String,
    pub line:          usize,
    pub ret:           Option<&'a Type<'a>>,
    pub args:          Vec<FunctionArg<'a>>,
    // TODO
}

/// An argument to a function
pub struct FunctionArg<'a> {
    pub name:          String,
    pub typ:           &'a Type<'a>
}
