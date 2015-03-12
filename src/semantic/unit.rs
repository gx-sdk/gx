// gx language implementation
// Copyright (C) 2015 Alex Iadicicco <http://ajitek.net>
//
// For licensing information, refer to the COPYING file
// in the project root

//! Semantic information for units

use std::collections::HashMap;
use std::fmt;

use msg;
use semantic::*;
use semantic::types::TypeRef;
use frontend::tree;

pub struct Unit<'a> {
    pub name:      String,
    pub units:     HashMap<String, Unit<'a>>,
    pub types:     HashMap<String, TypeRef<'a>>,
}

impl<'a> Unit<'a> {
    fn empty(name: String) -> Unit<'a> {
        Unit {
            name:   name,
            units:  HashMap::new(),
            types:  HashMap::new()
        }
    }

    pub fn add_from_tree(&mut self, t: &tree::Decl) -> SemResult<()> {
        match t.body {
            tree::DeclBody::Unit(ref unit) => {
                self.units.insert(
                    unit.name.clone(),
                    try!(Unit::from_tree_unit(unit))
                );
            },

            tree::DeclBody::Type(ref typ) => {
                self.types.insert(
                    typ.name.clone(),
                    try!(TypeRef::from_tree(&typ.typ))
                );
            },

            _ => { }
        }

        Ok(())
    }

    pub fn from_tree_input(t: &tree::Input) -> SemResult<Unit<'a>> {
        let mut u = Unit::empty(String::from_str("<input>"));
        let mut msgs = msg::MessageList::empty();

        for decl in t.iter() {
            match u.add_from_tree(decl) {
                Ok(_) => { },
                Err(m) => { msgs.add_all(m) }
            }
        }

        if msgs.is_empty() {
            Ok(u)
        } else {
            Err(msgs)
        }
    }

    pub fn from_tree_unit(t: &tree::UnitDecl) -> SemResult<Unit<'a>> {
        let mut u = Unit::empty(t.name.clone());
        let mut msgs = msg::MessageList::empty();

        for decl in t.decls.iter() {
            match u.add_from_tree(decl) {
                Ok(_) => { },
                Err(m) => { msgs.add_all(m) }
            }
        }

        if msgs.is_empty() {
            Ok(u)
        } else {
            Err(msgs)
        }
    }
}
