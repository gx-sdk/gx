// gx language implementation
// Copyright (C) 2015 Alex Iadicicco <http://ajitek.net>
//
// For licensing information, refer to the COPYING file
// in the project root

//! Semantic information for units

use std::collections::HashMap;

use dump::*;
use frontend::tree;
use msg;
use semantic::*;
use semantic::types::TypeRef;

pub struct Unit<'a> {
    pub name:      String,
    pub units:     HashMap<String, Unit<'a>>,
    pub types:     HashMap<String, TypeRef<'a>>,
    pub ns:        symbol::SymbolTable<'a>
}

impl<'a> Unit<'a> {
    fn empty(name: String) -> Unit<'a> {
        Unit {
            name:   name,
            units:  HashMap::new(),
            types:  HashMap::new(),
            ns:     symbol::SymbolTable::empty(),
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
        let mut u = Unit::empty("<input>".to_owned());
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

impl<'a> Dumpable for Unit<'a> {
    fn dump(&self, d: &mut DumpContext) {
        d.push_str(&format!("unit {}", self.name)[..]);
        for v in self.units.values() {
            v.dump(d);
        }
        for (k, v) in self.types.iter() {
            d.put_ln(format!("type {} = {:?}", k, v));
        }
        d.pop();
    }
}
