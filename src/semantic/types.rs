// gx language implementation
// Copyright (C) 2015 Alex Iadicicco <http://ajitek.net>
//
// For licensing information, refer to the COPYING file
// in the project root

//! Semantic information concerning types.

use std::rc::Rc;
use std::cell::RefCell;
use std::fmt;

use msg;
use semantic::*;

/// A reference to a type node. References are either a `semantic::Path`, or
/// an actual Rust reference to a `Type`. This corresponds to an edge in the
/// type graph.
pub struct TypeRef<'a> {
    cell: Rc<Ref<&'a Type<'a>>>
}

impl<'a> TypeRef<'a> {
    /// Constructs a new `TypeRef` object from a vanilla Rust reference.
    pub fn new(t: &'a Type<'a>) -> TypeRef<'a> {
        TypeRef { cell: Rc::new(RefCell::new(RefBody::Res(t))) }
    }

    /// Attempts to retrieve a vanilla Rust reference. In the event of a
    /// failure, a `msg::Message` is constructed with information about the
    /// error.
    pub fn unwrap(&self) -> Result<&'a Type<'a>, msg::Message> {
        match *self.cell.borrow() {
            RefBody::Named(ref p) => Err(msg::Message {
                    kind:   msg::MessageKind::Internal,
                    msg:    format!("{:?} unresolved", p),
                    start:  None,
                    end:    None,
                }),
            RefBody::Res(p) => Ok(p),
        }
    }
}

impl<'a> fmt::Debug for TypeRef<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self.cell.borrow() {
            RefBody::Named(ref p) =>
                write!(f, "ref={:?}", p),
            RefBody::Res(ref p) =>
                write!(f, "t={:?}", p)
        }
    }
}

/// A struct representing a node in the type graph.
///
/// # Internal types
/// `U8`, `U16`, `U32`, `S8`, `S16`, and `S32` correspond to the similarly named
/// built-in types for integer values. `BCD` and `Fixed` correspond to the `bcd`
/// and `fixed` language features. The `Bitvec` variant corresponds to the
/// `bitvec` language feature. These types are the leaves of the type graph.
///
/// # Structured types
/// `Pointer` is a pointer to a type. `Array` is an array of elements of a
/// single type. `Struct` is an ordered collection of name/type pairs. These
/// types contain other types, and so form the internal nodes of the type graph.
pub enum Type<'a> {
    U8, U16, U32,
    S8, S16, S32,

    BCD        (usize),
    Fixed      (usize, usize),
    Bitvec     (usize, Vec<BitvecMember>),

    Pointer    (TypeRef<'a>),
    Array      (usize, TypeRef<'a>),
    Struct     (Vec<StructMember<'a>>),
}

pub struct StructMember<'a> {
    pub name:  String,
    pub typ:   TypeRef<'a>,
}

pub enum BitvecMember {
    Literal    (usize, usize),
    Variable   (usize, String),
}

impl<'a> fmt::Debug for Type<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Type::U8   => write!(f, "u8"),
            Type::U16  => write!(f, "u16"),
            Type::U32  => write!(f, "u32"),
            Type::S8   => write!(f, "s8"),
            Type::S16  => write!(f, "s16"),
            Type::S32  => write!(f, "s32"),

            Type::BCD(n) => write!(f, "bcd<{}>", n),
            Type::Fixed(n, m) => write!(f, "fixed<{},{}>", n, m),
            Type::Bitvec(n, ref ms) => {
                try!(write!(f, "bitvec<{}>(", n));
                for m in ms.iter() {
                    match *m {
                        BitvecMember::Literal(n, x) =>
                            try!(write!(f, "{}:{}", n, x)),
                        BitvecMember::Variable(n, ref x) =>
                            try!(write!(f, "{}:{}", n, x)),
                    }
                }
                Ok(())
            }

            Type::Pointer(ref t) => write!(f, "*{:?}", t),
            Type::Array(n, ref t) => write!(f, "[{}]{:?}", n, t),
            Type::Struct(ref ms) => {
                try!(write!(f, "struct {{ "));
                for m in ms.iter() {
                    try!(write!(f, "{}:{:?}", m.name, m.typ));
                }
                try!(write!(f, "}}"));
                Ok(())
            }
        }
    }
}
