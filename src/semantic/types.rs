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
use expr::Expr;
use frontend::tree;

/// A reference to a type node. References are either a `semantic::Path`, or
/// an actual Rust reference to a `Type`. This corresponds to an edge in the
/// type graph.
pub struct TypeRef<'a> {
    cell: Ref<Rc<Type<'a>>>
}

impl<'a> TypeRef<'a> {
    /// Constructs a new `TypeRef` object from a vanilla Rust reference.
    pub fn new(t: Type<'a>) -> TypeRef<'a> {
        TypeRef { cell: RefCell::new(RefBody::Res(Rc::new(t))) }
    }

    /// Constructs a new `TypeRef` object from an unresolved path.
    pub fn unresolved(p: Path) -> TypeRef<'a> {
        TypeRef { cell: RefCell::new(RefBody::Named(p)) }
    }

    /// Converts the parse tree type specifier to a semantic tree type
    /// reference.
    pub fn from_tree(t: &tree::TypeSpec) -> SemResult<TypeRef<'a>> {
        match *t {
            tree::TypeSpec::Alias(ref nm) => {
                Ok(if nm.0.len() == 1 {
                    match nm.0[0].as_slice() {
                        "u8"   => TypeRef::new(Type::U8),
                        "u16"  => TypeRef::new(Type::U16),
                        "u32"  => TypeRef::new(Type::U32),
                        "s8"   => TypeRef::new(Type::S8),
                        "s16"  => TypeRef::new(Type::S16),
                        "s32"  => TypeRef::new(Type::S32),
                        _      => TypeRef::unresolved(Path::from_tree(nm)),
                    }
                } else {
                    TypeRef::unresolved(Path::from_tree(nm))
                })
            },
            tree::TypeSpec::Parameterized(ref nm, ref ps) => {
                if nm.0.len() != 1 {
                    return Err(msg::Message {
                        kind:   msg::MessageKind::Error,
                        msg:    format!("invalid parameterized type"),
                        start:  None,
                        end:    None
                    })
                }
                match nm.0[0].as_slice() {
                    "bcd" => {
                        if ps.len() != 1 {
                            return Err(msg::Message {
                                kind:   msg::MessageKind::Error,
                                msg:    format!("bcd<> expects 1 parameter, \
                                                 found {}", ps.len()),
                                start:  None,
                                end:    None
                            })
                        }
                        if let Expr::Primary(tree::Primary::Number(n)) = ps[0] {
                            Ok(TypeRef::new(Type::BCD(n as usize)))
                        } else {
                            Err(msg::Message {
                                kind:   msg::MessageKind::Error,
                                msg:    format!("bcd<> expects numeric \
                                                 type parameter"),
                                start:  None,
                                end:    None
                            })
                        }
                    },
                    "fixed" => {
                        if ps.len() != 2 {
                            return Err(msg::Message {
                                kind:   msg::MessageKind::Error,
                                msg:    format!("fixed<> expects 2 parameters, \
                                                 found {}", ps.len()),
                                start:  None,
                                end:    None
                            })
                        }
                        if let (&Expr::Primary(tree::Primary::Number(n1)),
                                &Expr::Primary(tree::Primary::Number(n2)))
                            = (&ps[0], &ps[1]) {
                            Ok(TypeRef::new(Type::Fixed(
                                n1 as usize,
                                n2 as usize
                            )))
                        } else {
                            Err(msg::Message {
                                kind:   msg::MessageKind::Error,
                                msg:    format!("fixed<> expects numeric \
                                                 type parameter"),
                                start:  None,
                                end:    None
                            })
                        }
                    },
                    x => Err(msg::Message {
                        kind:   msg::MessageKind::Error,
                        msg:    format!("invalid parameterized type `{}'", x),
                        start:  None,
                        end:    None
                    })
                }
            },
            tree::TypeSpec::Pointer(ref to) =>
                Ok(TypeRef::new(Type::Pointer(try!(TypeRef::from_tree(to))))),
            tree::TypeSpec::Array(n, ref of) =>
                Ok(TypeRef::new(Type::Array(n, try!(TypeRef::from_tree(of))))),
            _ => Ok(TypeRef::unresolved(Path { components: Vec::new() })),
        }
    }
}

impl<'a> fmt::Debug for TypeRef<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self.cell.borrow() {
            RefBody::Named(ref p) =>
                write!(f, "'{:?}'", p),
            RefBody::Res(ref p) =>
                write!(f, "{:?}", p)
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
    Array      (isize, TypeRef<'a>),
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
