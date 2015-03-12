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

impl<'a> Clone for TypeRef<'a> {
    fn clone(&self) -> TypeRef<'a> {
        match *self.cell.borrow() {
            RefBody::Res(ref rc) =>
                TypeRef { cell: RefCell::new(RefBody::Res(rc.clone())) },
            RefBody::Named(ref p) =>
                TypeRef { cell: RefCell::new(RefBody::Named(p.clone())) },
        }
    }
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

    fn error(msg: String, s: tree::Span) -> msg::MessageList {
        msg::Message {
            kind: msg::MessageKind::Error,
            msg: msg,
            start: Some(msg::Position {
                file:  String::from_str("<input>"), // TODO
                line:  s.start.line,
                col:   s.start.col
            }),
            end: Some(msg::Position {
                file:  String::from_str("<input>"), // TODO
                line:  s.end.line,
                col:   s.end.col
            }),
        }.to_list()
    }

    /// Converts the parse tree type specifier to a semantic tree type
    /// reference.
    pub fn from_tree(t: &tree::TypeSpec) -> SemResult<TypeRef<'a>> {
        match t.body {
            tree::TypeBody::Alias(ref nm) => {
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

            tree::TypeBody::Parameterized(ref nm, ref ps) => {
                if nm.0.len() != 1 {
                    return Err(TypeRef::error(
                        format!("invalid parameterized type"),
                        t.span
                    ));
                }

                match nm.0[0].as_slice() {
                    "bcd" => {
                        if ps.len() != 1 {
                            return Err(TypeRef::error(
                                format!("bcd<> expects 1 parameter, \
                                         found {}", ps.len()),
                                t.span
                            ));
                        }
                        if let Expr::Primary(tree::Primary::Number(n)) = ps[0] {
                            Ok(TypeRef::new(Type::BCD(n as usize)))
                        } else {
                            Err(TypeRef::error(
                                format!("bcd<> expects numeric \
                                         type parameter"),
                                t.span
                            ))
                        }
                    },

                    "fixed" => {
                        if ps.len() != 2 {
                            return Err(TypeRef::error(
                                format!("fixed<> expects 2 parameters, \
                                         found {}", ps.len()),
                                t.span
                            ));
                        }
                        if let (&Expr::Primary(tree::Primary::Number(n1)),
                                &Expr::Primary(tree::Primary::Number(n2)))
                            = (&ps[0], &ps[1]) {
                            Ok(TypeRef::new(Type::Fixed(
                                n1 as usize,
                                n2 as usize
                            )))
                        } else {
                            Err(TypeRef::error(
                                format!("fixed<> expects numeric \
                                         type parameter"),
                                t.span
                            ))
                        }
                    },
                    x => Err(TypeRef::error(
                        format!("invalid parameterized type `{}'", x),
                        t.span
                    ))
                }
            },

            tree::TypeBody::Pointer(ref to) =>
                Ok(TypeRef::new(Type::Pointer(try!(TypeRef::from_tree(to))))),

            tree::TypeBody::Array(n, ref of) =>
                Ok(TypeRef::new(Type::Array(n, try!(TypeRef::from_tree(of))))),

            tree::TypeBody::Struct(ref ms) => {
                let mut v = Vec::new();

                for m in ms.iter() {
                    if let Some(_) = m.init {
                        return Err(TypeRef::error(
                            format!("initializers are not valid in struct defs"),
                            m.typ.span
                        ));
                    }

                    let tr = try!(TypeRef::from_tree(&m.typ));

                    for id in m.ids.iter() {
                        v.push(StructMember {
                            name:  id.clone(),
                            typ:   tr.clone()
                        });
                    }
                }

                Ok(TypeRef::new(Type::Struct(v)))
            },

            tree::TypeBody::Bitvec(num, ref ms) => {
                let mut v = Vec::new();
                let mut width: usize = 0;

                for m in ms.iter() {
                    v.push(match *m {
                        tree::BitvecMember::Literal(n, w) => {
                            if w <= 0 {
                                return Err(TypeRef::error(
                                    format!("bitvec members must have \
                                             positive width"),
                                    t.span
                                ))
                            }
                            // TODO: check literal has correct width
                            width += w as usize;
                            BitvecMember::Literal(w as usize, n)
                        },
                        tree::BitvecMember::Variable(ref id, w) => {
                            if w <= 0 {
                                return Err(TypeRef::error(
                                    format!("bitvec members must have \
                                             positive width"),
                                    t.span
                                ))
                            }
                            width += w as usize;
                            BitvecMember::Variable(w as usize, id.clone())
                        },
                    })
                }

                width = match num {
                    None => {
                        width
                    },
                    Some(n) => if n <= 0 || (width as isize) > n {
                        return Err(TypeRef::error(
                            format!("bitvec specified width too small \
                                     for sum of members"),
                            t.span
                        ))
                    } else {
                        n as usize
                    },
                };

                Ok(TypeRef::new(Type::Bitvec(width, v)))
            },
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
                            try!(write!(f, "{}:{},", n, x)),
                        BitvecMember::Variable(n, ref x) =>
                            try!(write!(f, "{}:{},", n, x)),
                    }
                }
                try!(write!(f, ")"));
                Ok(())
            }

            Type::Pointer(ref t) => write!(f, "*{:?}", t),
            Type::Array(n, ref t) => write!(f, "[{}]{:?}", n, t),
            Type::Struct(ref ms) => {
                try!(write!(f, "struct {{ "));
                for m in ms.iter() {
                    try!(write!(f, "{}:{:?} ", m.name, m.typ));
                }
                try!(write!(f, "}}"));
                Ok(())
            }
        }
    }
}
