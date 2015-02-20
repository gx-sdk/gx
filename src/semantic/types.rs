// gx language implementation
// Copyright (C) 2014-present Alex Iadicicco <http://ajitek.net>
//
// For licensing information, refer to the COPYING file
// in the project root

//! Semantic understanding of `gx` types. The structures here are carefully
//! designed to reflect only the semantic extent of the type system.

use std::fmt;

/// The primary type descriptor. An equivalence relation is defined for this
/// enum that considers only the relevant structural aspects of a type. For
/// example, two `Type::Struct`s are considered equal if they have the same
/// number of members, and the member types are all equal.
pub enum Type<'a> {
    /// Unsigned 8-bit value
    U8,
    /// Unsigned 16-bit value
    U16,
    /// Unsigned 32-bit value
    U32,
    /// Signed 8-bit value
    S8,
    /// Signed 16-bit value
    S16,
    /// Signed 32-bit value
    S32,

    /// Binary coded decimal. The single argument is the number of digits.
    BCD            (usize),
    /// Fixed point arithmetic value. The attributes represent the number of
    /// bits before and after the decimal point, respectively.
    Fixed          (usize, usize),
    /// Bit vector (bitvec) type. The first argument is the number of bits
    /// in the full vector, and the second argument is a vector of members.
    Bitvec         (usize, Vec<BitvecMember>),

    /// A pointer to a value of the given type.
    Pointer        (&'a Type<'a>),
    /// An array of values of the given type. The first argument represents
    /// the number of elements in the array, as declared.
    Array          (usize, &'a Type<'a>),
    /// A struct.
    Struct         (Vec<StructMember<'a>>),
}

/// A member of a struct.
pub struct StructMember<'a> {
    pub name:      String,
    pub typ:       &'a Type<'a>,
}

/// A member of a bitvec.
pub enum BitvecMember {
    Literal        (usize, usize),
    Variable       (usize, String),
}

impl<'a> PartialEq for Type<'a> {
    fn eq(&self, other: &Type<'a>) -> bool {
        use self::Type::*;

        if self as *const Type<'a> == other as *const Type<'a> {
            return true;
        }

        match (self, other) {
            (&U8,  &U8)  => true,
            (&U16, &U16) => true,
            (&U32, &U32) => true,
            (&S8,  &S8)  => true,
            (&S16, &S16) => true,
            (&S32, &S32) => true,

            (&BCD(x), &BCD(y))
                => x == y,
            (&Fixed(a,b), &Fixed(c,d))
                => (a == c) && (b == d),
            (&Pointer(ref t1), &Pointer(ref t2))
                => t1 == t2,
            (&Array(n1, ref t1), &Array(n2, ref t2))
                => n1 == n2 && t1 == t2,

            (&Struct(ref v1), &Struct(ref v2)) => {
                if v1.len() != v2.len() {
                    return false;
                }

                for (a, b) in v1.iter().zip(v2.iter()) {
                    if a.typ != b.typ {
                        return false;
                    }
                }

                return true
            },

            (&Bitvec(n1, ref v1), &Bitvec(n2, ref v2)) => {
                if n1 != n2 || v1.len() != v2.len() {
                    return false;
                }

                for (a, b) in v1.iter().zip(v2.iter()) {
                    if a.size() != b.size() {
                        return false;
                    }
                }

                return true
            },

            _ => false,
        }
    }
}

impl<'a> fmt::Debug for Type<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        use self::Type::*;

        match *self {
            U8  => f.write_str("u8"),
            U16 => f.write_str("u16"),
            U32 => f.write_str("u32"),
            S8  => f.write_str("s8"),
            S16 => f.write_str("s16"),
            S32 => f.write_str("s32"),

            BCD(x) => f.write_fmt(format_args!("bcd<{}>", x)),
            Fixed(x,y) => f.write_fmt(format_args!("fixed<{},{}>", x, y)),
            Bitvec(n, _) => f.write_fmt(format_args!("bitvec<{}>(...)", n)),

            Pointer(ref to) =>
                f.write_fmt(format_args!("*{:?}", to)),
            Array(n, ref to) =>
                f.write_fmt(format_args!("[{}]{:?}", n, to)),
            Struct(ref v) => {
                let mut comma = false;
                try!(f.write_str("struct{"));
                for x in v.iter() {
                    if !comma {
                        try!(x.typ.fmt(f));
                        comma = true;
                    } else {
                        try!(f.write_str(","));
                        try!(x.typ.fmt(f));
                    };
                }
                f.write_str(";")
            }
        }
    }
}

impl BitvecMember {
    fn size(&self) -> usize {
        match *self {
            BitvecMember::Literal(x, _)   => x,
            BitvecMember::Variable(x, _)  => x,
        }
    }
}

// UNIT TESTS
// =============================================================================

#[test]
fn test_simple_type_formatters() {
    assert_eq!("u8",  format!("{:?}", Type::U8));
    assert_eq!("u16", format!("{:?}", Type::U16));
    assert_eq!("u32", format!("{:?}", Type::U32));
    assert_eq!("s8",  format!("{:?}", Type::S8));
    assert_eq!("s16", format!("{:?}", Type::S16));
    assert_eq!("s32", format!("{:?}", Type::S32));

    assert_eq!("bcd<8>", format!("{:?}", Type::BCD(8)));
    assert_eq!("fixed<8,24>", format!("{:?}", Type::Fixed(8, 24)));
    // leave out Bitvec for now (TODO)
}

#[test]
fn test_fancy_type_formatters() {
    assert_eq!("*bcd<2>", format!("{:?}",
        Type::Pointer(&Type::BCD(2))
    ));
    assert_eq!("[5]**fixed<16,16>", format!("{:?}",
        Type::Array(5, &Type::Pointer(&Type::Pointer(&Type::Fixed(16, 16))))
    ));
    // leave out Struct for now (TODO)
}

#[test]
fn test_simple_equivalence() {
    assert_eq!(Type::U8,  Type::U8);
    assert_eq!(Type::U16, Type::U16);
    assert_eq!(Type::U32, Type::U32);
    assert_eq!(Type::S8,  Type::S8);
    assert_eq!(Type::S16, Type::S16);
    assert_eq!(Type::S32, Type::S32);

    assert_eq!(Type::BCD(3), Type::BCD(3));

    assert_eq!(Type::Fixed(2, 4), Type::Fixed(2, 4));
}

#[test]
fn test_simple_inequivalence() {
    // don't want to be exhaustive here. deal with it.

    assert!(Type::U8 != Type::U16);
    assert!(Type::U32 != Type::S8);

    assert!(Type::BCD(3) != Type::BCD(6));

    assert!(Type::Fixed(2, 2) != Type::Fixed(2, 5));
    assert!(Type::Fixed(2, 2) != Type::Fixed(3, 2));
    assert!(Type::Fixed(1, 2) != Type::Fixed(2, 1));
}
