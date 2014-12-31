/* type system */

use std::collections::HashMap;

pub struct Type<'a> {
    pub name:      String,
    pub spec:      TypeSpec<'a>,
}

pub enum TypeSpec<'a> {
    /* singleton primitive types: */
    U8, U16, U32,
    S8, S16, S32,

    /* parameterized primitive types: */
    BCD            (uint),
    Fixed          (uint, uint),
    Bitvec         (uint, HashMap<&'a str, &'a BitvecMember>),

    /* compound types: */
    Pointer        (&'a Type<'a>),
    Array          (uint, &'a Type<'a>),
    Struct         (HashMap<&'a str, &'a StructMember<'a>>),
}

pub struct StructMember<'a> {
    pub name:      String,
    pub typ:       &'a Type<'a>,

    pub offs:      uint,
}

pub struct BitvecMember {
    pub name:      String,

    pub bitoffs:   uint,
    pub bitlen:    uint,
}

impl<'a> PartialEq for Type<'a> {
    fn eq(&self, other: &Type<'a>) -> bool {
        match self.spec {
            TypeSpec::U8  => match other.spec { TypeSpec::U8  => true, _ => false },
            TypeSpec::U16 => match other.spec { TypeSpec::U16 => true, _ => false },
            TypeSpec::U32 => match other.spec { TypeSpec::U32 => true, _ => false },
            TypeSpec::S8  => match other.spec { TypeSpec::S8  => true, _ => false },
            TypeSpec::S16 => match other.spec { TypeSpec::S16 => true, _ => false },
            TypeSpec::S32 => match other.spec { TypeSpec::S32 => true, _ => false },

            TypeSpec::BCD(x) => match other.spec {
                TypeSpec::BCD(y)            => x == y,
                _                           => false,
            },

            TypeSpec::Fixed(a,b) => match other.spec {
                TypeSpec::Fixed(c,d)        => (a == c) && (b == d),
                _                           => false,
            },

            TypeSpec::Pointer(ref t1) => match other.spec {
                TypeSpec::Pointer(ref t2)   => t1 == t2,
                _                           => false,
            },

            TypeSpec::Array(n1, ref t1) => match other.spec {
                TypeSpec::Array(n2, ref t2) => n1 == n2 && t1 == t2,
                _                           => false,
            },

            _ => false, /* TODO */
        }
    }
}
