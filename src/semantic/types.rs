/* type system */

use std::collections::HashMap;

pub enum Type<'a> {
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

            _ => false, /* TODO */
        }
    }
}

impl<'a> ToString for Type<'a> {
    fn to_string(&self) -> String {
        use self::Type::*;

        match *self {
            U8  => String::from_str("u8"),
            U16 => String::from_str("u16"),
            U32 => String::from_str("u32"),
            S8  => String::from_str("s8"),
            S16 => String::from_str("s16"),
            S32 => String::from_str("s32"),

            BCD(x) => format!("bcd<{}>", x),
            Fixed(x,y) => format!("fixed<{},{}>", x, y),
            Bitvec(n, _) => format!("bitvec<{}>(...)", n),

            Pointer(ref to) => format!("*{}", to.to_string()),
            Array(n, ref to) => format!("[{}]{}", n, to.to_string()),
            Struct(_) => String::from_str("struct{...}"),
        }
    }
}
