/* type system */

pub enum Type<'a> {
    /* singleton primitive types: */
    U8, U16, U32,
    S8, S16, S32,

    /* parameterized primitive types: */
    BCD            (uint),
    Fixed          (uint, uint),
    Bitvec         (uint, Vec<BitvecMember>),

    /* compound types: */
    Pointer        (&'a Type<'a>),
    Array          (uint, &'a Type<'a>),
    Struct         (Vec<StructMember<'a>>),
}

pub struct StructMember<'a> {
    pub name:      String,
    pub typ:       &'a Type<'a>,
}

pub enum BitvecMember {
    Literal        (uint, uint),
    Variable       (uint, String),
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
            Struct(ref v) => {
                let mut body = String::new();
                for x in v.iter() {
                    let s = if body.is_empty() {
                        x.typ.to_string()
                    } else {
                        format!(",{}", x.typ.to_string())
                    };
                    body.push_str(s.as_slice());
                }
                format!("struct{{{}}}", body)
            }
        }
    }
}
