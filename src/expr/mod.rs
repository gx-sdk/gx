// gx language implementation
// Copyright (C) 2014 Alex Iadicicco
//
// For licensing information, refer to the COPYING file
// in the project root

//! This module contains code for manipulating generic `gx` expressions.
//! The main `Expr` type has a 'primary type' as a type parameter. This
//! primary type is essentially the type of the leaves of the expression.

/// The main expression type. The type parameter `P` is the type of primary
/// expressions. Primary expressions are the non-recursive subexpressions
/// in an expression system. For example, a simple calculator has integers
/// as primaries, since they cannot be broken apart in terms of the
/// grammar of expressions provided by the calculator. In the parse phase,
/// primaries are things like identifiers and constants.
pub enum Expr<P> {

    /// Evaluates the sub-expressions in order, discarding the results of
    /// all but the last. The result of the last subexpression becomes the
    /// result of the whole expression.
    Comma          (Vec<Expr<P>>),

    /// Assigns the value of the second expression to the location of the
    /// first expression, optionally performing an operation with the
    /// old value, as with +=, -=, etc. in most modern languages
    Assign         (Option<BinOp>, Box<Expr<P>>, Box<Expr<P>>),

    /// Evaluates the first expression and interprets as a boolean value.
    /// If true, the second expression is evaluated and becomes the value
    /// of the whole expression. If false, the third expression is used.
    Ternary        (Box<Expr<P>>, Box<Expr<P>>, Box<Expr<P>>),

    /// Combines the two subexpressions using the given binary operator.
    Binary         (BinOp, Box<Expr<P>>, Box<Expr<P>>),

    /// Applies the given unary operator.
    Unary          (UnOp, Box<Expr<P>>),

    /// Evaluates the first expression, and calls it as a function with
    /// the results of the expressions in the vector as arguments.
    Call           (Box<Expr<P>>, Vec<Expr<P>>),

    /// Evaluates the expression, then evaluates to the member of the
    /// resulting scope with the given identifier.
    Member         (Box<Expr<P>>, String),

    /// Irreducible subexpression.
    Primary        (P),
}

/// Binary operators, from a grammar standpoint. This list is re-implemented
/// in later passes as needed.
#[derive(Show)]
pub enum BinOp {
    #[doc = "addition"]                         Add,
    #[doc = "subtraction"]                      Sub,
    #[doc = "multiplication"]                   Mul,
    #[doc = "division"]                         Div,
    #[doc = "modulus"]                          Mod,
    #[doc = "left shift"]                       LShift,
    #[doc = "right shift"]                      RShift,
    #[doc = "bitwise and"]                      BitAnd,
    #[doc = "bitwise or"]                       BitOr,
    #[doc = "bitwise exclusive or"]             BitXor,
    #[doc = "boolean and"]                      BoolAnd,
    #[doc = "boolean or"]                       BoolOr,
    #[doc = "element-of (array subscript)"]     Element,
    #[doc = "equality test"]                    Eq,
    #[doc = "inequality test"]                  NotEq,
    #[doc = "less than"]                        Less,
    #[doc = "greater than"]                     Greater,
    #[doc = "less than or equal to"]            LessEq,
    #[doc = "greater than or equal to"]         GreaterEq,
}
impl Copy for BinOp {
}

/// Unary operators, taking a single operand
#[derive(Show)]
pub enum UnOp {
    #[doc = "pre-incerment"]                    PreIncr,
    #[doc = "post-increment"]                   PostIncr,
    #[doc = "pre-decrement"]                    PreDecr,
    #[doc = "post-decrement"]                   PostDecr,
    #[doc = "address-of"]                       AddrOf,
    #[doc = "pointer dereference"]              Deref,
    #[doc = "sizeof"]                           SizeOf,
    #[doc = "bitwise negation"]                 BitNot,
    #[doc = "boolean negation"]                 BoolNot,
}
impl Copy for UnOp {
}

impl BinOp {
    /// Returns a printable glyph representing the given binary
    /// operation. Useful mostly for debugging/diagnostic purposes
    pub fn glyph(&self) -> &'static str {
        match *self {
            BinOp::Add         => "+",
            BinOp::Sub         => "-",
            BinOp::Mul         => "*",
            BinOp::Div         => "/",
            BinOp::Mod         => "%",
            BinOp::LShift      => "<<",
            BinOp::RShift      => ">>",
            BinOp::BitAnd      => "&",
            BinOp::BitOr       => "|",
            BinOp::BitXor      => "^",
            BinOp::BoolAnd     => "&&",
            BinOp::BoolOr      => "||",
            BinOp::Element     => "[]",
            BinOp::Eq          => "==",
            BinOp::NotEq       => "!=",
            BinOp::Less        => "<",
            BinOp::Greater     => ">",
            BinOp::LessEq      => "<=",
            BinOp::GreaterEq   => ">=",
        }
    }
}

impl<P> Expr<P> {
    /// Returns true if the `Expr` is an instance of `Primary` or not
    pub fn is_primary(&self) -> bool {
        match *self {
            Expr::Primary(_) => true,
            _                => false,
        }
    }
}

/// Types implementing the `StaticOperand` trait may be combined at compile
/// time using the `BinOp` and `UnOp` operations.
pub trait StaticOperand {
    fn has_static_value(&self) -> bool;

    fn apply_bin_op(self, other: Self, op: BinOp) -> Self;
    fn apply_un_op(self, op: UnOp) -> Self;
}

impl<P: StaticOperand> Expr<P> {
    /// Returns true if the expression can be folded
    pub fn can_fold(&self) -> bool {
        match *self {
            Expr::Binary(_, box ref e1, box ref e2) =>
                e1.can_fold() && e2.can_fold(),

            Expr::Unary(_, box ref e) =>
                e.can_fold(),

            Expr::Primary(ref p) =>
                p.has_static_value(),

            _ => false,
        }
    }

    /// Folds the current expression, consuming it
    pub fn fold(self) -> Expr<P> {
        match self {
            Expr::Unary(op, box ex) =>
                match ex.fold() {
                    Expr::Primary(p) =>
                        Expr::Primary(p.apply_un_op(op)),

                    x =>
                        Expr::Unary(op, box x)
                },

            Expr::Binary(op, box ex1, box ex2) =>
                match (ex1.fold(), ex2.fold()) {
                    (Expr::Primary(p1), Expr::Primary(p2)) =>
                        Expr::Primary(p1.apply_bin_op(p2, op)),

                    (a, b) =>
                        Expr::Binary(op, box a, box b),
                },

            x => x
        }
    }
}
