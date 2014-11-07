/* parse tree definitions */

pub enum Operator {
    OprIdentity,
    OprConstant,

    OprAdd,
    OprSubtract,
    OprMultiply,
    OprDivide,
}

pub enum Operand {
    OpnNothing,
    OpnExpression     (Box<Expression>),
    OpnConstant       (uint),
}

pub struct Expression {
    pub opr: Operator,
    pub opn0: Operand,
    pub opn1: Operand,
}

impl Operator {
    pub fn is_binary(&self) -> bool {
        match *self {
            OprIdentity  => false,
            OprConstant  => false,

            OprAdd       => true,
            OprSubtract  => true,
            OprMultiply  => true,
            OprDivide    => true,
        }
    }

    pub fn apply(&self, a: uint, b: uint) -> uint {
        match *self {
            OprIdentity  => a,
            OprConstant  => a,

            OprAdd       => a + b,
            OprSubtract  => a - b,
            OprMultiply  => a * b,
            OprDivide    => a / b,
        }
    }
}

impl Operand {
    pub fn is_constant(&self) -> bool {
        match *self {
            OpnNothing             => false,
            OpnConstant(_)         => true,
            OpnExpression(ref ex)  => ex.is_constant(),
        }
    }

    pub fn get_value(&self) -> uint {
        match *self {
            OpnNothing             => 0,
            OpnConstant(x)         => x,
            OpnExpression(ref ex)  => ex.get_value(),
        }
    }
}

impl Expression {
    pub fn is_constant(&self) -> bool {
        self.opn0.is_constant() && if self.opr.is_binary() {
            self.opn1.is_constant()
        } else {
            true
        }
    }

    pub fn get_value(&self) -> uint {
        self.opr.apply(
            self.opn0.get_value(),
            self.opn1.get_value()
        )
    }
}
