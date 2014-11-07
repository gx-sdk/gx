/* parse tree definitions */

pub enum Operator {
    OprIdentity,

    OprIdentifier,
    OprConstant,

    OprAdd,
    OprSubtract,
    OprMultiply,
    OprDivide,
}

pub enum Operand {
    OpnNothing,
    OpnExpression  (Box<Expression>),
    OpnIdentifier  (String),
    OpnConstant    (uint),
}

pub struct Expression {
    pub opr: Operator,
    pub opn0: Operand,
    pub opn1: Operand,
}

impl Operator {
    pub fn is_binary(&self) -> bool {
        match *self {
            OprAdd          => true,
            OprSubtract     => true,
            OprMultiply     => true,
            OprDivide       => true,

            _               => false
        }
    }

    pub fn apply(&self, a: uint, b: uint) -> uint {
        match *self {
            OprIdentity     => a,

            OprConstant     => a,

            OprAdd          => a + b,
            OprSubtract     => a - b,
            OprMultiply     => a * b,
            OprDivide       => a / b,

            _               => 0,
        }
    }
}

impl Operand {
    pub fn is_constant(&self) -> bool {
        match *self {
            OpnConstant(_)         => true,
            OpnExpression(ref ex)  => ex.is_constant(),

            _                      => false,
        }
    }

    pub fn get_value(&self) -> uint {
        match *self {
            OpnConstant(x)         => x,
            OpnExpression(ref ex)  => ex.get_value(),

            _                      => 0,
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

pub enum StatementList {
    Node(Statement, Box<StatementList>),
    Nil,
}

pub enum Statement {
    Print    (Expression),
    IfBlock  (Expression, Box<StatementList>),
    Assign   (String, Expression),
}

impl StatementList {
    fn len(&self) -> uint {
        match *self { Node(_, ref xs) => 1 + xs.len(), Nil => 0 }
    }
}
