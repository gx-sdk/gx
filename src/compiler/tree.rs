/* parse tree definitions */

pub enum Operator {
    Add,
    Subtract,
    Multiply,
    Divide,
}

pub enum Operand {
    Nothing,
    Expression  (Operator, Box<Operand>, Box<Operand>),
    Identifier  (String),
    Constant    (uint),
}

impl Operator {
    pub fn apply(&self, a: uint, b: uint) -> uint {
        match *self {
            Add          => a + b,
            Subtract     => a - b,
            Multiply     => a * b,
            Divide       => a / b,
        }
    }
}

impl Operand {
    pub fn is_constant(&self) -> bool {
        match *self {
            Expression(_, ref opn0, ref opn1) =>
                opn0.is_constant() && opn1.is_constant(),
            Constant(_) => true,
            _ => false,
        }
    }

    pub fn get_value(&self) -> uint {
        match *self {
            Expression(opr, ref opn0, ref opn1) =>
                opr.apply(opn0.get_value(), opn1.get_value()),
            Constant(x) => x,
            _ => 0,
        }
    }
}

pub enum StatementList {
    Node(Statement, Box<StatementList>),
    Nil,
}

pub enum Statement {
    Print    (Operand),
    IfBlock  (Operand, Box<StatementList>),
    Assign   (String, Operand),
}

impl StatementList {
    pub fn len(&self) -> uint {
        match *self { Node(_, ref xs) => 1 + xs.len(), Nil => 0 }
    }

    pub fn is_nil(&self) -> bool {
        match *self { Node(_, _) => false, Nil => true }
    }
}
