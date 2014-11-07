/* parse tree definitions */

enum Operator {
    OprAdd,
    OprSubtract,
    OprMultiply,
    OprDivide,
}

enum Operand {
    OpnNothing,
    OpnExpression     (Box<Expression>),
    OpnConstant       (uint),
}

struct Expression {
    opr: Operator,
    opn: [Operand, ..2],
}

impl Operator {
    fn is_binary(&self) -> bool {
        match *self {
            OprAdd       => true,
            OprSubtract  => true,
            OprMultiply  => true,
            OprDivide    => true,
        }
    }
}

impl Operand {
    fn is_constant(&self) -> bool {
        match *self {
            OpnNothing             => false,
            OpnConstant(_)         => true,
            OpnExpression(ref ex)  => ex.is_constant(),
        }
    }
}

impl Expression {
    fn is_constant(&self) -> bool {
        self.opn[0].is_constant() && if self.opr.is_binary() {
            self.opn[1].is_constant()
        } else {
            true
        }
    }
}
