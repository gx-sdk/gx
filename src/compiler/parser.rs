use compiler::token::Token;
use compiler::tree::*;

pub struct Parser<It> {
    input: It,
    ungot: Vec<Token>,
}

impl <It: Iterator<Token>> Parser<It> {
    pub fn new(input: It) -> Parser<It> {
        Parser {
            input: input,
            ungot: Vec::with_capacity(5),
        }
    }

    fn untok(&mut self, t: Token) {
        self.ungot.push(t);
    }

    fn gettok(&mut self) -> Token {
        match self.ungot.pop() {
            Some(t) => t,
            None => match self.input.next() {
                Some(t) => t,
                None => Token::TokIgnore,
            }
        }
    }

    fn expect(&mut self, t: Token) {
        let t_ = self.gettok();
        if t != t_ {
            panic!("expected {}, got {}", t, t_)
        }
    }

    /* expr  -> term expr'
       expr' -> \epsilon
             -> '+' expr
             -> '-' expr */
    pub fn expr(&mut self) -> Expression {
        let opn0 = self.term();

        match self.gettok() {
            Token::TokPlus => Expression {
                opr: OprAdd,
                opn0: OpnExpression(box opn0),
                opn1: OpnExpression(box self.expr()),
            },
            Token::TokMinus => Expression {
                opr: OprSubtract,
                opn0: OpnExpression(box opn0),
                opn1: OpnExpression(box self.expr()),
            },

            c => { self.untok(c); opn0 }
        }
    }

    /* term  -> factor term'
       term' -> \epsilon
             -> '*' term
             -> '/' term */
    pub fn term(&mut self) -> Expression {
        let opn0 = self.factor();

        match self.gettok() {
            Token::TokStar => Expression {
                opr: OprMultiply,
                opn0: OpnExpression(box opn0),
                opn1: OpnExpression(box self.expr()),
            },
            Token::TokSlash => Expression {
                opr: OprDivide,
                opn0: OpnExpression(box opn0),
                opn1: OpnExpression(box self.expr()),
            },

            c => { self.untok(c); opn0 }
        }
    }

    /* factor -> NUM
              -> '(' expr ')' */
    pub fn factor(&mut self) -> Expression {
        match self.gettok() {
            Token::TokNumber(n) => Expression {
                opr: OprIdentity,
                opn0: OpnConstant(n),
                opn1: OpnNothing,
            },
            Token::TokLParen => {
                let ex = self.expr();
                self.expect(Token::TokRParen);
                ex
            },

            c => panic!("unexpected {}", c),
        }
    }
}
