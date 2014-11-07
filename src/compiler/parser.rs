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

    /* expr  -> term
             -> term '+' expr
             -> term '-' expr */
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

    /* term  -> factor
             -> factor '*' term
             -> factor '/' term */
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

    /* factor -> Number
              -> Identifier
              -> '(' expr ')' */
    pub fn factor(&mut self) -> Expression {
        match self.gettok() {
            Token::TokNumber(n) => Expression {
                opr: OprIdentity,
                opn0: OpnConstant(n),
                opn1: OpnNothing,
            },
            Token::TokIdentifier(s) => Expression {
                opr: OprIdentifier,
                opn0: OpnIdentifier(s),
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

    /* stmt-list -> \epsilon
                 -> stmt stmt-list */
    pub fn stmt_list(&mut self) -> StatementList {
        /* this should be unrolled so recursion is not needed */

        let t = self.gettok();

        match t {
            Token::TokPrint | Token::TokIf | Token::TokIdentifier(_) => {
                self.untok(t);
                Node(self.stmt(), box self.stmt_list())
            },

            _ => {
                self.untok(t);
                Nil
            },
        }
    }

    /* stmt -> 'print' expr ';'
            -> 'if' expr '{' stmt-list '}'
            -> Identifier '=' expr ';' */
    pub fn stmt(&mut self) -> Statement {
        match self.gettok() {
            Token::TokPrint => {
                let st = Print(self.expr());
                self.expect(Token::TokSemicolon);
                st
            },

            Token::TokIf => {
                let ex = self.expr();
                self.expect(Token::TokLBrace);
                let st = box self.stmt_list();
                self.expect(Token::TokRBrace);
                IfBlock(ex, st)
            },

            Token::TokIdentifier(s) => {
                self.expect(Token::TokAssign);
                let st = Assign(s, self.expr());
                self.expect(Token::TokSemicolon);
                st
            },

            c => panic!("expected 'if', 'print', or assignment, got {}", c),
        }
    }
}
