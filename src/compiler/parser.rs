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
    /*pub fn expr(&mut self) -> Operand {
        let opn0 = self.term();

        match self.gettok() {
            Token::TokPlus  => Expression(Add,      box opn0, box self.expr()),
            Token::TokMinus => Expression(Subtract, box opn0, box self.expr()),

            c => { self.untok(c); opn0 }
        }
    }*/

    /* term  -> factor
             -> factor '*' term
             -> factor '/' term */
    /*pub fn term(&mut self) -> Operand {
        let opn0 = self.factor();

        match self.gettok() {
            Token::TokStar  => Expression(Multiply, box opn0, box self.term()),
            Token::TokSlash => Expression(Divide,   box opn0, box self.term()),

            c => { self.untok(c); opn0 }
        }
    }*/

    /* factor -> Number
              -> Identifier
              -> '(' expr ')' */
    /*pub fn factor(&mut self) -> Operand {
        match self.gettok() {
            Token::TokNumber(n) => Constant(n),
            Token::TokIdentifier(s) => Identifier(s),
            Token::TokLParen => {
                let ex = self.expr();
                self.expect(Token::TokRParen);
                ex
            },

            c => panic!("unexpected {}", c),
        }
    }*/

    /* stmt-list -> \epsilon
                 -> stmt stmt-list */
    /*pub fn stmt_list(&mut self) -> StatementList {
        let mut stmts = Vec::new();

        loop {
            let t = self.gettok();

            match t {
                Token::TokIf | Token::TokIdentifier(_) => {
                    self.untok(t);
                    stmts.push(box self.stmt());
                },

                _ => {
                    self.untok(t);
                    return stmts;
                },
            }
        }
    }*/

    /* stmt -> 'print' expr ';'
            -> 'if' expr '{' stmt-list '}'
            -> Identifier '=' expr ';' */
    /*pub fn stmt(&mut self) -> Statement {
        match self.gettok() {
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
    }*/
}
