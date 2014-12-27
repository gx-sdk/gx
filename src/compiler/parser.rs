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
                None => Token::Ignore,
            }
        }
    }

    fn expect(&mut self, t: Token) {
        let t_ = self.gettok();

        if t != t_ {
            panic!("expected {}, got {}", t, t_)
        }
    }

    fn peek(&mut self) -> &Token {
        let t = self.gettok();
        self.untok(t);
        return &self.ungot[self.ungot.len() - 1];
    }

    pub fn id(&mut self) -> Id {
        match self.gettok() {
            Token::Identifier(x) => x,
            _ => panic!("expected identifier"),
        }
    }

    pub fn constant(&mut self) -> Expr {
        match self.gettok() {
            Token::Number(x) =>     Expr::Number(x as int),
            Token::Character(x) =>  Expr::Number(x as int),
            _ => panic!("expected constant"),
        }
    }

    pub fn number(&mut self) -> Number {
        match self.gettok() {
            Token::Number(x) =>     x as int,
            Token::Character(x) =>  x as int,
            _ => panic!("expected number"),
        }
    }

    pub fn id_list(&mut self) -> Vec<Id> {
        let mut v = Vec::new();

        loop {
            v.push(self.id());

            match *self.peek() {
                Token::Comma => { self.gettok(); },
                _ => return v
            }
        }
    }

    pub fn constant_list(&mut self) -> Vec<Expr> {
        let mut v = Vec::new();

        loop {
            v.push(self.constant());

            match *self.peek() {
                Token::Comma => { self.gettok(); },
                _ => return v
            }
        }
    }

    pub fn file(&mut self) -> Input {
        let mut v = Vec::new();

        loop {
            match *self.peek() {
                Token::Unit  => v.push(self.unit()),
                _ => return v,
            }
        }
    }

    pub fn unit(&mut self) -> Unit {
        self.expect(Token::Unit);
        let name = self.id();
        self.expect(Token::LBrace);
        let decls = self.decl_list();
        self.expect(Token::RBrace);

        Unit {
            name:          name,
            decls:         decls
        }
    }

    pub fn decl_list(&mut self) -> Vec<Decl> {
        let mut v = Vec::new();

        loop {
            match self.decl_o() {
                Some(x)    => v.push(x),
                _          => return v,
            }
        }
    }

    pub fn decl_o(&mut self) -> Option<Decl> {
        match *self.peek() {
            Token::Pub     |
            Token::Type    |
            Token::Fn      |
            Token::Var     |
            Token::Const   |
            Token::Region  =>
                Some(self.decl()),
            _ =>
                None
        }
    }

    pub fn decl(&mut self) -> Decl {
        let is_pub = match self.gettok() {
            Token::Pub     => true,
            x              => { self.untok(x); false }
        };

        Decl {
            is_pub:        is_pub,
            body:          self.decl_body(),
        }
    }

    pub fn decl_body(&mut self) -> DeclBody {
        match *self.peek() {
            Token::Type    => DeclBody::Type      (self.type_decl()),
            Token::Fn      => DeclBody::Func      (self.func_decl()),
            Token::Var     => DeclBody::GlobalVar (self.global_var_decl()),
            Token::Const   => DeclBody::Const     (self.const_decl()),
            Token::Region  => DeclBody::Region    (self.region_decl()),
            _ => panic!("expected declaration body!")
        }
    }

    pub fn type_decl(&mut self) -> TypeDecl {
        self.expect(Token::Type);
        let name = self.id();
        self.expect(Token::Colon);
        let typ = self.type_spec();
        self.expect(Token::Semicolon);

        TypeDecl {
            name:          name,
            typ:           typ,
        }
    }

    /* type-spec  -> id
                  -> id '<' constant-list '>'
                  -> '*' type-spec
                  -> '[' number ']' type-spec
                  -> struct-spec
                  -> bitvec-spec */
    pub fn type_spec(&mut self) -> TypeSpec {
        match self.gettok() {
            Token::Identifier(id) => {
                match self.gettok() {
                    Token::Less => {
                        let x = TypeSpec::Parameterized(id, self.constant_list());
                        self.expect(Token::Greater);
                        x
                    },

                    x => {
                        self.untok(x);
                        TypeSpec::Alias(id)
                    }
                }
            },

            Token::Star => {
                TypeSpec::Pointer(box self.type_spec())
            },

            Token::LBrack => {
                let x = self.number();
                self.expect(Token::RBrack);
                TypeSpec::Array(x, box self.type_spec())
            },

            Token::Struct => {
                self.untok(Token::Struct);
                self.struct_spec()
            },

            Token::Bitvec => {
                self.untok(Token::Bitvec);
                self.bitvec_spec()
            },

            _ => panic!("expected type specifier"),
        }
    }

    pub fn struct_spec(&mut self) -> TypeSpec {
        self.expect(Token::Struct);
        self.expect(Token::LBrack);

        let mut body = Vec::new();

        loop {
            match self.var_decl_o() {
                Some(x) => body.push(x),
                None    => break,
            }
        }

        self.expect(Token::RBrack);

        TypeSpec::Struct(body)
    }

    pub fn bitvec_spec(&mut self) -> TypeSpec {
        self.expect(Token::Bitvec);

        let size = match *self.peek() {
            Token::Less => {
                self.expect(Token::Less);
                let n = self.number();
                self.expect(Token::Greater);
                Some(n)
            },
            _ => None,
        };

        self.expect(Token::LParen);

        let mut body = Vec::new();

        loop {
            body.push(self.bitvec_member());

            match self.gettok() {
                Token::Comma  => { },
                x             => self.untok(x),
            }
        }

        self.expect(Token::RParen);

        TypeSpec::Bitvec(size, body)
    }

    pub fn bitvec_member(&mut self) -> BitvecMember {
        match self.gettok() {
            Token::Identifier(x) => {
                self.expect(Token::Colon);
                BitvecMember::Variable(x, self.number())
            },

            Token::BinaryString(x) => {
                /* TODO: binary string bitvec members */
                BitvecMember::Literal(0)
            },

            _ => panic!("expected bitvec member"),

        }
    }

    pub fn func_decl(&mut self) -> FuncDecl {
        panic!("not yet implemented") // TODO
    }

    pub fn global_var_decl(&mut self) -> GlobalVarDecl {
        panic!("not yet implemented") // TODO
    }

    pub fn const_decl(&mut self) -> ConstDecl {
        panic!("not yet implemented") // TODO
    }

    pub fn region_decl(&mut self) -> RegionDecl {
        panic!("not yet implemented") // TODO
    }

    pub fn var_decl_o(&mut self) -> Option<VarDecl> {
        None // TODO
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
