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
        self.expect(Token::LBrace);

        let mut body = Vec::new();

        loop {
            match self.var_decl_o() {
                Some(x) => body.push(x),
                None    => break,
            }
        }

        self.expect(Token::RBrace);

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
                x             => { self.untok(x); break },
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

    pub fn global_var_decl_o(&mut self) -> Option<GlobalVarDecl> {
        match *self.peek() {
            Token::Var  => Some(self.global_var_decl()),
            _           => None,
        }
    }

    pub fn global_var_decl(&mut self) -> GlobalVarDecl {
        self.expect(Token::Var);

        GlobalVarDecl {
            storage:   self.storage(),
            decl:      self.var_decl(),
        }
    }

    pub fn storage(&mut self) -> Storage {
        Storage {
            loc:       self.storage_loc(),
            params:    self.storage_params(),
        }
    }

    pub fn storage_loc(&mut self) -> StorageLoc {
        match self.gettok() {
            Token::Ram => StorageLoc::RAM,
            Token::Rom => StorageLoc::ROM,
            x => { self.untok(x); StorageLoc::Default },
        }
    }

    pub fn storage_params(&mut self) -> Vec<StorageParam> {
        let mut params = Vec::new();

        loop {
            match self.storage_param_o() {
                Some(x)  => params.push(x),
                None     => return params
            }
        }
    }

    pub fn storage_param_o(&mut self) -> Option<StorageParam> {
        None
    }

    pub fn var_decl_o(&mut self) -> Option<VarDecl> {
        match *self.peek() {
            Token::Identifier(_)  => Some(self.var_decl()),
            _                     => None
        }
    }

    pub fn var_decl(&mut self) -> VarDecl {
        let ids = self.id_list();
        self.expect(Token::Colon);
        let typ = self.type_spec();
        let init = match self.gettok() {
            Token::Semicolon => None,
            Token::Assign => {
                let x = Some(self.expr());
                self.expect(Token::Semicolon);
                x
            },
            x => panic!("expected = or ; got {}", x)
        };

        VarDecl {
            ids:       ids,
            typ:       typ,
            init:      init,
        }
    }

    pub fn const_decl(&mut self) -> ConstDecl {
        self.expect(Token::Const);
        let id = self.id();
        self.expect(Token::Colon);
        let typ = self.type_spec();
        self.expect(Token::Assign);
        let init = self.constant();
        self.expect(Token::Semicolon);

        ConstDecl {
            id:        id,
            typ:       typ,
            init:      init
        }
    }

    pub fn region_decl(&mut self) -> RegionDecl {
        let name = self.region_name();
        self.expect(Token::LBrace);
        let vars = self.region_decl_body();
        self.expect(Token::RBrace);

        RegionDecl {
            name:      name,
            vars:      vars,
        }
    }

    pub fn region_name(&mut self) -> RegionName {
        self.expect(Token::Region);
        self.expect(Token::LParen);
        let section = self.id();
        self.expect(Token::Comma);
        let layer = self.id();
        self.expect(Token::RParen);

        RegionName {
            section:   section,
            layer:     layer
        }
    }

    pub fn region_decl_body(&mut self) -> Vec<GlobalVarDecl> {
        let mut body = Vec::new();

        loop {
            match self.global_var_decl_o() {
                Some(x)  => body.push(x),
                None     => return body,
            }
        }
    }

    pub fn func_decl(&mut self) -> FuncDecl {
        self.expect(Token::Fn);
        let name = self.id();
        self.expect(Token::LParen);
        let params = self.func_params();
        self.expect(Token::RParen);
        let ret = self.func_return();
        self.expect(Token::LBrace);
        let body = self.stmt_list();
        self.expect(Token::RBrace);

        FuncDecl {
            name:      name,
            params:    params,
            ret:       ret,
            body:      body
        }
    }

    pub fn func_return(&mut self) -> Option<TypeSpec> {
        match self.gettok() {
            Token::Colon => Some(self.type_spec()),
            x => { self.untok(x); None }
        }
    }

    pub fn func_params(&mut self) -> Vec<FuncParam> {
        let mut params = Vec::new();

        loop {
            match self.func_param_o() {
                Some(x)  => params.push(x),
                None     => return params
            }

            match self.gettok() {
                Token::Comma   => { },
                x              => { self.untok(x); return params; }
            }
        }
    }

    pub fn func_param_o(&mut self) -> Option<FuncParam> {
        match *self.peek() {
            Token::Identifier(_) => Some(self.func_param()),
            _                    => None,
        }
    }

    pub fn func_param(&mut self) -> FuncParam {
        let ids = self.id_list();
        self.expect(Token::Colon);

        FuncParam {
            ids:       ids,
            typ:       self.type_spec(),
        }
    }

    pub fn stmt_list(&mut self) -> Stmt {
        let mut stmts = Vec::new();

        loop {
            match self.stmt_o() {
                Some(x)  => stmts.push(x),
                None     => return Stmt::Compound(stmts),
            }
        }
    }

    pub fn stmt_o(&mut self) -> Option<Stmt> {
        let tok = self.gettok();

        match tok {
            Token::LBrace => {
                let st = self.stmt_list();
                self.expect(Token::RBrace);
                Some(st)
            },

            Token::Var      => Some(Stmt::Var(self.var_decl())),

            Token::If       => { self.untok(tok); Some(self.if_stmt()) },
            Token::Switch   => { self.untok(tok); Some(self.switch_stmt()) },
            Token::Loop     => { self.untok(tok); Some(self.loop_stmt()) },
            Token::While    => { self.untok(tok); Some(self.while_stmt()) },
            Token::For      => { self.untok(tok); Some(self.for_stmt()) },

            Token::Break    => { self.stmt_simple_o(Stmt::Break) },
            Token::Continue => { self.stmt_simple_o(Stmt::Continue) },
            Token::Repeat   => { self.stmt_simple_o(Stmt::Repeat) },

            Token::Return => {
                match self.gettok() {
                    Token::Semicolon => Some(Stmt::Return(None)),
                    x => {
                        self.untok(x);
                        let ex = self.expr();
                        self.stmt_simple_o(Stmt::Return(Some(ex)))
                    }
                }
            },

            /* everything an expr can start with. KEEP UPDATED! */
            Token::Incr           |
            Token::Decr           |
            Token::Excl           |
            Token::Tilde          |
            Token::Star           |
            Token::Amp            |
            Token::Sizeof         |
            Token::Identifier(_)  |
            Token::Number(_)      |
            Token::String(_)      |
            Token::Character(_)   |
            Token::LParen         => {
                self.untok(tok);
                let ex = self.expr();
                self.stmt_simple_o(Stmt::Eval(ex))
            },

            _ => { self.untok(tok); None }
        }
    }

    pub fn stmt(&mut self) -> Stmt {
        match self.stmt_o() {
            Some(x)  => x,
            None     => panic!("expected statement")
        }
    }

    pub fn stmt_simple_o(&mut self, stmt: Stmt) -> Option<Stmt> {
        self.expect(Token::Semicolon);
        Some(stmt)
    }

    pub fn if_stmt(&mut self) -> Stmt {
        self.expect(Token::If);
        self.expect(Token::LParen);
        let cond = self.expr();
        self.expect(Token::RParen);
        let tb = box self.stmt();
        let fb = match self.gettok() {
            Token::Else => Some(box self.stmt()),
            x => { self.untok(x); None },
        };

        Stmt::If(IfStmt {
            cond:      cond,
            tb:        tb,
            fb:        fb,
        })
    }

    pub fn switch_stmt(&mut self) -> Stmt {
        self.expect(Token::Switch);
        self.expect(Token::LParen);
        let ex = self.expr();
        self.expect(Token::RParen);
        self.expect(Token::LBrace);
        let body = self.switch_body();
        self.expect(Token::RBrace);

        Stmt::Switch(SwitchStmt {
            ex:        ex,
            cases:     body,
        })
    }

    pub fn switch_body(&mut self) -> Vec<SwitchCase> {
        let mut body = Vec::new();

        loop {
            match self.switch_case_o() {
                Some(x)  => body.push(x),
                None     => return body
            }
        }
    }

    pub fn switch_case_o(&mut self) -> Option<SwitchCase> {
        match *self.peek() {
            Token::Case | Token::Default => Some(self.switch_case()),
            _ => None,
        }
    }

    pub fn switch_case(&mut self) -> SwitchCase {
        match self.gettok() {
            Token::Case => {
                let c = self.constant();
                self.expect(Token::Colon);
                SwitchCase::Case(c, self.stmt_list())
            },

            Token::Default => {
                self.expect(Token::Colon);
                SwitchCase::Default(self.stmt_list())
            },

            _ => panic!("expected 'case' or 'default'"),
        }
    }

    pub fn loop_stmt(&mut self) -> Stmt {
        self.expect(Token::Loop);

        Stmt::Loop(LoopStmt {
            body:      box self.stmt()
        })
    }

    pub fn while_stmt(&mut self) -> Stmt {
        self.expect(Token::While);
        self.expect(Token::LParen);
        let cond = self.expr();
        self.expect(Token::RParen);

        Stmt::While(WhileStmt {
            cond:      cond,
            body:      box self.stmt(),
        })
    }

    pub fn for_stmt(&mut self) -> Stmt {
        self.expect(Token::For);
        self.expect(Token::LParen);
        let id = self.id();
        self.expect(Token::In);
        let iter = self.expr();
        self.expect(Token::RParen);

        Stmt::For(ForStmt {
            id:        id,
            iter:      iter,
            body:      box self.stmt(),
        })
    }

    pub fn expr(&mut self) -> Expr {
        self.ex_comma()
    }

    pub fn ex_list(&mut self) -> Vec<Expr> {
        let mut list = Vec::new();

        loop {
            /* this is awful, it's everything that can follow an ex-list,
               and it stops if it sees that */
            match *self.peek() {
                Token::RParen => return list,
                _ => { }
            }

            list.push(self.ex_assign());

            match *self.peek() {
                Token::Comma => { self.gettok(); },
                _ => return list,
            }
        }
    }

    pub fn ex_comma(&mut self) -> Expr {
        let left = self.ex_assign();

        match *self.peek() {
            Token::Comma => { },
            _ => { return left }
        }

        let mut vec = Vec::new();
        vec.push(left);

        loop {
            match self.gettok() {
                Token::Comma => vec.push(self.ex_assign()),
                x => { self.untok(x); return Expr::Comma(vec) }
            }
        }
    }

    pub fn ex_assign(&mut self) -> Expr {
        let left = self.ex_tern();

        let op = match *self.peek() {
            Token::Assign    => BinOp::Eq,
            Token::PlusEq    => BinOp::Add,
            Token::MinusEq   => BinOp::Sub,
            Token::StarEq    => BinOp::Mul,
            Token::SlashEq   => BinOp::Div,
            Token::ModEq     => BinOp::Mod,
            Token::LShiftEq  => BinOp::LShift,
            Token::RShiftEq  => BinOp::RShift,
            Token::BitAndEq  => BinOp::BitAnd,
            Token::BitXorEq  => BinOp::BitXor,
            Token::BitOrEq   => BinOp::BitOr,
            _ => return left,
        };

        self.gettok();

        Expr::Assign(
            box left,
            box self.ex_assign(),
            op
        )
    }

    pub fn ex_tern(&mut self) -> Expr {
        let left = self.ex_lor();

        match *self.peek() {
            Token::Question => { },
            _ => return left,
        }

        self.expect(Token::Question);
        let mid = self.ex_lor();
        self.expect(Token::Colon);

        Expr::Ternary(
            box left,
            box mid,
            box self.ex_tern()
        )
    }

    pub fn ex_lor(&mut self) -> Expr {
        let left = self.ex_land();

        match self.gettok() {
            Token::DblPipe =>
                Expr::Binary(BinOp::BoolOr, box left, box self.ex_lor()),
            tok =>
                { self.untok(tok); left }
        }
    }

    pub fn ex_land(&mut self) -> Expr {
        let left = self.ex_bor();

        match self.gettok() {
            Token::DblAmp =>
                Expr::Binary(BinOp::BoolAnd, box left, box self.ex_land()),
            tok =>
                { self.untok(tok); left }
        }
    }

    pub fn ex_bor(&mut self) -> Expr {
        let left = self.ex_bxor();

        match self.gettok() {
            Token::Pipe =>
                Expr::Binary(BinOp::BitOr, box left, box self.ex_bor()),
            tok =>
                { self.untok(tok); left }
        }
    }

    pub fn ex_bxor(&mut self) -> Expr {
        let left = self.ex_band();

        match self.gettok() {
            Token::Caret =>
                Expr::Binary(BinOp::BitXor, box left, box self.ex_bxor()),
            tok =>
                { self.untok(tok); left }
        }
    }

    pub fn ex_band(&mut self) -> Expr {
        let left = self.ex_eq();

        match self.gettok() {
            Token::Amp =>
                Expr::Binary(BinOp::BitAnd, box left, box self.ex_band()),
            tok =>
                { self.untok(tok); left }
        }
    }

    pub fn ex_eq(&mut self) -> Expr {
        let left = self.ex_cmp();

        match self.gettok() {
            Token::Eq =>
                Expr::Binary(BinOp::Eq, box left, box self.ex_eq()),
            Token::NotEq =>
                Expr::Binary(BinOp::NotEq, box left, box self.ex_eq()),

            tok =>
                { self.untok(tok); left }
        }
    }

    pub fn ex_cmp(&mut self) -> Expr {
        let left = self.ex_shift();

        match self.gettok() {
            Token::Less =>
                Expr::Binary(BinOp::Less, box left, box self.ex_cmp()),
            Token::Greater =>
                Expr::Binary(BinOp::Greater, box left, box self.ex_cmp()),
            Token::LessEq =>
                Expr::Binary(BinOp::LessEq, box left, box self.ex_cmp()),
            Token::GreaterEq =>
                Expr::Binary(BinOp::GreaterEq, box left, box self.ex_cmp()),

            tok =>
                { self.untok(tok); left }
        }
    }

    pub fn ex_shift(&mut self) -> Expr {
        let left = self.ex_add();

        match self.gettok() {
            Token::LShift =>
                Expr::Binary(BinOp::LShift, box left, box self.ex_shift()),
            Token::RShift =>
                Expr::Binary(BinOp::RShift, box left, box self.ex_shift()),

            tok =>
                { self.untok(tok); left }
        }
    }

    pub fn ex_add(&mut self) -> Expr {
        let left = self.ex_mul();

        match self.gettok() {
            Token::Plus =>
                Expr::Binary(BinOp::Add, box left, box self.ex_add()),
            Token::Minus =>
                Expr::Binary(BinOp::Sub, box left, box self.ex_add()),

            tok =>
                { self.untok(tok); left }
        }
    }

    pub fn ex_mul(&mut self) -> Expr {
        let left = self.ex_unary();

        match self.gettok() {
            Token::Star =>
                Expr::Binary(BinOp::Mul, box left, box self.ex_mul()),
            Token::Slash =>
                Expr::Binary(BinOp::Div, box left, box self.ex_mul()),
            Token::Mod =>
                Expr::Binary(BinOp::Mod, box left, box self.ex_mul()),

            tok =>
                { self.untok(tok); left }
        }
    }

    pub fn ex_unary(&mut self) -> Expr {
        match self.gettok() {
            Token::Incr =>
                Expr::Unary(UnOp::PreIncr, box self.ex_unary()),
            Token::Decr =>
                Expr::Unary(UnOp::PreDecr, box self.ex_unary()),
            Token::Excl =>
                Expr::Unary(UnOp::BoolNot, box self.ex_unary()),
            Token::Tilde =>
                Expr::Unary(UnOp::BitNot, box self.ex_unary()),
            Token::Star =>
                Expr::Unary(UnOp::Deref, box self.ex_unary()),
            Token::Amp =>
                Expr::Unary(UnOp::AddrOf, box self.ex_unary()),
            Token::Sizeof =>
                Expr::Unary(UnOp::SizeOf, box self.ex_unary()),

            tok =>
                { self.untok(tok); self.ex_bottom() }
        }
    }

    pub fn ex_bottom(&mut self) -> Expr {
        let left = self.ex_primary();

        match self.gettok() {
            Token::Incr =>
                Expr::Unary(UnOp::PostIncr, box left),
            Token::Decr =>
                Expr::Unary(UnOp::PostDecr, box left),
            Token::LParen => {
                let ex = Expr::Call(box left, self.ex_list());
                self.expect(Token::RParen);
                ex
            },
            Token::LBrack => {
                let ex = Expr::Binary(BinOp::Element, box left, box self.expr());
                self.expect(Token::RBrack);
                ex
            },
            Token::Dot =>
                Expr::Member(box left, self.id()),

            tok =>
                { self.untok(tok); left }
        }
    }

    pub fn ex_primary(&mut self) -> Expr {
        match *self.peek() {
            Token::Identifier(_) =>
                Expr::Id(self.id()),
            Token::Number(x) =>
                { self.gettok(); Expr::Number(x as int) }
            Token::Character(x) =>
                { self.gettok(); Expr::Number(x as int) }
            Token::LParen => {
                self.expect(Token::LParen);
                let ex = self.expr();
                self.expect(Token::RParen);
                ex
            }

            _ => { panic!("expected expression") }
        }
    }
}
