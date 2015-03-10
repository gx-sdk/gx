// gx language implementation
// Copyright (C) 2014-present Alex Iadicicco <http://ajitek.net>
//
// For licensing information, refer to the COPYING file
// in the project root

//! Parser for the `gx` language. Internally is a mostly recursive descent
//! parser, with some PEG-like functions to handle associativity correctly.

use frontend::token::Token;
use frontend::tree::*;
use frontend::lexer::Lexer;
use frontend::lexer::LexerToken;
use frontend::lexer::Position;

use expr::*;

use std::io;

/// An instance of a parser. If you have an `Iterator<Token>` go ahead and
/// create one with `Parser::new`. The resulting `Parser` instance is full
/// of functions for parsing the different non-terminals in the grammar, but
/// the most relevant is probably `Parser::file`, corresponding to the start
/// symbol.
pub struct Parser<It> {
    input: Lexer<It>,
    ungot: Vec<LexerToken>,
}

impl<It: Iterator<Item = Result<char, io::CharsError>>> Parser<It> {
    pub fn new(input: Lexer<It>) -> Parser<It> {
        Parser {
            input: input,
            ungot: Vec::with_capacity(5),
        }
    }

    fn pos(&mut self) -> Position {
        if self.ungot.len() == 0 {
            self.input.pos()
        } else {
            self.ungot[self.ungot.len() - 1].1
        }
    }

    fn untok(&mut self, t: LexerToken) {
        self.ungot.push(t);
    }

    fn gettok(&mut self) -> LexerToken {
        match self.ungot.pop() {
            Some(t) => t,
            None => match self.input.next() {
                Some(t) => t,
                None => panic!("end of file when parsing"),
            }
        }
    }

    fn expect(&mut self, t: Token) {
        let t_ = self.gettok();

        if t != t_.0 {
            panic!("expected {:?}, got {:?}", t, t_.0)
        }
    }

    fn peek(&mut self) -> &Token {
        let t = self.gettok();
        self.untok(t);
        return &self.ungot[self.ungot.len() - 1].0;
    }

    pub fn id(&mut self) -> Id {
        match self.gettok().0 {
            Token::Identifier(x) => x,
            _ => panic!("expected identifier"),
        }
    }

    pub fn constant(&mut self) -> Expr<Primary> {
        Expr::Primary(match self.gettok().0 {
            Token::Number(x) =>     Primary::Number(x as isize),
            Token::Character(x) =>  Primary::Number(x as isize),
            _ => panic!("expected constant"),
        })
    }

    pub fn number(&mut self) -> Number {
        match self.gettok().0 {
            Token::Number(x) =>     x as isize,
            Token::Character(x) =>  x as isize,
            _ => panic!("expected number"),
        }
    }

    /// ```plain
    /// id-list -> id ',' id-list
    ///          | id
    /// ```
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

    /// ```plain
    /// path -> id '::' path
    ///       | id
    /// ```
    pub fn path(&mut self) -> Path {
        let mut v = Vec::new();

        loop {
            v.push(self.id());

            match *self.peek() {
                Token::DblColon => { self.gettok(); },
                _ => return Path(v)
            }
        }
    }

    /// ```plain
    /// constant-list -> constant ',' constant-list
    ///                | constant
    /// ```
    pub fn constant_list(&mut self) -> Vec<Expr<Primary>> {
        let mut v = Vec::new();

        loop {
            v.push(self.constant());

            match *self.peek() {
                Token::Comma => { self.gettok(); },
                _ => return v
            }
        }
    }

    /// ```plain
    /// file -> file' EOF
    /// ```
    pub fn file(&mut self) -> Input {
        let v = self.file_p();
        self.expect(Token::EOF);

        v
    }

    /// ```plain
    /// file' -> decl file'
    ///        | ə
    /// ```
    pub fn file_p(&mut self) -> Input {
        let mut v = Vec::new();

        loop {
            match self.decl_o() {
                Some(d)  => v.push(d),
                None => return v,
            }
        }
    }

    /// ```plain
    /// decl-list -> decl decl-list
    ///            | ə
    /// ```
    pub fn decl_list(&mut self) -> Vec<Decl> {
        let mut v = Vec::new();

        loop {
            match self.decl_o() {
                Some(x)    => v.push(x),
                _          => return v,
            }
        }
    }

    fn decl_o(&mut self) -> Option<Decl> {
        match *self.peek() {
            Token::Pub     |
            Token::Unit    |
            Token::Use     |
            Token::In      |
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

    /// ```plain
    /// decl -> decl-scope decl-body
    /// decl-scope -> pub | ə
    /// ```
    pub fn decl(&mut self) -> Decl {
        let start = self.pos();
        let is_pub = match self.gettok() {
            LexerToken(Token::Pub, _) => true,
            x => { self.untok(x); false }
        };
        let body = self.decl_body();

        Decl {
            is_pub:        is_pub,
            body:          body,
            span: Span {
                start:     start,
                end:       self.pos(),
            }
        }
    }

    /// ```plain
    /// decl-body -> unit-decl
    ///            | use-decl
    ///            | type-decl
    ///            | func-decl
    ///            | global-var-decl
    ///            | const-decl
    ///            | region-decl
    /// ```
    pub fn decl_body(&mut self) -> DeclBody {
        match *self.peek() {
            Token::Unit    => DeclBody::Unit      (self.unit_decl()),
            Token::Use      |
            Token::In      => DeclBody::Use       (self.use_decl()),
            Token::Type    => DeclBody::Type      (self.type_decl()),
            Token::Fn      => DeclBody::Func      (self.func_decl()),
            Token::Var     => DeclBody::GlobalVar (self.global_var_decl()),
            Token::Const   => DeclBody::Const     (self.const_decl()),
            Token::Region  => DeclBody::Region    (self.region_decl()),
            _ => panic!("expected declaration body!")
        }
    }

    /// ```plain
    /// unit-decl -> 'unit' id '{' decl-list '}'
    /// ```
    pub fn unit_decl(&mut self) -> UnitDecl {
        self.expect(Token::Unit);
        let name = self.id();
        self.expect(Token::LBrace);
        let decls = self.decl_list();
        self.expect(Token::RBrace);

        UnitDecl {
            name:          name,
            decls:         decls
        }
    }

    /// ```plain
    /// use-decl -> 'use' path ';'
    ///           | 'in' path 'use' id-list ';'
    ///           | 'in' path 'use' '*' ';'
    ///           | 'use' path 'as' id ';'
    /// ```
    pub fn use_decl(&mut self) -> UseDecl {
        match self.gettok().0 {
            Token::Use => {
                let p = self.path();
                match self.gettok().0 {
                    Token::Semicolon =>
                        UseDecl::Single(p),
                    Token::As => {
                        let x = self.id();
                        self.expect(Token::Semicolon);
                        UseDecl::Aliased(p, x)
                    },
                    _ => panic!("expected ';' or 'as'")
                }
            },

            Token::In => {
                let p = self.path();
                self.expect(Token::Use);
                let x = match *self.peek() {
                    Token::Identifier(_) =>
                        UseDecl::Many(p, self.id_list()),
                    Token::Star => {
                        self.gettok();
                        UseDecl::Glob(p)
                    }
                    _ => panic!("expected identifier or '*'")
                };
                self.expect(Token::Semicolon);
                x
            },

            _ => panic!("expected 'use' declaration")
        }
    }

    /// ```plain
    /// type-decl -> 'type' id ':' type-spec ';'
    /// ```
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

    /// ```plain
    /// type-spec -> path
    ///            | id '<' constant-list '>'
    ///            | '*' type-spec
    ///            | '[' number ']' type-spec
    ///            | struct-spec
    ///            | bitvec-spec
    /// ```
    pub fn type_spec(&mut self) -> TypeSpec {
        let tok = self.gettok();
        match tok.0 {
            Token::Identifier(_) => {
                self.untok(tok);
                let p = self.path();

                match self.gettok() {
                    LexerToken(Token::Less, _) => {
                        let x = TypeSpec::Parameterized(p, self.constant_list());
                        self.expect(Token::Greater);
                        x
                    },

                    x => {
                        self.untok(x);
                        TypeSpec::Alias(p)
                    }
                }
            },

            Token::Star => {
                TypeSpec::Pointer(Box::new(self.type_spec()))
            },

            Token::LBrack => {
                let x = self.number();
                self.expect(Token::RBrack);
                TypeSpec::Array(x, Box::new(self.type_spec()))
            },

            Token::Struct => {
                self.untok(tok);
                self.struct_spec()
            },

            Token::Bitvec => {
                self.untok(tok);
                self.bitvec_spec()
            },

            _ => panic!("expected type specifier"),
        }
    }

    /// ```plain
    /// struct-spec -> 'struct' '{' struct-body '}'
    /// struct-body -> var-decl struct-body
    ///              | ə
    /// ```
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

    /// ```plain
    /// bitvec-spec -> 'bitvec' bitvec-size '(' bitvec-body ')'
    /// bitvec-size -> '<' number '>' | ə
    /// bitvec-body -> bitvec-member ',' bitvec-body
    ///              | bitvec-member
    /// ```
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
                LexerToken(Token::Comma, _) => { },
                x => { self.untok(x); break },
            }
        }

        self.expect(Token::RParen);

        TypeSpec::Bitvec(size, body)
    }

    /// ```plain
    /// bitvec-member -> number ':' number
    ///                | id ':' number
    /// ```
    pub fn bitvec_member(&mut self) -> BitvecMember {
        match self.gettok().0 {
            Token::Identifier(x) => {
                self.expect(Token::Colon);
                BitvecMember::Variable(x, self.number())
            },

            Token::Number(x) => {
                self.expect(Token::Colon);
                BitvecMember::Literal(x, self.number())
            },

            _ => panic!("expected bitvec member"),

        }
    }

    fn global_var_decl_o(&mut self) -> Option<GlobalVarDecl> {
        match *self.peek() {
            Token::Var  => Some(self.global_var_decl()),
            _           => None,
        }
    }

    /// ```plain
    /// global-var-decl -> 'var' storage var-decl
    /// ```
    pub fn global_var_decl(&mut self) -> GlobalVarDecl {
        self.expect(Token::Var);

        GlobalVarDecl {
            storage:   self.storage(),
            decl:      self.var_decl(),
        }
    }

    /// ```plain
    /// storage -> storage-loc storage-params
    /// ```
    pub fn storage(&mut self) -> Storage {
        Storage {
            loc:       self.storage_loc(),
            params:    self.storage_params(),
        }
    }

    /// ```plain
    /// storage-loc -> ram | rom | ə
    /// ```
    pub fn storage_loc(&mut self) -> StorageLoc {
        match self.gettok() {
            LexerToken(Token::Ram, _) => StorageLoc::RAM,
            LexerToken(Token::Rom, _) => StorageLoc::ROM,
            x => { self.untok(x); StorageLoc::Default },
        }
    }

    /// ```plain
    /// storage-params -> storage-param storage-params
    ///                 | ə
    /// ```
    pub fn storage_params(&mut self) -> Vec<StorageParam> {
        let mut params = Vec::new();

        loop {
            match self.storage_param_o() {
                Some(x)  => params.push(x),
                None     => return params
            }
        }
    }

    /// ```plain
    /// storage-param -> region-name
    /// ```
    fn storage_param_o(&mut self) -> Option<StorageParam> {
        match *self.peek() {
            Token::Region  => Some(StorageParam::Region(self.region_name())),
            _              => None,
        }
    }

    fn var_decl_o(&mut self) -> Option<VarDecl> {
        match *self.peek() {
            Token::Identifier(_)  => Some(self.var_decl()),
            _                     => None
        }
    }

    /// ```plain
    /// var-decl -> id-list ':' type-spec ';'
    ///           | id ':' type-spec '=' expr ';'
    /// ```
    pub fn var_decl(&mut self) -> VarDecl {
        let ids = self.id_list();
        self.expect(Token::Colon);
        let typ = self.type_spec();
        let init = match self.gettok().0 {
            Token::Semicolon => None,
            Token::Assign => {
                let x = Some(self.expr());
                self.expect(Token::Semicolon);
                x
            },
            x => panic!("expected = or ; got {:?}", x)
        };

        VarDecl {
            ids:       ids,
            typ:       typ,
            init:      init,
        }
    }

    /// ```plain
    /// const-decl -> 'const' id ':' type-spec '=' constant ';'
    /// ```
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

    /// ```plain
    /// region-decl -> region-name '{' region-decl-body '}'
    /// ```
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

    /// ```plain
    /// region-name -> 'region' '(' id ',' id ')'
    /// ```
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

    /// ```plain
    /// region-decl-body -> global-var-decl region-decl-body | ə
    /// ```
    pub fn region_decl_body(&mut self) -> Vec<GlobalVarDecl> {
        let mut body = Vec::new();

        loop {
            match self.global_var_decl_o() {
                Some(x)  => body.push(x),
                None     => return body,
            }
        }
    }

    /// ```plain
    /// func-decl -> func-heading '{' func-body '}'
    /// func-heading -> 'fn' id '(' func-params ')' func-return
    /// ```
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
            name:   name,
            params: params,
            ret:    ret,
            body:   body
        }
    }

    /// ```plain
    /// func-return -> ':' type-spec | ə
    /// ```
    pub fn func_return(&mut self) -> Option<TypeSpec> {
        match self.gettok() {
            LexerToken(Token::Colon, _) => Some(self.type_spec()),
            x => { self.untok(x); None }
        }
    }

    /// ```plain
    /// func-params -> func-params' | ə
    /// func-params' -> func-param ',' func-params'
    ///               | func-param
    /// ```
    pub fn func_params(&mut self) -> Vec<FuncParam> {
        let mut params = Vec::new();

        loop {
            match self.func_param_o() {
                Some(x)  => params.push(x),
                None     => return params
            }

            match self.gettok() {
                LexerToken(Token::Comma, _) => { },
                x => { self.untok(x); return params; }
            }
        }
    }

    fn func_param_o(&mut self) -> Option<FuncParam> {
        match *self.peek() {
            Token::Identifier(_) => Some(self.func_param()),
            _                    => None,
        }
    }

    /// ```plain
    /// func-param -> id-list ':' type-spec
    /// ```
    pub fn func_param(&mut self) -> FuncParam {
        let ids = self.id_list();
        self.expect(Token::Colon);

        FuncParam {
            ids:       ids,
            typ:       self.type_spec(),
        }
    }

    /// ```plain
    /// stmt-list -> stmt stmt-list | ə
    /// ```
    pub fn stmt_list(&mut self) -> Stmt {
        let start = self.pos();
        let mut stmts = Vec::new();

        loop {
            match self.stmt_o() {
                Some(x)  => stmts.push(x),
                None =>
                    return Stmt {
                        body: StmtBody::Compound(stmts),
                        span: Span {
                            start: start,
                            end:   self.pos()
                        }
                    },
            }
        }
    }

    fn stmt_o(&mut self) -> Option<Stmt> {
        let tok = self.gettok();

        match tok.0 {
            Token::LBrace => {
                let st = self.stmt_list();
                self.expect(Token::RBrace);
                Some(st)
            },

            Token::Var      =>
                Some(Stmt {
                    body: StmtBody::Var(self.var_decl()),
                    span: Span {
                        start: tok.1,
                        end:   self.pos()
                    }
                }),

            Token::If       => { self.untok(tok); Some(self.if_stmt()) },
            Token::Switch   => { self.untok(tok); Some(self.switch_stmt()) },
            Token::Loop     => { self.untok(tok); Some(self.loop_stmt()) },
            Token::While    => { self.untok(tok); Some(self.while_stmt()) },
            Token::For      => { self.untok(tok); Some(self.for_stmt()) },

            Token::Break    => { self.stmt_simple_o(tok.1, StmtBody::Break) },
            Token::Continue => { self.stmt_simple_o(tok.1, StmtBody::Continue) },
            Token::Repeat   => { self.stmt_simple_o(tok.1, StmtBody::Repeat) },

            Token::Return => {
                match self.gettok() {
                    LexerToken(Token::Semicolon, end) =>
                        Some(Stmt {
                            body: StmtBody::Return(None),
                            span: Span {
                                start: tok.1,
                                end:   end,
                            }
                        }),
                    x => {
                        self.untok(x);
                        let ex = self.expr();
                        self.stmt_simple_o(tok.1, StmtBody::Return(Some(ex)))
                    }
                }
            },

            // everything an expr can start with. KEEP UPDATED!
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
                let start = tok.1;
                self.untok(tok);
                let ex = self.expr();
                self.stmt_simple_o(start, StmtBody::Eval(ex))
            },

            _ => { self.untok(tok); None }
        }
    }

    /// ```plain
    /// stmt -> expr ';'
    ///       | '{' stmt-list '}'
    ///       | 'var' var-decl
    ///       | if-stmt
    ///       | switch-stmt
    ///       | loop-stmt
    ///       | while-stmt
    ///       | for-stmt
    ///       | 'break' ';'
    ///       | 'continue' ';'
    ///       | 'repeat' ';'
    ///       | 'return' ';'
    ///       | 'return' expr ';'
    /// ```
    pub fn stmt(&mut self) -> Stmt {
        match self.stmt_o() {
            Some(x)  => x,
            None     => panic!("expected statement")
        }
    }

    fn stmt_simple_o(&mut self, p: Position, body: StmtBody) -> Option<Stmt> {
        self.expect(Token::Semicolon);
        Some(Stmt {
            body: body,
            span: Span {
                start: p,
                end:   self.pos()
            }
        })
    }

    /// ```plain
    /// if-stmt -> if '(' expr ')' stmt
    ///          | if '(' expr ')' stmt 'else' stmt
    /// ```
    pub fn if_stmt(&mut self) -> Stmt {
        let start = self.pos();
        self.expect(Token::If);
        self.expect(Token::LParen);
        let cond = self.expr();
        self.expect(Token::RParen);
        let tb = Box::new(self.stmt());
        let fb = match self.gettok() {
            LexerToken(Token::Else, _) => Some(Box::new(self.stmt())),
            x => { self.untok(x); None },
        };

        Stmt {
            body: StmtBody::If(IfStmt {
                cond: cond,
                tb:   tb,
                fb:   fb,
            }),
            span: Span {
                start: start,
                end:   self.pos()
            }
        }
    }

    /// ```plain
    /// switch-stmt -> 'switch' '(' expr ')' '{' switch-body '}'
    /// ```
    pub fn switch_stmt(&mut self) -> Stmt {
        let start = self.pos();
        self.expect(Token::Switch);
        self.expect(Token::LParen);
        let ex = self.expr();
        self.expect(Token::RParen);
        self.expect(Token::LBrace);
        let body = self.switch_body();
        self.expect(Token::RBrace);

        Stmt {
            body: StmtBody::Switch(SwitchStmt {
                ex:    ex,
                cases: body,
            }),
            span: Span {
                start: start,
                end:   self.pos()
            }
        }
    }

    /// ```plain
    /// switch-body -> switch-case switch-body | ə
    /// ```
    pub fn switch_body(&mut self) -> Vec<SwitchCase> {
        let mut body = Vec::new();

        loop {
            match self.switch_case_o() {
                Some(x)  => body.push(x),
                None     => return body
            }
        }
    }

    fn switch_case_o(&mut self) -> Option<SwitchCase> {
        match *self.peek() {
            Token::Case | Token::Default => Some(self.switch_case()),
            _ => None,
        }
    }

    /// ```plain
    /// switch-case -> 'case' const ':' stmt-list
    ///              | 'default' ':' stmt-list
    /// ```
    pub fn switch_case(&mut self) -> SwitchCase {
        match self.gettok().0 {
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

    /// ```plain
    /// loop-stmt -> 'loop' stmt
    /// ```
    pub fn loop_stmt(&mut self) -> Stmt {
        let start = self.pos();
        self.expect(Token::Loop);

        let body = StmtBody::Loop(LoopStmt {
            body: Box::new(self.stmt())
        });
        Stmt {
            body: body,
            span: Span {
                start: start,
                end:   self.pos()
            }
        }
    }

    /// ```plain
    /// while-stmt -> 'while' '(' expr ')' stmt
    /// ```
    pub fn while_stmt(&mut self) -> Stmt {
        let start = self.pos();
        self.expect(Token::While);
        self.expect(Token::LParen);
        let cond = self.expr();
        self.expect(Token::RParen);

        let body = StmtBody::While(WhileStmt {
            cond: cond,
            body: Box::new(self.stmt()),
        });
        Stmt {
            body: body,
            span: Span {
                start: start,
                end:   self.pos()
            }
        }
    }

    /// ```plain
    /// for-stmt -> 'for' '(' id 'in' expr ')' stmt
    /// ```
    pub fn for_stmt(&mut self) -> Stmt {
        let start = self.pos();
        self.expect(Token::For);
        self.expect(Token::LParen);
        let id = self.id();
        self.expect(Token::In);
        let iter = self.expr();
        self.expect(Token::RParen);

        let body = StmtBody::For(ForStmt {
            id:   id,
            iter: iter,
            body: Box::new(self.stmt()),
        });
        Stmt {
            body: body,
            span: Span {
                start: start,
                end:   self.pos()
            }
        }
    }

    /// ```plain
    /// expr -> ex-comma
    /// ```
    pub fn expr(&mut self) -> Expr<Primary> {
        self.ex_comma()
    }

    /// ```plain
    /// ex-list -> ex-list' | ə
    /// ex-list' -> ex-assign ',' ex-list'
    ///           | ex-assign
    /// ```
    pub fn ex_list(&mut self) -> Vec<Expr<Primary>> {
        let mut list = Vec::new();

        loop {
            // this is awful, it's everything that can follow an ex-list,
            // and it stops if it sees that
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

    /// ```plain
    /// ex-comma -> ex-assign
    ///           | ex-assign , ex-comma
    /// ```
    pub fn ex_comma(&mut self) -> Expr<Primary> {
        let left = self.ex_assign();

        match *self.peek() {
            Token::Comma => { },
            _ => { return left }
        }

        let mut vec = Vec::new();
        vec.push(left);

        loop {
            match self.gettok() {
                LexerToken(Token::Comma, _) => vec.push(self.ex_assign()),
                x => { self.untok(x); return Expr::Comma(vec) }
            }
        }
    }

    /// ```plain
    /// ex-assign -> ex-tern
    ///            | ex-tern '=' ex-assign
    ///            | ex-tern '+=' ex-assign
    ///            | ex-tern '-=' ex-assign
    ///            | ex-tern '*=' ex-assign
    ///            | ex-tern '/=' ex-assign
    ///            | ex-tern '%=' ex-assign
    ///            | ex-tern '<<=' ex-assign
    ///            | ex-tern '>>=' ex-assign
    ///            | ex-tern '&=' ex-assign
    ///            | ex-tern '^=' ex-assign
    ///            | ex-tern '|=' ex-assign
    /// ```
    pub fn ex_assign(&mut self) -> Expr<Primary> {
        let left = self.ex_tern();

        let op = match *self.peek() {
            Token::Assign    => None,
            Token::PlusEq    => Some(BinOp::Add),
            Token::MinusEq   => Some(BinOp::Sub),
            Token::StarEq    => Some(BinOp::Mul),
            Token::SlashEq   => Some(BinOp::Div),
            Token::ModEq     => Some(BinOp::Mod),
            Token::LShiftEq  => Some(BinOp::LShift),
            Token::RShiftEq  => Some(BinOp::RShift),
            Token::BitAndEq  => Some(BinOp::BitAnd),
            Token::BitXorEq  => Some(BinOp::BitXor),
            Token::BitOrEq   => Some(BinOp::BitOr),
            _ => return left,
        };

        self.gettok();

        Expr::Assign(
            op,
            Box::new(left),
            Box::new(self.ex_assign()),
        )
    }

    /// ```plain
    /// ex-tern -> ex-lor
    ///          | ex-lor '?' ex-lor ':' ex-tern
    /// ```
    pub fn ex_tern(&mut self) -> Expr<Primary> {
        let left = self.ex_lor();

        match *self.peek() {
            Token::Question => { },
            _ => return left,
        }

        self.expect(Token::Question);
        let mid = self.ex_lor();
        self.expect(Token::Colon);

        Expr::Ternary(
            Box::new(left),
            Box::new(mid),
            Box::new(self.ex_tern())
        )
    }

    /// ```plain
    /// ex-lor -> ex-land
    ///         | ex-lor '||' ex-land
    /// ```
    pub fn ex_lor(&mut self) -> Expr<Primary> {
        let mut left = self.ex_land();

        loop {
            let tok = self.gettok();
            match tok.0 {
                Token::DblPipe =>
                    left = Expr::Binary(
                        BinOp::BoolOr,
                        Box::new(left),
                        Box::new(self.ex_land())
                    ),
                _ =>
                    { self.untok(tok); return left }
            }
        }
    }

    /// ```plain
    /// ex-land -> ex-bor
    ///          | ex-land '&&' ex-bor
    /// ```
    pub fn ex_land(&mut self) -> Expr<Primary> {
        let mut left = self.ex_bor();

        loop {
            let tok = self.gettok();
            match tok.0 {
                Token::DblAmp =>
                    left = Expr::Binary(
                        BinOp::BoolAnd,
                        Box::new(left),
                        Box::new(self.ex_bor())
                    ),
                _ =>
                    { self.untok(tok); return left }
            }
        }
    }

    /// ```plain
    /// ex-bor -> ex-bxor
    ///         | ex-bor '|' ex-bxor
    /// ```
    pub fn ex_bor(&mut self) -> Expr<Primary> {
        let mut left = self.ex_bxor();

        loop {
            let tok = self.gettok();
            match tok.0 {
                Token::Pipe =>
                    left = Expr::Binary(
                        BinOp::BitOr,
                        Box::new(left),
                        Box::new(self.ex_bxor())
                    ),
                _ =>
                    { self.untok(tok); return left }
            }
        }
    }

    /// ```plain
    /// ex-bxor -> ex-band
    ///          | ex-bxor '^' ex-band
    /// ```
    pub fn ex_bxor(&mut self) -> Expr<Primary> {
        let mut left = self.ex_band();

        loop {
            let tok = self.gettok();
            match tok.0 {
                Token::Caret =>
                    left = Expr::Binary(
                        BinOp::BitXor,
                        Box::new(left),
                        Box::new(self.ex_band())
                    ),
                _ =>
                    { self.untok(tok); return left }
            }
        }
    }

    /// ```plain
    /// ex-band -> ex-eq
    ///          | ex-band '&' ex-eq
    /// ```
    pub fn ex_band(&mut self) -> Expr<Primary> {
        let mut left = self.ex_eq();

        loop {
            let tok = self.gettok();
            match tok.0 {
                Token::Amp =>
                    left = Expr::Binary(
                        BinOp::BitAnd,
                        Box::new(left),
                        Box::new(self.ex_eq())
                    ),
                _ =>
                    { self.untok(tok); return left }
            }
        }
    }

    /// ```plain
    /// ex-eq -> ex-cmp
    ///        | ex-eq '==' ex-cmp
    ///        | ex-eq '!=' ex-cmp
    /// ```
    pub fn ex_eq(&mut self) -> Expr<Primary> {
        let mut left = self.ex_cmp();

        loop {
            let tok = self.gettok();
            match tok.0 {
                Token::Eq =>
                    left = Expr::Binary(
                        BinOp::Eq,
                        Box::new(left),
                        Box::new(self.ex_cmp())
                    ),
                Token::NotEq =>
                    left = Expr::Binary(
                        BinOp::NotEq,
                        Box::new(left),
                        Box::new(self.ex_cmp())
                    ),

                _ =>
                    { self.untok(tok); return left }
            }
        }
    }

    /// ```plain
    /// ex-cmp -> ex-shift
    ///         | ex-cmp '<' ex-shift
    ///         | ex-cmp '>' ex-shift
    ///         | ex-cmp '<=' ex-shift
    ///         | ex-cmp '>=' ex-shift
    /// ```
    pub fn ex_cmp(&mut self) -> Expr<Primary> {
        let mut left = self.ex_shift();

        loop {
            let tok = self.gettok();
            match tok.0 {
                Token::Less =>
                    left = Expr::Binary(
                        BinOp::Less,
                        Box::new(left),
                        Box::new(self.ex_shift())
                    ),
                Token::Greater =>
                    left = Expr::Binary(
                        BinOp::Greater,
                        Box::new(left),
                        Box::new(self.ex_shift())
                    ),
                Token::LessEq =>
                    left = Expr::Binary(
                        BinOp::LessEq,
                        Box::new(left),
                        Box::new(self.ex_shift())
                    ),
                Token::GreaterEq =>
                    left = Expr::Binary(
                        BinOp::GreaterEq,
                        Box::new(left),
                        Box::new(self.ex_shift())
                    ),

                _ =>
                    { self.untok(tok); return left }
            }
        }
    }

    /// ```plain
    /// ex-shift -> ex-add
    ///           | ex-shift '<<' ex-add
    ///           | ex-shift '>>' ex-add
    /// ```
    pub fn ex_shift(&mut self) -> Expr<Primary> {
        let mut left = self.ex_add();

        loop {
            let tok = self.gettok();
            match tok.0 {
                Token::LShift =>
                    left = Expr::Binary(
                        BinOp::LShift,
                        Box::new(left),
                        Box::new(self.ex_add())
                    ),
                Token::RShift =>
                    left = Expr::Binary(
                        BinOp::RShift,
                        Box::new(left),
                        Box::new(self.ex_add())
                    ),

                _ =>
                    { self.untok(tok); return left }
            }
        }
    }

    /// ```plain
    /// ex-add -> ex-mul
    ///         | ex-add '+' ex-mul
    ///         | ex-add '-' ex-mul
    /// ```
    pub fn ex_add(&mut self) -> Expr<Primary> {
        let mut left = self.ex_mul();

        loop {
            let tok = self.gettok();
            match tok.0 {
                Token::Plus =>
                    left = Expr::Binary(
                        BinOp::Add,
                        Box::new(left),
                        Box::new(self.ex_mul())
                    ),
                Token::Minus =>
                    left = Expr::Binary(
                        BinOp::Sub,
                        Box::new(left),
                        Box::new(self.ex_mul())
                    ),

                _ =>
                    { self.untok(tok); return left }
            }
        }
    }

    /// ```plain
    /// ex-mul -> ex-unary
    ///         | ex-mul '*' ex-unary
    ///         | ex-mul '/' ex-unary
    ///         | ex-mul '%' ex-unary
    /// ```
    pub fn ex_mul(&mut self) -> Expr<Primary> {
        let mut left = self.ex_unary();

        loop {
            let tok = self.gettok();
            match tok.0 {
                Token::Star =>
                    left = Expr::Binary(
                        BinOp::Mul,
                        Box::new(left),
                        Box::new(self.ex_unary())
                    ),
                Token::Slash =>
                    left = Expr::Binary(
                        BinOp::Div,
                        Box::new(left),
                        Box::new(self.ex_unary())
                    ),
                Token::Mod =>
                    left = Expr::Binary(
                        BinOp::Mod,
                        Box::new(left),
                        Box::new(self.ex_unary())
                    ),

                _ =>
                    { self.untok(tok); return left }
            }
        }
    }

    /// ```plain
    /// ex-unary -> '++' ex-unary
    ///           | '--' ex-unary
    ///           | '!' ex-unary
    ///           | '~' ex-unary
    ///           | '*' ex-unary
    ///           | '&' ex-unary
    ///           | 'sizeof' ex-unary
    ///           | ex-bottom
    /// ```
    pub fn ex_unary(&mut self) -> Expr<Primary> {
        let tok = self.gettok();

        match tok.0 {
            Token::Incr =>
                Expr::Unary(UnOp::PreIncr, Box::new(self.ex_unary())),
            Token::Decr =>
                Expr::Unary(UnOp::PreDecr, Box::new(self.ex_unary())),
            Token::Excl =>
                Expr::Unary(UnOp::BoolNot, Box::new(self.ex_unary())),
            Token::Tilde =>
                Expr::Unary(UnOp::BitNot, Box::new(self.ex_unary())),
            Token::Star =>
                Expr::Unary(UnOp::Deref, Box::new(self.ex_unary())),
            Token::Amp =>
                Expr::Unary(UnOp::AddrOf, Box::new(self.ex_unary())),
            Token::Sizeof =>
                Expr::Unary(UnOp::SizeOf, Box::new(self.ex_unary())),

            _ =>
                { self.untok(tok); self.ex_bottom() }
        }
    }

    /// ```plain
    /// ex-bottom -> ex-primary
    ///            | ex-primary '++'
    ///            | ex-primary '--'
    ///            | ex-primary '(' ex-list ')'
    ///            | ex-primary '.' ex-bottom
    /// ```
    pub fn ex_bottom(&mut self) -> Expr<Primary> {
        let mut left = self.ex_primary();

        loop {
            let tok = self.gettok();

            match tok.0 {
                Token::Incr =>
                    return Expr::Unary(UnOp::PostIncr, Box::new(left)),
                Token::Decr =>
                    return Expr::Unary(UnOp::PostDecr, Box::new(left)),
                Token::LParen => {
                    left = Expr::Call(Box::new(left), self.ex_list());
                    self.expect(Token::RParen);
                },
                Token::LBrack => {
                    left = Expr::Binary(
                        BinOp::Element,
                        Box::new(left),
                        Box::new(self.expr())
                    );
                    self.expect(Token::RBrack);
                },
                Token::Dot =>
                    left = Expr::Member(Box::new(left), self.id()),

                _ =>
                    { self.untok(tok); return left }
            }
        }
    }

    /// ```plain
    /// ex-primary -> path
    ///            -> constant
    ///            -> '(' expr ')'
    /// ```
    pub fn ex_primary(&mut self) -> Expr<Primary> {
        let tok = self.gettok();

        match tok.0 {
            Token::Identifier(_) => {
                self.untok(tok);
                Expr::Primary(Primary::Path(self.path()))
            }
            Token::Number(x) =>
                Expr::Primary(Primary::Number(x as isize)),
            Token::Character(x) =>
                Expr::Primary(Primary::Number(x as isize)),
            Token::LParen => {
                let ex = self.expr();
                self.expect(Token::RParen);
                ex
            }

            _ => { panic!("expected expression") }
        }
    }
}
