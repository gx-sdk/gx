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
use msg;

use expr::*;

use std::io;
use std::result;

/// An instance of a parser. If you have an `Iterator<Token>` go ahead and
/// create one with `Parser::new`. The resulting `Parser` instance is full
/// of functions for parsing the different non-terminals in the grammar, but
/// the most relevant is probably `Parser::file`, corresponding to the start
/// symbol.
pub struct Parser<It> {
    input: Lexer<It>,
    ungot: Vec<LexerToken>,
}

pub type Result<T> = result::Result<T, msg::Message>;

impl<It: Iterator<Item = result::Result<char, io::CharsError>>> Parser<It> {
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

    fn span_to_here(&mut self, start: Position) -> Span {
        Span {
            start:  start,
            end:    self.pos()
        }
    }

    fn error(&mut self, p: Position, msg: String) -> msg::Message {
        msg::Message {
            kind:   msg::MessageKind::Error,
            msg:    msg,
            start:  Some(msg::Position {
                file:  self.input.file.clone(),
                line:  p.line,
                col:   p.col
            }),
            end:    None
        }
    }

    fn error_here(&mut self, msg: &str) -> msg::Message {
        let here = self.pos();
        self.error(here, String::from_str(msg))
    }

    fn untok(&mut self, t: LexerToken) {
        self.ungot.push(t);
    }

    fn gettok(&mut self) -> Result<LexerToken> {
        match self.ungot.pop() {
            Some(t) => Ok(t),
            None => match self.input.next() {
                Some(t) => Ok(t),
                None => Err(self.error_here("unexpected end of file")),
            }
        }
    }

    fn expect(&mut self, t: Token) -> Result<LexerToken> {
        let t_ = try!(self.gettok());

        if t != t_.0 {
            Err(self.error(t_.1, format!("expected {:?}, got {:?}", t, t_.0)))
        } else {
            Ok(t_)
        }
    }

    fn peek(&mut self) -> Result<&Token> {
        let t = try!(self.gettok());
        self.untok(t);
        return Ok(&self.ungot[self.ungot.len() - 1].0);
    }

    pub fn id(&mut self) -> Result<Id> {
        let tok = try!(self.gettok());
        match tok.0 {
            Token::Identifier(x) => Ok(x),
            _ => Err(self.error(tok.1, format!("expected identifier"))),
        }
    }

    pub fn constant(&mut self) -> Result<Expr<Primary>> {
        let tok = try!(self.gettok());
        Ok(Expr::Primary(match tok.0 {
            Token::Number(x) =>     Primary::Number(x as isize),
            Token::Character(x) =>  Primary::Number(x as isize),
            _ => return Err(self.error(tok.1, format!("expected constant"))),
        }))
    }

    pub fn number(&mut self) -> Result<Number> {
        let tok = try!(self.gettok());
        Ok(match tok.0 {
            Token::Number(x) =>     x as isize,
            Token::Character(x) =>  x as isize,
            _ => return Err(self.error(tok.1, format!("expected number"))),
        })
    }

    /// ```plain
    /// id-list -> id ',' id-list
    ///          | id
    /// ```
    pub fn id_list(&mut self) -> Result<Vec<Id>> {
        let mut v = Vec::new();

        loop {
            v.push(try!(self.id()));

            match *try!(self.peek()) {
                Token::Comma => { try!(self.gettok()); },
                _ => return Ok(v)
            }
        }
    }

    /// ```plain
    /// path -> id '::' path
    ///       | id
    /// ```
    pub fn path(&mut self) -> Result<Path> {
        let mut v = Vec::new();

        loop {
            v.push(try!(self.id()));

            match *try!(self.peek()) {
                Token::DblColon => { try!(self.gettok()); },
                _ => return Ok(Path(v))
            }
        }
    }

    /// ```plain
    /// constant-list -> constant ',' constant-list
    ///                | constant
    /// ```
    pub fn constant_list(&mut self) -> Result<Vec<Expr<Primary>>> {
        let mut v = Vec::new();

        loop {
            v.push(try!(self.constant()));

            match *try!(self.peek()) {
                Token::Comma => { try!(self.gettok()); },
                _ => return Ok(v)
            }
        }
    }

    /// ```plain
    /// file -> file' EOF
    /// ```
    pub fn file(&mut self) -> Result<Input> {
        let v = try!(self.file_p());
        try!(self.expect(Token::EOF));

        Ok(v)
    }

    /// ```plain
    /// file' -> decl file'
    ///        | ə
    /// ```
    pub fn file_p(&mut self) -> Result<Input> {
        let mut v = Vec::new();

        loop {
            match try!(self.decl_o()) {
                Some(d) => v.push(d),
                None => return Ok(v),
            }
        }
    }

    /// ```plain
    /// decl-list -> decl decl-list
    ///            | ə
    /// ```
    pub fn decl_list(&mut self) -> Result<Vec<Decl>> {
        let mut v = Vec::new();

        loop {
            match try!(self.decl_o()) {
                Some(x) => v.push(x),
                _ => return Ok(v),
            }
        }
    }

    fn decl_o(&mut self) -> Result<Option<Decl>> {
        match *try!(self.peek()) {
            Token::Pub     |
            Token::Unit    |
            Token::Use     |
            Token::In      |
            Token::Type    |
            Token::Fn      |
            Token::Var     |
            Token::Const   |
            Token::Region  =>
                Ok(Some(try!(self.decl()))),
            _ =>
                Ok(None)
        }
    }

    /// ```plain
    /// decl -> decl-scope decl-body
    /// decl-scope -> pub | ə
    /// ```
    pub fn decl(&mut self) -> Result<Decl> {
        let start = self.pos();
        let is_pub = match try!(self.gettok()) {
            LexerToken(Token::Pub, _) => true,
            x => { self.untok(x); false }
        };
        let body = try!(self.decl_body());

        Ok(Decl {
            is_pub:  is_pub,
            body:    body,
            span:    self.span_to_here(start),
        })
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
    pub fn decl_body(&mut self) -> Result<DeclBody> {
        Ok(match *try!(self.peek()) {
            Token::Unit    => DeclBody::Unit      (try!(self.unit_decl())),
            Token::Use      |
            Token::In      => DeclBody::Use       (try!(self.use_decl())),
            Token::Type    => DeclBody::Type      (try!(self.type_decl())),
            Token::Fn      => DeclBody::Func      (try!(self.func_decl())),
            Token::Var     => DeclBody::GlobalVar (try!(self.global_var_decl())),
            Token::Const   => DeclBody::Const     (try!(self.const_decl())),
            Token::Region  => DeclBody::Region    (try!(self.region_decl())),
            _ => return Err(self.error_here("expected declaration")),
        })
    }

    /// ```plain
    /// unit-decl -> 'unit' id '{' decl-list '}'
    /// ```
    pub fn unit_decl(&mut self) -> Result<UnitDecl> {
        try!(self.expect(Token::Unit));
        let name = try!(self.id());
        try!(self.expect(Token::LBrace));
        let decls = try!(self.decl_list());
        try!(self.expect(Token::RBrace));

        Ok(UnitDecl {
            name:          name,
            decls:         decls
        })
    }

    /// ```plain
    /// use-decl -> 'use' path ';'
    ///           | 'in' path 'use' id-list ';'
    ///           | 'in' path 'use' '*' ';'
    ///           | 'use' path 'as' id ';'
    /// ```
    pub fn use_decl(&mut self) -> Result<UseDecl> {
        let tok = try!(self.gettok());
        match tok.0 {
            Token::Use => {
                let p = try!(self.path());
                Ok(match try!(self.gettok()).0 {
                    Token::Semicolon =>
                        UseDecl::Single(p),
                    Token::As => {
                        let x = try!(self.id());
                        try!(self.expect(Token::Semicolon));
                        UseDecl::Aliased(p, x)
                    },
                    _ => return Err(self.error_here("expected ';' or 'as'")),
                })
            },

            Token::In => {
                let p = try!(self.path());
                try!(self.expect(Token::Use));
                let x = match *try!(self.peek()) {
                    Token::Identifier(_) =>
                        UseDecl::Many(p, try!(self.id_list())),
                    Token::Star => {
                        try!(self.gettok());
                        UseDecl::Glob(p)
                    }
                    _ => return Err(self.error_here("expected identifier or '*'"))
                };
                try!(self.expect(Token::Semicolon));
                Ok(x)
            },

            _ => return Err(self.error(tok.1,
                    format!("expected 'use' declaration")))
        }
    }

    /// ```plain
    /// type-decl -> 'type' id ':' type-spec ';'
    /// ```
    pub fn type_decl(&mut self) -> Result<TypeDecl> {
        try!(self.expect(Token::Type));
        let name = try!(self.id());
        try!(self.expect(Token::Colon));
        let typ = try!(self.type_spec());
        try!(self.expect(Token::Semicolon));

        Ok(TypeDecl {
            name:          name,
            typ:           typ,
        })
    }

    /// ```plain
    /// type-spec -> path
    ///            | id '<' constant-list '>'
    ///            | '*' type-spec
    ///            | '[' number ']' type-spec
    ///            | struct-spec
    ///            | bitvec-spec
    /// ```
    pub fn type_spec(&mut self) -> Result<TypeSpec> {
        let tok = try!(self.gettok());
        let start = tok.1;
        match tok.0 {
            Token::Identifier(_) => {
                self.untok(tok);
                let p = try!(self.path());

                match try!(self.gettok()) {
                    LexerToken(Token::Less, _) => {
                        let x = TypeBody::Parameterized(p,
                            try!(self.constant_list())
                            );
                        try!(self.expect(Token::Greater));
                        Ok(TypeSpec {
                            body: x,
                            span: self.span_to_here(start),
                        })
                    },

                    x => {
                        self.untok(x);
                        Ok(TypeSpec {
                            body: TypeBody::Alias(p),
                            span: self.span_to_here(start),
                        })
                    }
                }
            },

            Token::Star => {
                let body = TypeBody::Pointer(Box::new(try!(self.type_spec())));
                Ok(TypeSpec {
                    body: body,
                    span: self.span_to_here(start),
                })
            },

            Token::LBrack => {
                let x = try!(self.number());
                try!(self.expect(Token::RBrack));
                let body = TypeBody::Array(x, Box::new(try!(self.type_spec())));
                Ok(TypeSpec {
                    body: body,
                    span: self.span_to_here(start),
                })
            },

            Token::Struct => {
                self.untok(tok);
                self.struct_spec()
            },

            Token::Bitvec => {
                self.untok(tok);
                self.bitvec_spec()
            },

            _ => Err(self.error(tok.1, format!("expected type specifier"))),
        }
    }

    /// ```plain
    /// struct-spec -> 'struct' '{' struct-body '}'
    /// struct-body -> var-decl struct-body
    ///              | ə
    /// ```
    pub fn struct_spec(&mut self) -> Result<TypeSpec> {
        let start = self.pos();

        try!(self.expect(Token::Struct));
        try!(self.expect(Token::LBrace));

        let mut body = Vec::new();

        loop {
            match try!(self.var_decl_o()) {
                Some(x) => body.push(x),
                None    => break,
            }
        }

        try!(self.expect(Token::RBrace));

        Ok(TypeSpec {
            body: TypeBody::Struct(body),
            span: self.span_to_here(start),
        })
    }

    /// ```plain
    /// bitvec-spec -> 'bitvec' bitvec-size '(' bitvec-body ')'
    /// bitvec-size -> '<' number '>' | ə
    /// bitvec-body -> bitvec-member ',' bitvec-body
    ///              | bitvec-member
    /// ```
    pub fn bitvec_spec(&mut self) -> Result<TypeSpec> {
        let start = self.pos();

        try!(self.expect(Token::Bitvec));

        let size = match *try!(self.peek()) {
            Token::Less => {
                try!(self.expect(Token::Less));
                let n = try!(self.number());
                try!(self.expect(Token::Greater));
                Some(n)
            },
            _ => None,
        };

        try!(self.expect(Token::LParen));

        let mut body = Vec::new();

        loop {
            body.push(try!(self.bitvec_member()));

            match try!(self.gettok()) {
                LexerToken(Token::Comma, _) => { },
                x => { self.untok(x); break },
            }
        }

        try!(self.expect(Token::RParen));

        Ok(TypeSpec {
            body: TypeBody::Bitvec(size, body),
            span: self.span_to_here(start),
        })
    }

    /// ```plain
    /// bitvec-member -> number ':' number
    ///                | id ':' number
    /// ```
    pub fn bitvec_member(&mut self) -> Result<BitvecMember> {
        let tok = try!(self.gettok());
        match tok.0 {
            Token::Identifier(x) => {
                try!(self.expect(Token::Colon));
                Ok(BitvecMember::Variable(x, try!(self.number())))
            },

            Token::Number(x) => {
                try!(self.expect(Token::Colon));
                Ok(BitvecMember::Literal(x, try!(self.number())))
            },

            _ => Err(self.error(tok.1, format!("expected bitvec member"))),

        }
    }

    fn global_var_decl_o(&mut self) -> Result<Option<GlobalVarDecl>> {
        Ok(match *try!(self.peek()) {
            Token::Var  => Some(try!(self.global_var_decl())),
            _           => None,
        })
    }

    /// ```plain
    /// global-var-decl -> 'var' storage var-decl
    /// ```
    pub fn global_var_decl(&mut self) -> Result<GlobalVarDecl> {
        try!(self.expect(Token::Var));

        let storage  = try!(self.storage());
        let decl     = try!(self.var_decl());

        Ok(GlobalVarDecl {
            storage:   storage,
            decl:      decl,
        })
    }

    /// ```plain
    /// storage -> storage-loc storage-params
    /// ```
    pub fn storage(&mut self) -> Result<Storage> {
        let loc = try!(self.storage_loc());
        let par = try!(self.storage_params());
        Ok(Storage {
            loc:       loc,
            params:    par,
        })
    }

    /// ```plain
    /// storage-loc -> ram | rom | ə
    /// ```
    pub fn storage_loc(&mut self) -> Result<StorageLoc> {
        Ok(match try!(self.gettok()) {
            LexerToken(Token::Ram, _) => StorageLoc::RAM,
            LexerToken(Token::Rom, _) => StorageLoc::ROM,
            x => { self.untok(x); StorageLoc::Default },
        })
    }

    /// ```plain
    /// storage-params -> storage-param storage-params
    ///                 | ə
    /// ```
    pub fn storage_params(&mut self) -> Result<Vec<StorageParam>> {
        let mut params = Vec::new();

        loop {
            match try!(self.storage_param_o()) {
                Some(x)  => params.push(x),
                None     => return Ok(params)
            }
        }
    }

    /// ```plain
    /// storage-param -> region-name
    /// ```
    fn storage_param_o(&mut self) -> Result<Option<StorageParam>> {
        Ok(match *try!(self.peek()) {
            Token::Region  => Some(StorageParam::Region(
                                    try!(self.region_name()))),
            _              => None,
        })
    }

    fn var_decl_o(&mut self) -> Result<Option<VarDecl>> {
        Ok(match *try!(self.peek()) {
            Token::Identifier(_)  => Some(try!(self.var_decl())),
            _                     => None
        })
    }

    /// ```plain
    /// var-decl -> id-list ':' type-spec ';'
    ///           | id ':' type-spec '=' expr ';'
    /// ```
    pub fn var_decl(&mut self) -> Result<VarDecl> {
        let ids = try!(self.id_list());
        try!(self.expect(Token::Colon));
        let typ = try!(self.type_spec());
        let tok = try!(self.gettok());
        let init = match tok.0 {
            Token::Semicolon => None,
            Token::Assign => {
                let x = Some(try!(self.expr()));
                try!(self.expect(Token::Semicolon));
                x
            },
            _ => return Err(self.error(tok.1,
                    format!("expected = or ; got {:?}", tok.0)))
        };

        Ok(VarDecl {
            ids:       ids,
            typ:       typ,
            init:      init,
        })
    }

    /// ```plain
    /// const-decl -> 'const' id ':' type-spec '=' constant ';'
    /// ```
    pub fn const_decl(&mut self) -> Result<ConstDecl> {
        try!(self.expect(Token::Const));
        let id = try!(self.id());
        try!(self.expect(Token::Colon));
        let typ = try!(self.type_spec());
        try!(self.expect(Token::Assign));
        let init = try!(self.constant());
        try!(self.expect(Token::Semicolon));

        Ok(ConstDecl {
            id:        id,
            typ:       typ,
            init:      init
        })
    }

    /// ```plain
    /// region-decl -> region-name '{' region-decl-body '}'
    /// ```
    pub fn region_decl(&mut self) -> Result<RegionDecl> {
        let name = try!(self.region_name());
        try!(self.expect(Token::LBrace));
        let vars = try!(self.region_decl_body());
        try!(self.expect(Token::RBrace));

        Ok(RegionDecl {
            name:      name,
            vars:      vars,
        })
    }

    /// ```plain
    /// region-name -> 'region' '(' id ',' id ')'
    /// ```
    pub fn region_name(&mut self) -> Result<RegionName> {
        try!(self.expect(Token::Region));
        try!(self.expect(Token::LParen));
        let section = try!(self.id());
        try!(self.expect(Token::Comma));
        let layer = try!(self.id());
        try!(self.expect(Token::RParen));

        Ok(RegionName {
            section:   section,
            layer:     layer
        })
    }

    /// ```plain
    /// region-decl-body -> global-var-decl region-decl-body | ə
    /// ```
    pub fn region_decl_body(&mut self) -> Result<Vec<GlobalVarDecl>> {
        let mut body = Vec::new();

        loop {
            match try!(self.global_var_decl_o()) {
                Some(x)  => body.push(x),
                None     => return Ok(body),
            }
        }
    }

    /// ```plain
    /// func-decl -> func-heading '{' func-body '}'
    /// func-heading -> 'fn' id '(' func-params ')' func-return
    /// ```
    pub fn func_decl(&mut self) -> Result<FuncDecl> {
        try!(self.expect(Token::Fn));
        let name = try!(self.id());
        try!(self.expect(Token::LParen));
        let params = try!(self.func_params());
        try!(self.expect(Token::RParen));
        let ret = try!(self.func_return());
        try!(self.expect(Token::LBrace));
        let body = try!(self.stmt_list());
        try!(self.expect(Token::RBrace));

        Ok(FuncDecl {
            name:   name,
            params: params,
            ret:    ret,
            body:   body
        })
    }

    /// ```plain
    /// func-return -> ':' type-spec | ə
    /// ```
    pub fn func_return(&mut self) -> Result<Option<TypeSpec>> {
        Ok(match try!(self.gettok()) {
            LexerToken(Token::Colon, _) => Some(try!(self.type_spec())),
            x => { self.untok(x); None }
        })
    }

    /// ```plain
    /// func-params -> func-params' | ə
    /// func-params' -> func-param ',' func-params'
    ///               | func-param
    /// ```
    pub fn func_params(&mut self) -> Result<Vec<FuncParam>> {
        let mut params = Vec::new();

        loop {
            match try!(self.func_param_o()) {
                Some(x)  => params.push(x),
                None     => return Ok(params)
            }

            match try!(self.gettok()) {
                LexerToken(Token::Comma, _) => { },
                x => { self.untok(x); return Ok(params); }
            }
        }
    }

    fn func_param_o(&mut self) -> Result<Option<FuncParam>> {
        Ok(match *try!(self.peek()) {
            Token::Identifier(_) => Some(try!(self.func_param())),
            _                    => None,
        })
    }

    /// ```plain
    /// func-param -> id-list ':' type-spec
    /// ```
    pub fn func_param(&mut self) -> Result<FuncParam> {
        let ids = try!(self.id_list());
        try!(self.expect(Token::Colon));

        Ok(FuncParam {
            ids:       ids,
            typ:       try!(self.type_spec()),
        })
    }

    /// ```plain
    /// stmt-list -> stmt stmt-list | ə
    /// ```
    pub fn stmt_list(&mut self) -> Result<Stmt> {
        let start = self.pos();
        let mut stmts = Vec::new();

        loop {
            match try!(self.stmt_o()) {
                Some(x)  => stmts.push(x),
                None =>
                    return Ok(Stmt {
                        body: StmtBody::Compound(stmts),
                        span: self.span_to_here(start),
                    }),
            }
        }
    }

    fn stmt_o(&mut self) -> Result<Option<Stmt>> {
        let tok = try!(self.gettok());

        Ok(match tok.0 {
            Token::LBrace => {
                let st = try!(self.stmt_list());
                try!(self.expect(Token::RBrace));
                Some(st)
            },

            Token::Var      =>
                Some(Stmt {
                    body: StmtBody::Var(try!(self.var_decl())),
                    span: self.span_to_here(tok.1),
                }),

            Token::If       =>
                { self.untok(tok); Some(try!(self.if_stmt())) },
            Token::Switch   =>
                { self.untok(tok); Some(try!(self.switch_stmt())) },
            Token::Loop     =>
                { self.untok(tok); Some(try!(self.loop_stmt())) },
            Token::While    =>
                { self.untok(tok); Some(try!(self.while_stmt())) },
            Token::For      =>
                { self.untok(tok); Some(try!(self.for_stmt())) },

            Token::Break    =>
                { try!(self.stmt_simple_o(tok.1, StmtBody::Break)) },
            Token::Continue =>
                { try!(self.stmt_simple_o(tok.1, StmtBody::Continue)) },
            Token::Repeat   =>
                { try!(self.stmt_simple_o(tok.1, StmtBody::Repeat)) },

            Token::Return => {
                match try!(self.gettok()) {
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
                        let ex = try!(self.expr());
                        let ret = StmtBody::Return(Some(ex));
                        try!(self.stmt_simple_o(tok.1, ret))
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
                let ex = try!(self.expr());
                try!(self.stmt_simple_o(start, StmtBody::Eval(ex)))
            },

            _ => { self.untok(tok); None }
        })
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
    pub fn stmt(&mut self) -> Result<Stmt> {
        match try!(self.stmt_o()) {
            Some(x)  => Ok(x),
            None     => Err(self.error_here("expected statement")),
        }
    }

    fn stmt_simple_o(&mut self, p: Position, body: StmtBody) -> Result<Option<Stmt>> {
        try!(self.expect(Token::Semicolon));
        Ok(Some(Stmt {
            body: body,
            span: self.span_to_here(p),
        }))
    }

    /// ```plain
    /// if-stmt -> if '(' expr ')' stmt
    ///          | if '(' expr ')' stmt 'else' stmt
    /// ```
    pub fn if_stmt(&mut self) -> Result<Stmt> {
        let start = self.pos();
        try!(self.expect(Token::If));
        try!(self.expect(Token::LParen));
        let cond = try!(self.expr());
        try!(self.expect(Token::RParen));
        let tb = Box::new(try!(self.stmt()));
        let fb = match try!(self.gettok()) {
            LexerToken(Token::Else, _) => Some(Box::new(try!(self.stmt()))),
            x => { self.untok(x); None },
        };

        Ok(Stmt {
            body: StmtBody::If(IfStmt {
                cond: cond,
                tb:   tb,
                fb:   fb,
            }),
            span: self.span_to_here(start),
        })
    }

    /// ```plain
    /// switch-stmt -> 'switch' '(' expr ')' '{' switch-body '}'
    /// ```
    pub fn switch_stmt(&mut self) -> Result<Stmt> {
        let start = self.pos();
        try!(self.expect(Token::Switch));
        try!(self.expect(Token::LParen));
        let ex = try!(self.expr());
        try!(self.expect(Token::RParen));
        try!(self.expect(Token::LBrace));
        let body = try!(self.switch_body());
        try!(self.expect(Token::RBrace));

        Ok(Stmt {
            body: StmtBody::Switch(SwitchStmt {
                ex:    ex,
                cases: body,
            }),
            span: self.span_to_here(start),
        })
    }

    /// ```plain
    /// switch-body -> switch-case switch-body | ə
    /// ```
    pub fn switch_body(&mut self) -> Result<Vec<SwitchCase>> {
        let mut body = Vec::new();

        loop {
            match try!(self.switch_case_o()) {
                Some(x)  => body.push(x),
                None     => return Ok(body)
            }
        }
    }

    fn switch_case_o(&mut self) -> Result<Option<SwitchCase>> {
        Ok(match *try!(self.peek()) {
            Token::Case | Token::Default => Some(try!(self.switch_case())),
            _ => None,
        })
    }

    /// ```plain
    /// switch-case -> 'case' const ':' stmt-list
    ///              | 'default' ':' stmt-list
    /// ```
    pub fn switch_case(&mut self) -> Result<SwitchCase> {
        let tok = try!(self.gettok());
        match tok.0 {
            Token::Case => {
                let c = try!(self.constant());
                try!(self.expect(Token::Colon));
                Ok(SwitchCase::Case(c, try!(self.stmt_list())))
            },

            Token::Default => {
                try!(self.expect(Token::Colon));
                Ok(SwitchCase::Default(try!(self.stmt_list())))
            },

            _ => Err(self.error(tok.1, format!("expected 'case' or 'default'")))
        }
    }

    /// ```plain
    /// loop-stmt -> 'loop' stmt
    /// ```
    pub fn loop_stmt(&mut self) -> Result<Stmt> {
        let start = self.pos();
        try!(self.expect(Token::Loop));

        let body = StmtBody::Loop(LoopStmt {
            body: Box::new(try!(self.stmt()))
        });
        Ok(Stmt {
            body: body,
            span: self.span_to_here(start),
        })
    }

    /// ```plain
    /// while-stmt -> 'while' '(' expr ')' stmt
    /// ```
    pub fn while_stmt(&mut self) -> Result<Stmt> {
        let start = self.pos();
        try!(self.expect(Token::While));
        try!(self.expect(Token::LParen));
        let cond = try!(self.expr());
        try!(self.expect(Token::RParen));

        let body = StmtBody::While(WhileStmt {
            cond: cond,
            body: Box::new(try!(self.stmt())),
        });
        Ok(Stmt {
            body: body,
            span: self.span_to_here(start),
        })
    }

    /// ```plain
    /// for-stmt -> 'for' '(' id 'in' expr ')' stmt
    /// ```
    pub fn for_stmt(&mut self) -> Result<Stmt> {
        let start = self.pos();
        try!(self.expect(Token::For));
        try!(self.expect(Token::LParen));
        let id = try!(self.id());
        try!(self.expect(Token::In));
        let iter = try!(self.expr());
        try!(self.expect(Token::RParen));

        let body = StmtBody::For(ForStmt {
            id:   id,
            iter: iter,
            body: Box::new(try!(self.stmt())),
        });
        Ok(Stmt {
            body: body,
            span: self.span_to_here(start),
        })
    }

    /// ```plain
    /// expr -> ex-comma
    /// ```
    pub fn expr(&mut self) -> Result<Expr<Primary>> {
        self.ex_comma()
    }

    /// ```plain
    /// ex-list -> ex-list' | ə
    /// ex-list' -> ex-assign ',' ex-list'
    ///           | ex-assign
    /// ```
    pub fn ex_list(&mut self) -> Result<Vec<Expr<Primary>>> {
        let mut list = Vec::new();

        loop {
            // this is awful, it's everything that can follow an ex-list,
            // and it stops if it sees that
            match *try!(self.peek()) {
                Token::RParen => return Ok(list),
                _ => { }
            }

            list.push(try!(self.ex_assign()));

            match *try!(self.peek()) {
                Token::Comma => { try!(self.gettok()); },
                _ => return Ok(list),
            }
        }
    }

    /// ```plain
    /// ex-comma -> ex-assign
    ///           | ex-assign , ex-comma
    /// ```
    pub fn ex_comma(&mut self) -> Result<Expr<Primary>> {
        let left = try!(self.ex_assign());

        match *try!(self.peek()) {
            Token::Comma => { },
            _ => { return Ok(left) }
        }

        let mut vec = Vec::new();
        vec.push(left);

        loop {
            match try!(self.gettok()) {
                LexerToken(Token::Comma, _) => vec.push(try!(self.ex_assign())),
                x => { self.untok(x); return Ok(Expr::Comma(vec)) }
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
    pub fn ex_assign(&mut self) -> Result<Expr<Primary>> {
        let left = try!(self.ex_tern());

        let op = match *try!(self.peek()) {
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
            _ => return Ok(left),
        };

        try!(self.gettok());

        Ok(Expr::Assign(
            op,
            Box::new(left),
            Box::new(try!(self.ex_assign())),
        ))
    }

    /// ```plain
    /// ex-tern -> ex-lor
    ///          | ex-lor '?' ex-lor ':' ex-tern
    /// ```
    pub fn ex_tern(&mut self) -> Result<Expr<Primary>> {
        let left = try!(self.ex_lor());

        match *try!(self.peek()) {
            Token::Question => { },
            _ => return Ok(left),
        }

        try!(self.expect(Token::Question));
        let mid = try!(self.ex_lor());
        try!(self.expect(Token::Colon));

        Ok(Expr::Ternary(
            Box::new(left),
            Box::new(mid),
            Box::new(try!(self.ex_tern()))
        ))
    }

    /// ```plain
    /// ex-lor -> ex-land
    ///         | ex-lor '||' ex-land
    /// ```
    pub fn ex_lor(&mut self) -> Result<Expr<Primary>> {
        let mut left = try!(self.ex_land());

        loop {
            let tok = try!(self.gettok());
            match tok.0 {
                Token::DblPipe =>
                    left = Expr::Binary(
                        BinOp::BoolOr,
                        Box::new(left),
                        Box::new(try!(self.ex_land()))
                    ),
                _ =>
                    { self.untok(tok); return Ok(left) }
            }
        }
    }

    /// ```plain
    /// ex-land -> ex-bor
    ///          | ex-land '&&' ex-bor
    /// ```
    pub fn ex_land(&mut self) -> Result<Expr<Primary>> {
        let mut left = try!(self.ex_bor());

        loop {
            let tok = try!(self.gettok());
            match tok.0 {
                Token::DblAmp =>
                    left = Expr::Binary(
                        BinOp::BoolAnd,
                        Box::new(left),
                        Box::new(try!(self.ex_bor()))
                    ),
                _ =>
                    { self.untok(tok); return Ok(left) }
            }
        }
    }

    /// ```plain
    /// ex-bor -> ex-bxor
    ///         | ex-bor '|' ex-bxor
    /// ```
    pub fn ex_bor(&mut self) -> Result<Expr<Primary>> {
        let mut left = try!(self.ex_bxor());

        loop {
            let tok = try!(self.gettok());
            match tok.0 {
                Token::Pipe =>
                    left = Expr::Binary(
                        BinOp::BitOr,
                        Box::new(left),
                        Box::new(try!(self.ex_bxor()))
                    ),
                _ =>
                    { self.untok(tok); return Ok(left) }
            }
        }
    }

    /// ```plain
    /// ex-bxor -> ex-band
    ///          | ex-bxor '^' ex-band
    /// ```
    pub fn ex_bxor(&mut self) -> Result<Expr<Primary>> {
        let mut left = try!(self.ex_band());

        loop {
            let tok = try!(self.gettok());
            match tok.0 {
                Token::Caret =>
                    left = Expr::Binary(
                        BinOp::BitXor,
                        Box::new(left),
                        Box::new(try!(self.ex_band()))
                    ),
                _ =>
                    { self.untok(tok); return Ok(left) }
            }
        }
    }

    /// ```plain
    /// ex-band -> ex-eq
    ///          | ex-band '&' ex-eq
    /// ```
    pub fn ex_band(&mut self) -> Result<Expr<Primary>> {
        let mut left = try!(self.ex_eq());

        loop {
            let tok = try!(self.gettok());
            match tok.0 {
                Token::Amp =>
                    left = Expr::Binary(
                        BinOp::BitAnd,
                        Box::new(left),
                        Box::new(try!(self.ex_eq()))
                    ),
                _ =>
                    { self.untok(tok); return Ok(left) }
            }
        }
    }

    /// ```plain
    /// ex-eq -> ex-cmp
    ///        | ex-eq '==' ex-cmp
    ///        | ex-eq '!=' ex-cmp
    /// ```
    pub fn ex_eq(&mut self) -> Result<Expr<Primary>> {
        let mut left = try!(self.ex_cmp());

        loop {
            let tok = try!(self.gettok());
            match tok.0 {
                Token::Eq =>
                    left = Expr::Binary(
                        BinOp::Eq,
                        Box::new(left),
                        Box::new(try!(self.ex_cmp()))
                    ),
                Token::NotEq =>
                    left = Expr::Binary(
                        BinOp::NotEq,
                        Box::new(left),
                        Box::new(try!(self.ex_cmp()))
                    ),

                _ =>
                    { self.untok(tok); return Ok(left) }
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
    pub fn ex_cmp(&mut self) -> Result<Expr<Primary>> {
        let mut left = try!(self.ex_shift());

        loop {
            let tok = try!(self.gettok());
            match tok.0 {
                Token::Less =>
                    left = Expr::Binary(
                        BinOp::Less,
                        Box::new(left),
                        Box::new(try!(self.ex_shift()))
                    ),
                Token::Greater =>
                    left = Expr::Binary(
                        BinOp::Greater,
                        Box::new(left),
                        Box::new(try!(self.ex_shift()))
                    ),
                Token::LessEq =>
                    left = Expr::Binary(
                        BinOp::LessEq,
                        Box::new(left),
                        Box::new(try!(self.ex_shift()))
                    ),
                Token::GreaterEq =>
                    left = Expr::Binary(
                        BinOp::GreaterEq,
                        Box::new(left),
                        Box::new(try!(self.ex_shift()))
                    ),

                _ =>
                    { self.untok(tok); return Ok(left) }
            }
        }
    }

    /// ```plain
    /// ex-shift -> ex-add
    ///           | ex-shift '<<' ex-add
    ///           | ex-shift '>>' ex-add
    /// ```
    pub fn ex_shift(&mut self) -> Result<Expr<Primary>> {
        let mut left = try!(self.ex_add());

        loop {
            let tok = try!(self.gettok());
            match tok.0 {
                Token::LShift =>
                    left = Expr::Binary(
                        BinOp::LShift,
                        Box::new(left),
                        Box::new(try!(self.ex_add()))
                    ),
                Token::RShift =>
                    left = Expr::Binary(
                        BinOp::RShift,
                        Box::new(left),
                        Box::new(try!(self.ex_add()))
                    ),

                _ =>
                    { self.untok(tok); return Ok(left) }
            }
        }
    }

    /// ```plain
    /// ex-add -> ex-mul
    ///         | ex-add '+' ex-mul
    ///         | ex-add '-' ex-mul
    /// ```
    pub fn ex_add(&mut self) -> Result<Expr<Primary>> {
        let mut left = try!(self.ex_mul());

        loop {
            let tok = try!(self.gettok());
            match tok.0 {
                Token::Plus =>
                    left = Expr::Binary(
                        BinOp::Add,
                        Box::new(left),
                        Box::new(try!(self.ex_mul()))
                    ),
                Token::Minus =>
                    left = Expr::Binary(
                        BinOp::Sub,
                        Box::new(left),
                        Box::new(try!(self.ex_mul()))
                    ),

                _ =>
                    { self.untok(tok); return Ok(left) }
            }
        }
    }

    /// ```plain
    /// ex-mul -> ex-unary
    ///         | ex-mul '*' ex-unary
    ///         | ex-mul '/' ex-unary
    ///         | ex-mul '%' ex-unary
    /// ```
    pub fn ex_mul(&mut self) -> Result<Expr<Primary>> {
        let mut left = try!(self.ex_unary());

        loop {
            let tok = try!(self.gettok());
            match tok.0 {
                Token::Star =>
                    left = Expr::Binary(
                        BinOp::Mul,
                        Box::new(left),
                        Box::new(try!(self.ex_unary()))
                    ),
                Token::Slash =>
                    left = Expr::Binary(
                        BinOp::Div,
                        Box::new(left),
                        Box::new(try!(self.ex_unary()))
                    ),
                Token::Mod =>
                    left = Expr::Binary(
                        BinOp::Mod,
                        Box::new(left),
                        Box::new(try!(self.ex_unary()))
                    ),

                _ =>
                    { self.untok(tok); return Ok(left) }
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
    pub fn ex_unary(&mut self) -> Result<Expr<Primary>> {
        let tok = try!(self.gettok());

        match tok.0 {
            Token::Incr =>
                Ok(Expr::Unary(UnOp::PreIncr, Box::new(try!(self.ex_unary())))),
            Token::Decr =>
                Ok(Expr::Unary(UnOp::PreDecr, Box::new(try!(self.ex_unary())))),
            Token::Excl =>
                Ok(Expr::Unary(UnOp::BoolNot, Box::new(try!(self.ex_unary())))),
            Token::Tilde =>
                Ok(Expr::Unary(UnOp::BitNot, Box::new(try!(self.ex_unary())))),
            Token::Star =>
                Ok(Expr::Unary(UnOp::Deref, Box::new(try!(self.ex_unary())))),
            Token::Amp =>
                Ok(Expr::Unary(UnOp::AddrOf, Box::new(try!(self.ex_unary())))),
            Token::Sizeof =>
                Ok(Expr::Unary(UnOp::SizeOf, Box::new(try!(self.ex_unary())))),

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
    pub fn ex_bottom(&mut self) -> Result<Expr<Primary>> {
        let mut left = try!(self.ex_primary());

        loop {
            let tok = try!(self.gettok());

            match tok.0 {
                Token::Incr =>
                    return Ok(Expr::Unary(UnOp::PostIncr, Box::new(left))),
                Token::Decr =>
                    return Ok(Expr::Unary(UnOp::PostDecr, Box::new(left))),
                Token::LParen => {
                    left = Expr::Call(Box::new(left), try!(self.ex_list()));
                    try!(self.expect(Token::RParen));
                },
                Token::LBrack => {
                    left = Expr::Binary(
                        BinOp::Element,
                        Box::new(left),
                        Box::new(try!(self.expr()))
                    );
                    try!(self.expect(Token::RBrack));
                },
                Token::Dot =>
                    left = Expr::Member(Box::new(left), try!(self.id())),

                _ =>
                    { self.untok(tok); return Ok(left) }
            }
        }
    }

    /// ```plain
    /// ex-primary -> path
    ///            -> constant
    ///            -> '(' expr ')'
    /// ```
    pub fn ex_primary(&mut self) -> Result<Expr<Primary>> {
        let tok = try!(self.gettok());

        match tok.0 {
            Token::Identifier(_) => {
                self.untok(tok);
                Ok(Expr::Primary(Primary::Path(try!(self.path()))))
            }
            Token::Number(x) =>
                Ok(Expr::Primary(Primary::Number(x as isize))),
            Token::Character(x) =>
                Ok(Expr::Primary(Primary::Number(x as isize))),
            Token::LParen => {
                let ex = try!(self.expr());
                try!(self.expect(Token::RParen));
                Ok(ex)
            }

            _ => Err(self.error(tok.1, format!("expected expression")))
        }
    }
}
