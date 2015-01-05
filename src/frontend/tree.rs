// gx language implementation
// Copyright (C) 2014-present Alex Iadicicco <http://ajitek.net>
//
// For licensing information, refer to the COPYING file
// in the project root

//! Parse tree structures. Most of these match almost exactly with the
//! equivalents in the grammar.

pub type Id = String;
pub type Number = int;

pub type Input = Vec<Unit>;

pub struct Unit {
    pub name:      Id,
    pub decls:     Vec<Decl>
}

pub struct Decl {
    pub is_pub:    bool,
    pub body:      DeclBody,
}
pub enum DeclBody {
    Type           (TypeDecl),
    Func           (FuncDecl),
    GlobalVar      (GlobalVarDecl),
    Const          (ConstDecl),
    Region         (RegionDecl),
}

pub struct TypeDecl {
    pub name:      String,
    pub typ:       TypeSpec
}

pub enum TypeSpec {
    Alias          (Id),
    Parameterized  (Id, Vec<Expr>),
    Pointer        (Box<TypeSpec>),
    Array          (Number, Box<TypeSpec>),
    Struct         (Vec<VarDecl>),
    Bitvec         (Option<Number>, Vec<BitvecMember>),
}
pub enum BitvecMember {
    Literal        (uint, Number),
    Variable       (Id, Number),
}

pub struct GlobalVarDecl {
    pub storage:   Storage,
    pub decl:      VarDecl,
}
pub struct Storage {
    pub loc:       StorageLoc,
    pub params:    Vec<StorageParam>,
}
pub enum StorageLoc {
    Default,
    RAM,
    ROM,
}
impl Copy for StorageLoc {
}
pub enum StorageParam {
    Region         (RegionName),
    Ext            (Id, Vec<Expr>),
}
pub struct VarDecl {
    pub ids:       Vec<Id>,
    pub typ:       TypeSpec,
    pub init:      Option<Expr>,
}

pub struct ConstDecl {
    pub id:        Id,
    pub typ:       TypeSpec,
    pub init:      Expr,
}

pub struct RegionDecl {
    pub name:      RegionName,
    pub vars:      Vec<GlobalVarDecl>,
}
pub struct RegionName {
    pub section:   Id,
    pub layer:     Id,
}

pub struct FuncDecl {
    pub name:      Id,
    pub params:    Vec<FuncParam>,
    pub ret:       Option<TypeSpec>,
    pub body:      Stmt,
}
pub struct FuncParam {
    pub ids:       Vec<Id>,
    pub typ:       TypeSpec,
}

pub enum Stmt {
    Eval           (Expr),
    Compound       (Vec<Stmt>),
    Var            (VarDecl),
    If             (IfStmt),
    Switch         (SwitchStmt),
    Loop           (LoopStmt),
    While          (WhileStmt),
    For            (ForStmt),
    Break,
    Continue,
    Repeat,
    Return         (Option<Expr>)
}

pub struct IfStmt {
    pub cond:      Expr,
    pub tb:        Box<Stmt>,
    pub fb:        Option<Box<Stmt>>,
}

pub struct SwitchStmt {
    pub ex:        Expr,
    pub cases:     Vec<SwitchCase>,
}
pub enum SwitchCase {
    Case           (Expr, Stmt),
    Default        (Stmt),
}

pub struct LoopStmt {
    pub body:      Box<Stmt>
}

pub struct WhileStmt {
    pub cond:      Expr,
    pub body:      Box<Stmt>,
}

pub struct ForStmt {
    pub id:        Id,
    pub iter:      Expr,
    pub body:      Box<Stmt>,
}

pub enum Expr {
    Comma          (Vec<Expr>),
    Assign         (Box<Expr>, Box<Expr>, BinOp),
    Ternary        (Box<Expr>, Box<Expr>, Box<Expr>),
    Binary         (BinOp, Box<Expr>, Box<Expr>),
    Unary          (UnOp, Box<Expr>),
    Call           (Box<Expr>, Vec<Expr>),
    Member         (Box<Expr>, Id),
    Scoped         (Box<Expr>, Id),
    Id             (Id),
    Number         (Number),
}
#[deriving(Show)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    LShift,
    RShift,
    BitAnd,
    BitOr,
    BitXor,
    BoolAnd,
    BoolOr,
    Element,
    Eq,
    NotEq,
    Less,
    Greater,
    LessEq,
    GreaterEq,
}
impl Copy for BinOp {
}
#[deriving(Show)]
pub enum UnOp {
    PreIncr,
    PostIncr,
    PreDecr,
    PostDecr,
    AddrOf,
    Deref,
    SizeOf,
    BitNot,
    BoolNot,
}
impl Copy for UnOp {
}

impl BinOp {
    pub fn glyph(&self) -> &'static str {
        match *self {
            BinOp::Add         => "+",
            BinOp::Sub         => "-",
            BinOp::Mul         => "*",
            BinOp::Div         => "/",
            BinOp::Mod         => "%",
            BinOp::LShift      => "<<",
            BinOp::RShift      => ">>",
            BinOp::BitAnd      => "&",
            BinOp::BitOr       => "|",
            BinOp::BitXor      => "^",
            BinOp::BoolAnd     => "&&",
            BinOp::BoolOr      => "||",
            BinOp::Element     => "[]",
            BinOp::Eq          => "==",
            BinOp::NotEq       => "!=",
            BinOp::Less        => "<",
            BinOp::Greater     => ">",
            BinOp::LessEq      => "<=",
            BinOp::GreaterEq   => ">=",
        }
    }
}

pub struct DumpContext {
    pub blank: bool,
    pub depth: int,
}
impl Copy for DumpContext {
}

impl DumpContext {
    pub fn new() -> DumpContext {
        DumpContext {
            blank:  true,
            depth:  0,
        }
    }

    pub fn newline(&mut self) {
        if self.blank {
            return;
        }

        self.put(String::from_str("\n"));
        for _ in range(0, self.depth) {
            print!("  ");
        }
        self.blank = true;
    }

    pub fn push_str(&mut self, s: &str) {
        self.push(String::from_str(s));
    }

    pub fn push(&mut self, s: String) {
        self.newline();
        self.put(s);
        self.put(String::from_str(":"));
        self.depth += 1;
    }

    pub fn pop(&mut self) {
        if self.depth > 0 {
            self.depth -= 1;
        }
    }

    pub fn put_ln_str(&mut self, s: &str) {
        self.put_ln(String::from_str(s));
    }

    pub fn put_ln(&mut self, s: String) {
        self.newline();
        self.put(s);
    }

    fn put(&mut self, s: String) {
        self.blank = false;
        print!("{}", s);
    }

    pub fn end(&mut self) {
        if !self.blank {
            print!("\n");
        }
        self.blank = true;
    }
}

impl Unit {
    pub fn dump(&self, d: &mut DumpContext) {
        d.push_str("Unit");
        d.put_ln(format!("name: {}", self.name));
        for decl in self.decls.iter() {
            decl.dump(d);
        }
        d.pop();
    }
}

impl Decl {
    pub fn dump(&self, d: &mut DumpContext) {
        d.push_str("Decl");
        d.put_ln(format!("pub: {}", self.is_pub));
        self.body.dump(d);
        d.pop();
    }
}

impl DeclBody {
    pub fn dump(&self, d: &mut DumpContext) {
        match *self {
            DeclBody::Type(ref x) => {
                d.push_str("DeclBody::Type");
                x.dump(d);
            },
            DeclBody::Func(ref x) => {
                d.push_str("DeclBody::Func");
                x.dump(d);
            },
            DeclBody::GlobalVar(ref x) => {
                d.push_str("DeclBody::GlobalVar");
                x.dump(d);
            },
            DeclBody::Const(ref x) => {
                d.push_str("DeclBody::Const");
                x.dump(d);
            },
            DeclBody::Region(ref x) => {
                d.push_str("DeclBody::Region");
                x.dump(d);
            },
        };
        d.pop();
    }
}

impl TypeDecl {
    pub fn dump(&self, d: &mut DumpContext) {
        d.push_str("TypeDecl");
        d.put_ln(format!("name: {}", self.name));
        self.typ.dump(d);
        d.pop();
    }
}

impl TypeSpec {
    pub fn dump(&self, d: &mut DumpContext) {
        match *self {
            TypeSpec::Alias(ref x) => {
                d.put_ln(format!("TypeSpec::Alias({})", x));
            },
            TypeSpec::Parameterized(ref x, ref y) => {
                d.push_str("TypeSpec::Parameterized");
                d.put_ln(format!("id: {}", x));
                for t in y.iter() {
                    t.dump(d);
                }
                d.pop();
            },
            TypeSpec::Pointer(box ref x) => {
                d.push_str("TypeSpec::Pointer");
                x.dump(d);
                d.pop();
            },
            TypeSpec::Array(n, box ref t) => {
                d.push_str("TypeSpec::Array");
                d.put_ln(format!("size: {}", n));
                t.dump(d);
                d.pop();
            },
            TypeSpec::Struct(ref x) => {
                d.push_str("TypeSpec::Struct");
                for t in x.iter() {
                    t.dump(d);
                }
                d.pop();
            },
            TypeSpec::Bitvec(ref x, ref y) => {
                d.push_str("TypeSpec::Bitvec");
                match *x {
                    Some(n)  => d.put_ln(format!("size: {}", n)),
                    None     => d.put_ln_str("size: auto"),
                }
                for t in y.iter() {
                    t.dump(d);
                }
                d.pop();
            },
        }
    }
}

impl BitvecMember {
    pub fn dump(&self, d: &mut DumpContext) {
        match *self {
            BitvecMember::Literal(n, w) =>
                d.put_ln(format!("{} : {}", n, w)),
            BitvecMember::Variable(ref x, w) =>
                d.put_ln(format!("{} : {}", x, w)),
        }
    }
}

impl GlobalVarDecl {
    pub fn dump(&self, d: &mut DumpContext) {
        d.push_str("GlobalVarDecl");
        self.storage.dump(d);
        self.decl.dump(d);
        d.pop();
    }
}

impl Storage {
    pub fn dump(&self, d: &mut DumpContext) {
        d.push_str("Storage");
        self.loc.dump(d);
        for p in self.params.iter() {
            p.dump(d);
        }
        d.pop();
    }
}

impl StorageLoc {
    pub fn dump(&self, d: &mut DumpContext) {
        d.put_ln_str(match *self {
            StorageLoc::Default => "StorageLoc::Default",
            StorageLoc::RAM     => "StorageLoc::RAM",
            StorageLoc::ROM     => "StorageLoc::ROM",
        });
    }
}

impl StorageParam {
    pub fn dump(&self, d: &mut DumpContext) {
        match *self {
            StorageParam::Region(ref nm) => {
                d.push_str("StorageParam::Region");
                nm.dump(d);
                d.pop();
            },
            StorageParam::Ext(ref id, ref v) => {
                d.push_str("StorageParam::Ext");
                d.put_ln(format!("id: {}", id));
                for ex in v.iter() {
                    ex.dump(d);
                }
                d.pop();
            },
        }
    }
}

impl VarDecl {
    pub fn dump(&self, d: &mut DumpContext) {
        d.push_str("VarDecl");
        if self.ids.len() == 1 {
            d.put_ln(format!("id: {}", self.ids[0]));
        } else {
            d.push_str("ids");
            for id in self.ids.iter() {
                d.put_ln(id.clone());
            }
            d.pop();
        }
        self.typ.dump(d);
        match self.init {
            Some(ref x)  => x.dump(d),
            None         => { },
        }
        d.pop();
    }
}

impl ConstDecl {
    pub fn dump(&self, d: &mut DumpContext) {
        d.push_str("ConstDecl");
        d.put_ln(format!("id: {}", self.id));
        self.typ.dump(d);
        self.init.dump(d);
        d.pop();
    }
}

impl RegionDecl {
    pub fn dump(&self, d: &mut DumpContext) {
        d.push_str("RegionDecl");
        self.name.dump(d);
        for v in self.vars.iter() {
            v.dump(d);
        }
        d.pop();
    }
}

impl RegionName {
    pub fn dump(&self, d: &mut DumpContext) {
        d.put_ln(format!("region({}, {})", self.section, self.layer));
    }
}

impl FuncDecl {
    pub fn dump(&self, d: &mut DumpContext) {
        d.push_str("FuncDecl");
        d.put_ln(self.name.clone());
        for p in self.params.iter() {
            p.dump(d);
        }
        match self.ret {
            Some(ref t)  => t.dump(d),
            None         => d.put_ln_str("(no return type)"),
        }
        self.body.dump(d);
        d.pop();
    }
}

impl FuncParam {
    pub fn dump(&self, d: &mut DumpContext) {
        for id in self.ids.iter() {
            d.push_str("FuncParam");
            d.put_ln(format!("id: {}", id));
            self.typ.dump(d);
            d.pop();
        }
    }
}

impl Stmt {
    pub fn dump(&self, d: &mut DumpContext) {
        match *self {
            Stmt::Eval(ref ex) => {
                d.push_str("Stmt::Eval");
                ex.dump(d);
                d.pop();
            },
            Stmt::Compound(ref v) => {
                d.push_str("Stmt::Eval");
                for st in v.iter() {
                    st.dump(d);
                }
                d.pop();
            },
            Stmt::Var(ref decl) =>
                decl.dump(d),
            Stmt::If(ref f) =>
                f.dump(d),
            Stmt::Switch(ref s) =>
                s.dump(d),
            Stmt::Loop(ref l) =>
                l.dump(d),
            Stmt::While(ref w) =>
                w.dump(d),
            Stmt::For(ref f) =>
                f.dump(d),
            Stmt::Break =>
                d.put_ln_str("Stmt::Break"),
            Stmt::Continue =>
                d.put_ln_str("Stmt::Continue"),
            Stmt::Repeat =>
                d.put_ln_str("Stmt::Repeat"),
            Stmt::Return(ref t) => {
                match *t {
                    Some(ref ex) => {
                        d.push_str("Stmt::Return");
                        ex.dump(d);
                        d.pop();
                    },
                    None => {
                        d.put_ln_str("Stmt::Return");
                    },
                }
            },
        }
    }
}

impl IfStmt {
    pub fn dump(&self, d: &mut DumpContext) {
        d.push_str("IfStmt");
        self.cond.dump(d);
        self.tb.dump(d);
        match self.fb {
            Some(box ref st)  => st.dump(d),
            None              => { },
        }
        d.pop();
    }
}

impl SwitchStmt {
    pub fn dump(&self, d: &mut DumpContext) {
        d.push_str("SwitchStmt");
        self.ex.dump(d);
        for c in self.cases.iter() {
            c.dump(d);
        }
        d.pop();
    }
}

impl SwitchCase {
    pub fn dump(&self, d: &mut DumpContext) {
        match *self {
            SwitchCase::Case(ref x, ref st) => {
                d.push_str("SwitchCase::Case");
                x.dump(d);
                st.dump(d);
                d.pop();
            },
            SwitchCase::Default(ref st) => {
                d.push_str("SwitchCase::Default");
                st.dump(d);
                d.pop();
            },
        }
    }
}

impl LoopStmt {
    pub fn dump(&self, d: &mut DumpContext) {
        d.push_str("LoopStmt");
        self.body.dump(d);
        d.pop();
    }
}

impl WhileStmt {
    pub fn dump(&self, d: &mut DumpContext) {
        d.push_str("WhileStmt");
        self.cond.dump(d);
        self.body.dump(d);
        d.pop();
    }
}

impl ForStmt {
    pub fn dump(&self, d: &mut DumpContext) {
        d.push_str("ForStmt");
        d.put_ln(format!("id: {}", self.id));
        self.iter.dump(d);
        self.body.dump(d);
        d.pop();
    }
}

impl Expr {
    pub fn dump(&self, d: &mut DumpContext) {
        match *self {
            Expr::Comma(ref v) => {
                d.push_str("Expr::Comma");
                for ex in v.iter() {
                    ex.dump(d);
                }
                d.pop();
            },

            Expr::Assign(box ref to, box ref fr, ref op) => {
                d.push_str("Expr::Assign");
                to.dump(d);
                fr.dump(d);
                d.put_ln(format!("{}", op));
                d.pop();
            },

            Expr::Ternary(box ref cond, box ref tb, box ref fb) => {
                d.push_str("Expr::Ternary");
                cond.dump(d);
                tb.dump(d);
                fb.dump(d);
                d.pop();
            },

            Expr::Binary(ref op, box ref e1, box ref e2) => {
                d.push_str("Expr::Binary");
                d.put_ln(format!("{}", op));
                e1.dump(d);
                e2.dump(d);
                d.pop();
            },

            Expr::Unary(ref op, box ref ex) => {
                d.push_str("Expr::Unary");
                d.put_ln(format!("{}", op));
                ex.dump(d);
                d.pop();
            },

            Expr::Call(box ref f, ref v) => {
                d.push_str("Expr::Call");
                f.dump(d);
                for ex in v.iter() {
                    ex.dump(d);
                }
                d.pop();
            },

            Expr::Member(box ref ex, ref id) => {
                d.push_str("Expr::Member");
                ex.dump(d);
                d.put_ln(format!("member: {}", id));
                d.pop();
            },

            Expr::Scoped(box ref ex, ref id) => {
                d.push_str("Expr::Scoped");
                ex.dump(d);
                d.put_ln(format!("field: {}", id));
                d.pop();
            },

            Expr::Id(ref nm) =>
                d.put_ln(format!("Expr::Id({})", nm)),

            Expr::Number(n) =>
                d.put_ln(format!("Expr::Number({})", n)),
        }
    }
}
