/* parse tree definitions */

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
    Literal        (Number),
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
    Member,
    Eq,
    NotEq,
    Less,
    Greater,
    LessEq,
    GreaterEq,
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
            BinOp::Element     => ".",
            BinOp::Member      => "[]",
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
        for i in range(0, self.depth) {
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
        self.put(String::from_str("("));
        self.depth += 1;
    }

    pub fn pop(&mut self) {
        if self.depth > 0 {
            self.depth -= 1;
        }
        self.newline();
        self.put(String::from_str(")"));
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
            BitvecMember::Literal(n) =>
                d.put_ln(format!("literal {}", n)),
            BitvecMember::Variable(ref x, y) =>
                d.put_ln(format!("var {} : {}", x, y)),
        }
    }
}

impl GlobalVarDecl {
    pub fn dump(&self, d: &mut DumpContext) {
        d.put_ln_str("(not implemented)");
    }
}

impl Storage {
    pub fn dump(&self, d: &mut DumpContext) {
        d.put_ln_str("(not implemented)");
    }
}

impl StorageLoc {
    pub fn dump(&self, d: &mut DumpContext) {
        d.put_ln_str("(not implemented)");
    }
}

impl StorageParam {
    pub fn dump(&self, d: &mut DumpContext) {
        d.put_ln_str("(not implemented)");
    }
}

impl VarDecl {
    pub fn dump(&self, d: &mut DumpContext) {
        d.put_ln_str("(not implemented)");
    }
}

impl ConstDecl {
    pub fn dump(&self, d: &mut DumpContext) {
        d.put_ln_str("(not implemented)");
    }
}

impl RegionDecl {
    pub fn dump(&self, d: &mut DumpContext) {
        d.put_ln_str("(not implemented)");
    }
}

impl RegionName {
    pub fn dump(&self, d: &mut DumpContext) {
        d.put_ln_str("(not implemented)");
    }
}

impl FuncDecl {
    pub fn dump(&self, d: &mut DumpContext) {
        d.put_ln_str("(not implemented)");
    }
}

impl FuncParam {
    pub fn dump(&self, d: &mut DumpContext) {
        d.put_ln_str("(not implemented)");
    }
}

impl Stmt {
    pub fn dump(&self, d: &mut DumpContext) {
        d.put_ln_str("(not implemented)");
    }
}

impl IfStmt {
    pub fn dump(&self, d: &mut DumpContext) {
        d.put_ln_str("(not implemented)");
    }
}

impl SwitchStmt {
    pub fn dump(&self, d: &mut DumpContext) {
        d.put_ln_str("(not implemented)");
    }
}

impl SwitchCase {
    pub fn dump(&self, d: &mut DumpContext) {
        d.put_ln_str("(not implemented)");
    }
}

impl LoopStmt {
    pub fn dump(&self, d: &mut DumpContext) {
        d.put_ln_str("(not implemented)");
    }
}

impl WhileStmt {
    pub fn dump(&self, d: &mut DumpContext) {
        d.put_ln_str("(not implemented)");
    }
}

impl ForStmt {
    pub fn dump(&self, d: &mut DumpContext) {
        d.put_ln_str("(not implemented)");
    }
}

impl Expr {
    pub fn dump(&self, d: &mut DumpContext) {
        d.put_ln_str("(not implemented)");
    }
}
