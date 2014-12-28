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
    Member         (Box<Expr>, Id),
    Id             (Id),
    Number         (Number),
}
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
