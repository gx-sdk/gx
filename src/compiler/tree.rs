/* parse tree definitions */

pub type Id = String;
pub type Number = int;

pub type Input = Vec<Unit>;

pub struct Unit {
    name:          Id,
    decls:         Vec<Decl>
}

pub struct Decl {
    is_pub:        bool,
    body:          DeclBody,
}
pub enum DeclBody {
    Type           (TypeDecl),
    Func           (FuncDecl),
    GlobalVar      (GlobalVarDecl),
    Const          (ConstDecl),
    Region         (RegionDecl),
}

pub struct TypeDecl {
    name:          String,
    typ:           TypeSpec
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
    storage:       Storage,
    decl:          VarDecl,
}
pub struct Storage {
    loc:           StorageLoc,
    params:        Vec<StorageParam>,
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
    ids:           Vec<Id>,
    typ:           TypeSpec,
    initializer:   Expr,
}

pub struct ConstDecl {
    id:            Id,
    typ:           TypeSpec,
    initializer:   Expr,
}

pub struct RegionDecl {
    name:          RegionName,
    vars:          Vec<GlobalVarDecl>,
}
pub struct RegionName {
    section:       Id,
    layer:         Id,
}

pub struct FuncDecl {
    name:          Id,
    params:        Vec<FuncParam>,
    ret:           TypeSpec,
    body:          Stmt,
}
pub struct FuncParam {
    ids:           Vec<Id>,
    typ:           TypeSpec,
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
    cond:          Expr,
    tb:            Box<Stmt>,
    fb:            Option<Box<Stmt>>,
}

pub struct SwitchStmt {
    ex:            Expr,
    cases:         Vec<SwitchCase>,
}
pub enum SwitchCase {
    Case           (Expr, Vec<Stmt>),
    Default        (Vec<Stmt>),
}

pub struct LoopStmt {
    body:          Box<Stmt>
}

pub struct WhileStmt {
    cond:          Expr,
    body:          Box<Stmt>,
}

pub struct ForStmt {
    id:            Id,
    iter:          Expr,
    body:          Box<Stmt>,
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
