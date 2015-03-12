// gx language implementation
// Copyright (C) 2014-present Alex Iadicicco <http://ajitek.net>
//
// For licensing information, refer to the COPYING file
// in the project root

//! Parse tree structures. Most of these match almost exactly with the
//! equivalents in the grammar.

use frontend::lexer::Position;

use expr::Expr;
use dump::*;

pub type Id = String;
pub type Number = isize;

pub type Input = Vec<Decl>;

pub struct Path(pub Vec<Id>);

#[derive(Copy)]
pub struct Span {
    pub start:     Position,
    pub end:       Position,
}

pub struct Decl {
    pub is_pub:    bool,
    pub body:      DeclBody,
    pub span:      Span,
}
pub enum DeclBody {
    Unit           (UnitDecl),
    Use            (UseDecl),
    Type           (TypeDecl),
    Func           (FuncDecl),
    GlobalVar      (GlobalVarDecl),
    Const          (ConstDecl),
    Region         (RegionDecl),
}

/// Declaration of a namespace, a "unit".
pub struct UnitDecl {
    pub name:      Id,
    pub decls:     Vec<Decl>
}

/// Namespace import into the current lexical region
pub enum UseDecl {
    Single         (Path),
    Many           (Path, Vec<Id>),
    Aliased        (Path, Id),
    Glob           (Path),
}

/// Declaration of a type alias
pub struct TypeDecl {
    pub name:      String,
    pub typ:       TypeSpec
}

/// Type specifier
pub struct TypeSpec {
    pub body:      TypeBody,
    pub span:      Span,
}
pub enum TypeBody {
    Alias          (Path),
    Parameterized  (Path, Vec<Expr<Primary>>),
    Pointer        (Box<TypeSpec>),
    Array          (Number, Box<TypeSpec>),
    Struct         (Vec<VarDecl>),
    Bitvec         (Option<Number>, Vec<BitvecMember>),
}
pub enum BitvecMember {
    Literal        (usize, Number),
    Variable       (Id, Number),
}

/// Global storage declaration
pub struct GlobalVarDecl {
    pub storage:   Storage,
    pub decl:      VarDecl,
}
pub struct Storage {
    pub loc:       StorageLoc,
    pub params:    Vec<StorageParam>,
}
#[derive(Copy)]
pub enum StorageLoc {
    Default,
    RAM,
    ROM,
}
pub enum StorageParam {
    Region         (RegionName),
    Ext            (Id, Vec<Expr<Primary>>),
}
/// Generic variable declaration
pub struct VarDecl {
    pub ids:       Vec<Id>,
    pub typ:       TypeSpec,
    pub init:      Option<Expr<Primary>>,
}

pub struct ConstDecl {
    pub id:        Id,
    pub typ:       TypeSpec,
    pub init:      Expr<Primary>,
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

pub struct Stmt {
    pub body:      StmtBody,
    pub span:      Span
}

/// Program statement body
pub enum StmtBody {
    /// Evaluate the given expression and discard the result
    Eval           (Expr<Primary>),
    /// Execute the statements in order
    Compound       (Vec<Stmt>),
    /// Declares a variable. A sort of 'lifting' is performed, akin to
    /// JavaScript, where declarations are moved to the top of the scope
    /// they are declared in. However, the initializer is only evaluated
    /// at the spot where the variable is actually declared
    Var            (VarDecl),
    /// If statement. Tests a condition and only follows at most one path
    If             (IfStmt),
    /// Switch statement. Jumps to a label if a set of conditions hold
    Switch         (SwitchStmt),
    /// Loop statement. Executes a block of code indefinitely.
    Loop           (LoopStmt),
    /// Repeatedly executes a block of code until a condition is no
    /// longer true.
    While          (WhileStmt),
    /// Executes a block of code once for every element of some iterable
    /// object. The exact semantics of iterability are not part of the
    /// grammar or parser itself.
    For            (ForStmt),
    /// Continues execution at the statement immediately following the
    /// innermost breakable block (loops and switch)
    Break,
    /// Continues execution at the start of the next iteration of the
    /// innermost continuable block (loops)
    Continue,
    /// Continues execution at the beginning of the current iteration of
    /// the innermost repeatable block (loops)
    Repeat,
    /// Returns from the containing function.
    Return         (Option<Expr<Primary>>)
}

pub struct IfStmt {
    pub cond:      Expr<Primary>,
    pub tb:        Box<Stmt>,
    pub fb:        Option<Box<Stmt>>,
}

pub struct SwitchStmt {
    pub ex:        Expr<Primary>,
    pub cases:     Vec<SwitchCase>,
}
pub enum SwitchCase {
    Case           (Expr<Primary>, Stmt),
    Default        (Stmt),
}

pub struct LoopStmt {
    pub body:      Box<Stmt>
}

pub struct WhileStmt {
    pub cond:      Expr<Primary>,
    pub body:      Box<Stmt>,
}

pub struct ForStmt {
    pub id:        Id,
    pub iter:      Expr<Primary>,
    pub body:      Box<Stmt>,
}

pub enum Primary {
    Path           (Path),
    Number         (Number),
}

impl Dumpable for Span {
    fn dump(&self, d: &mut DumpContext) {
        d.put_ln(format!("Span({},{} -> {},{})",
            self.start.line, self.start.col,
            self.end.line,   self.end.col
            ));
    }
}

impl Dumpable for Decl {
    fn dump(&self, d: &mut DumpContext) {
        d.push_str("Decl");
        d.put_ln(format!("pub: {}", self.is_pub));
        self.body.dump(d);
        self.span.dump(d);
        d.pop();
    }
}

impl Dumpable for DeclBody {
    fn dump(&self, d: &mut DumpContext) {
        match *self {
            DeclBody::Unit(ref x) => {
                d.push_str("DeclBody::Unit");
                x.dump(d);
            },
            DeclBody::Use(ref x) => {
                d.push_str("DeclBody::Use");
                x.dump(d);
            },
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

impl Dumpable for UnitDecl {
    fn dump(&self, d: &mut DumpContext) {
        d.push_str("UnitDecl");
        d.put_ln(format!("name: {}", self.name));
        for decl in self.decls.iter() {
            decl.dump(d);
        }
        d.pop();
    }
}

impl Dumpable for UseDecl {
    fn dump(&self, d: &mut DumpContext) {
        match *self {
            UseDecl::Single(ref p) => {
                d.push_str("UseDecl::Single");
                p.dump(d);
            },
            UseDecl::Many(ref p, ref ids) => {
                d.push_str("UseDecl::Many");
                p.dump(d);
                for id in ids.iter() {
                    d.put_ln(format!("{}", id));
                }
            },
            UseDecl::Aliased(ref p, ref id) => {
                d.push_str("UseDecl::Aliased");
                p.dump(d);
                d.put_ln(format!("alias: {}", id));
            },
            UseDecl::Glob(ref p) => {
                d.push_str("UseDecl::Glob");
                p.dump(d);
            },
        }
        d.pop();
    }
}

impl Dumpable for TypeDecl {
    fn dump(&self, d: &mut DumpContext) {
        d.push_str("TypeDecl");
        d.put_ln(format!("name: {}", self.name));
        self.typ.dump(d);
        d.pop();
    }
}

impl Dumpable for TypeSpec {
    fn dump(&self, d: &mut DumpContext) {
        /*match ::semantic::types::TypeRef::from_tree(self) {
            Ok(x)  => print!("\n--------> {:?}", x),
            Err(m) => print!("\n--(err)-> {}", m)
        }*/
        d.push_str("TypeSpec");
        self.body.dump(d);
        self.span.dump(d);
        d.pop();
    }
}

impl Dumpable for TypeBody {
    fn dump(&self, d: &mut DumpContext) {
        match *self {
            TypeBody::Alias(ref x) => {
                d.push_str("TypeBody::Alias");
                x.dump(d);
                d.pop();
            },
            TypeBody::Parameterized(ref x, ref y) => {
                d.push_str("TypeBody::Parameterized");
                x.dump(d);
                for t in y.iter() {
                    t.dump(d);
                }
                d.pop();
            },
            TypeBody::Pointer(ref x) => {
                d.push_str("TypeBody::Pointer");
                x.dump(d);
                d.pop();
            },
            TypeBody::Array(n, ref t) => {
                d.push_str("TypeBody::Array");
                d.put_ln(format!("size: {}", n));
                t.dump(d);
                d.pop();
            },
            TypeBody::Struct(ref x) => {
                d.push_str("TypeBody::Struct");
                for t in x.iter() {
                    t.dump(d);
                }
                d.pop();
            },
            TypeBody::Bitvec(ref x, ref y) => {
                d.push_str("TypeBody::Bitvec");
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

impl Dumpable for BitvecMember {
    fn dump(&self, d: &mut DumpContext) {
        match *self {
            BitvecMember::Literal(n, w) =>
                d.put_ln(format!("{} : {}", n, w)),
            BitvecMember::Variable(ref x, w) =>
                d.put_ln(format!("{} : {}", x, w)),
        }
    }
}

impl Dumpable for GlobalVarDecl {
    fn dump(&self, d: &mut DumpContext) {
        d.push_str("GlobalVarDecl");
        self.storage.dump(d);
        self.decl.dump(d);
        d.pop();
    }
}

impl Dumpable for Storage {
    fn dump(&self, d: &mut DumpContext) {
        d.push_str("Storage");
        self.loc.dump(d);
        for p in self.params.iter() {
            p.dump(d);
        }
        d.pop();
    }
}

impl Dumpable for StorageLoc {
    fn dump(&self, d: &mut DumpContext) {
        d.put_ln_str(match *self {
            StorageLoc::Default => "StorageLoc::Default",
            StorageLoc::RAM     => "StorageLoc::RAM",
            StorageLoc::ROM     => "StorageLoc::ROM",
        });
    }
}

impl Dumpable for StorageParam {
    fn dump(&self, d: &mut DumpContext) {
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

impl Dumpable for VarDecl {
    fn dump(&self, d: &mut DumpContext) {
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

impl Dumpable for ConstDecl {
    fn dump(&self, d: &mut DumpContext) {
        d.push_str("ConstDecl");
        d.put_ln(format!("id: {}", self.id));
        self.typ.dump(d);
        self.init.dump(d);
        d.pop();
    }
}

impl Dumpable for RegionDecl {
    fn dump(&self, d: &mut DumpContext) {
        d.push_str("RegionDecl");
        self.name.dump(d);
        for v in self.vars.iter() {
            v.dump(d);
        }
        d.pop();
    }
}

impl Dumpable for RegionName {
    fn dump(&self, d: &mut DumpContext) {
        d.put_ln(format!("region({}, {})", self.section, self.layer));
    }
}

impl Dumpable for FuncDecl {
    fn dump(&self, d: &mut DumpContext) {
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

impl Dumpable for FuncParam {
    fn dump(&self, d: &mut DumpContext) {
        for id in self.ids.iter() {
            d.push_str("FuncParam");
            d.put_ln(format!("id: {}", id));
            self.typ.dump(d);
            d.pop();
        }
    }
}

impl Dumpable for Stmt {
    fn dump(&self, d: &mut DumpContext) {
        d.push_str("Stmt");
        self.body.dump(d);
        self.span.dump(d);
        d.pop();
    }
}

impl Dumpable for StmtBody {
    fn dump(&self, d: &mut DumpContext) {
        match *self {
            StmtBody::Eval(ref ex) => {
                d.push_str("StmtBody::Eval");
                ex.dump(d);
                d.pop();
            },
            StmtBody::Compound(ref v) => {
                d.push_str("StmtBody::Compound");
                for st in v.iter() {
                    st.dump(d);
                }
                d.pop();
            },
            StmtBody::Var(ref decl) =>
                decl.dump(d),
            StmtBody::If(ref f) =>
                f.dump(d),
            StmtBody::Switch(ref s) =>
                s.dump(d),
            StmtBody::Loop(ref l) =>
                l.dump(d),
            StmtBody::While(ref w) =>
                w.dump(d),
            StmtBody::For(ref f) =>
                f.dump(d),
            StmtBody::Break =>
                d.put_ln_str("Stmt::Break"),
            StmtBody::Continue =>
                d.put_ln_str("Stmt::Continue"),
            StmtBody::Repeat =>
                d.put_ln_str("Stmt::Repeat"),
            StmtBody::Return(ref t) => {
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

impl Dumpable for IfStmt {
    fn dump(&self, d: &mut DumpContext) {
        d.push_str("IfStmt");
        self.cond.dump(d);
        self.tb.dump(d);
        match self.fb {
            Some(ref st)  => st.dump(d),
            None          => { },
        }
        d.pop();
    }
}

impl Dumpable for SwitchStmt {
    fn dump(&self, d: &mut DumpContext) {
        d.push_str("SwitchStmt");
        self.ex.dump(d);
        for c in self.cases.iter() {
            c.dump(d);
        }
        d.pop();
    }
}

impl Dumpable for SwitchCase {
    fn dump(&self, d: &mut DumpContext) {
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

impl Dumpable for LoopStmt {
    fn dump(&self, d: &mut DumpContext) {
        d.push_str("LoopStmt");
        self.body.dump(d);
        d.pop();
    }
}

impl Dumpable for WhileStmt {
    fn dump(&self, d: &mut DumpContext) {
        d.push_str("WhileStmt");
        self.cond.dump(d);
        self.body.dump(d);
        d.pop();
    }
}

impl Dumpable for ForStmt {
    fn dump(&self, d: &mut DumpContext) {
        d.push_str("ForStmt");
        d.put_ln(format!("id: {}", self.id));
        self.iter.dump(d);
        self.body.dump(d);
        d.pop();
    }
}

impl<P: Dumpable> Dumpable for Expr<P> {
    fn dump(&self, d: &mut DumpContext) {
        match *self {
            Expr::Comma(ref v) => {
                d.push_str("Expr::Comma");
                for ex in v.iter() {
                    ex.dump(d);
                }
                d.pop();
            },

            Expr::Assign(ref op, ref to, ref fr) => {
                d.push_str("Expr::Assign");
                to.dump(d);
                fr.dump(d);
                d.put_ln(format!("{:?}", op));
                d.pop();
            },

            Expr::Ternary(ref cond, ref tb, ref fb) => {
                d.push_str("Expr::Ternary");
                cond.dump(d);
                tb.dump(d);
                fb.dump(d);
                d.pop();
            },

            Expr::Binary(ref op, ref e1, ref e2) => {
                d.push_str("Expr::Binary");
                d.put_ln(format!("{:?}", op));
                e1.dump(d);
                e2.dump(d);
                d.pop();
            },

            Expr::Unary(ref op, ref ex) => {
                d.push_str("Expr::Unary");
                d.put_ln(format!("{:?}", op));
                ex.dump(d);
                d.pop();
            },

            Expr::Call(ref f, ref v) => {
                d.push_str("Expr::Call");
                f.dump(d);
                for ex in v.iter() {
                    ex.dump(d);
                }
                d.pop();
            },

            Expr::Member(ref ex, ref id) => {
                d.push_str("Expr::Member");
                ex.dump(d);
                d.put_ln(format!("member: {}", id));
                d.pop();
            },

            Expr::Primary(ref p) =>
                p.dump(d),
        }
    }
}

impl Dumpable for Path {
    fn dump (&self, d: &mut DumpContext) {
        if self.0.len() == 1 {
            d.put_ln(format!("Path({})", self.0[0]));
            return;
        }

        d.push_str("Path");
        for nm in self.0.iter() {
            d.put_ln(format!("{}", nm));
        }
        d.pop();
    }
}

impl Dumpable for Primary {
    fn dump (&self, d: &mut DumpContext) {
        match *self {
            Primary::Path(ref p) =>
                p.dump(d),

            Primary::Number(n) =>
                d.put_ln(format!("Primary::Number({})", n)),
        }
    }
}
