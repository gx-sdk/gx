/* intermediate representation, a cousin of 3AC and inspired by LLVM */

/* A little bit about the design philosophy behind this IR,

   There are a number of important features that any good IR should have:

     (a) It should be easily generated by the frontend, (so not a 1-1 map to
         backend code, since that defeats the purpose of an IR at all, as now
         the frontend has to perform backend tasks like register allocation.)

     (b) It should be easily processed by the backend, (so not a 1-1 map of
         the frontend AST, since that also defeats the purpose of an IR.)

     (c) Some form of optimization should be possible. Without the possibility
         of optimization, the IR just becomes a halfway point between the
         frontend and backend. Although this in itself would be good for
         design, chances are there's a better IR that can be used that allows
         the opportunity for optimization. */

/* Another challenge with the IR of this particular target system is the
   incredibly odd way certain memory spaces are accessed. The FM and VDP
   chips are programmed first by writing an "address" register, and then a
   "value" register. In the case of the VDP, you can write multiple values
   at a fixed spacing as well. Half of the Z80's address space is a banked
   region of the 68000 address space that is programmed by writing the 9
   lower order bits, LSB first, one at a time to a particular region of memory.

   This mixed nature is not unique to the Genesis, but it is very different
   from those parts of the kinds of systems compilers are typically targeting.
   At the lowest level, a lot of operations will basically be writing magic
   values to magic spots in memory. Optimizing the code at this level would be
   very difficult. The IR needs a way to represent the more abstract parts
   of the system's memory model. */

/* The basic currency of this IR is the Value. All Values have a Type
   associated with them, which affects the kinds of things they can be used
   for, and how they are transformed into other Values. Values can also have
   a Location, which may or may not be fixed in memory. Location is a very
   complicated notion, as it can also be used to represent system memory not
   directly addressable, such as VRAM. Values can also have a static value,
   which allows certain basic optimizations such as constant folding. */

/* Values can be combined via Instructions. A dependency graph of Instructions
   forms a BasicBlock. */

/* A graph of BasicBlocks is a control flow graph (CFG), and is the body of a
   Function. */

/* A Module is a named collection of named Functions, named Locations,
   and blobs of static data, targeting a single backend. A Genesis ROM,
   represented by a Program, is a group of 1 or more 68000 Modules, and 0
   or more Z80 Modules. */

extern crate arena;

use self::arena::TypedArena;
use std::cell::Cell;
use std::cell::RefCell;

#[deriving(PartialEq)]
pub enum Operation {
    OpAdd,
    OpSubtract,
    OpMultiply,
    OpDivide,

    OpPrint,
    OpTestNonzero,
    OpAssign,
}

pub enum Value<'a> {
    Constant(uint),
    Location(String),
    InstResult(&'a Instruction<'a>),
}

pub struct Instruction<'a> {
    pub num:  uint,
    pub opr:  Operation,
    pub opn:  Vec<Value<'a>>,
}

impl<'a> Instruction<'a> {
    pub fn is_terminator(&self) -> bool {
        self.opr == OpTestNonzero
    }
}

pub struct BasicBlock<'a> {
    pub function:  &'a Function<'a>,
    pub num:       uint,

    pub insts:     RefCell<Vec<&'a Instruction<'a>>>,

    pub on_true:   Cell<Option<&'a BasicBlock<'a>>>,
    pub on_false:  Cell<Option<&'a BasicBlock<'a>>>,
}

pub struct Function<'a> {
    blocks:        TypedArena<BasicBlock<'a>>,
    num_blocks:    Cell<uint>,
    entry:         Cell<Option<&'a BasicBlock<'a>>>,

    insts:         TypedArena<Instruction<'a>>,
}

impl<'a> BasicBlock<'a> {
    pub fn new(f: &'a Function<'a>) -> BasicBlock<'a> {
        BasicBlock {
            function:  f,
            num:       f.num_blocks.get(),
            insts:     RefCell::new(Vec::new()),
            on_true:   Cell::new(None),
            on_false:  Cell::new(None),
        }
    }

    pub fn set_true(&self, branch: Option<&'a BasicBlock<'a>>) {
        self.on_true.set(branch);
    }

    pub fn set_false(&self, branch: Option<&'a BasicBlock<'a>>) {
        self.on_false.set(branch);
    }

    pub fn add_inst(
        &self,
        opr: Operation,
        opn: Vec<Value<'a>>
    ) -> &'a Instruction {
        let next_num = self.insts.borrow().len();
        let mut v = self.insts.borrow_mut();
        v.push(self.function.add_inst(Instruction {
            num: next_num,
            opr: opr,
            opn: opn,
        }));
        v[v.len() - 1]
    }
}

impl<'a> Function<'a> {
    pub fn new() -> Function<'a> {
        Function {
            blocks:      TypedArena::new(),
            num_blocks:  Cell::new(0),
            entry:       Cell::new(None),

            insts:       TypedArena::new(),
        }
    }

    pub fn add_block(&self) -> &'a BasicBlock {
        self.num_blocks.set(self.num_blocks.get() + 1);
        self.blocks.alloc(BasicBlock::new(self))
    }

    pub fn add_inst(&self, i: Instruction<'a>) -> &'a Instruction {
        self.insts.alloc(i)
    }

    pub fn set_entry(&self, bb: Option<&'a BasicBlock<'a>>) {
        self.entry.set(bb);
    }

    pub fn get_entry(&self) -> Option<&'a BasicBlock<'a>> {
        self.entry.get()
    }
}
