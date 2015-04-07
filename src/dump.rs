// gx language implementation
// Copyright (C) 2014-present Alex Iadicicco <http://ajitek.net>
//
// For licensing information, refer to the COPYING file
// in the project root

//! Dumping

#[derive(Clone,Copy)]
pub struct DumpContext {
    pub blank: bool,
    pub depth: isize,
}

pub trait Dumpable {
    fn dump(&self, d: &mut DumpContext);
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
        for _ in (0..self.depth) {
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

