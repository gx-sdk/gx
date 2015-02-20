// gx language implementation
// Copyright (C) 2014-present Alex Iadicicco <http://ajitek.net>
//
// For licensing information, refer to the COPYING file
// in the project root

#![unstable = "awaiting end-to-end implementation"]

//! This crate and all its modules are the components of the reference
//! implementation of the `gx` language. A small driver program is provided
//! as well that is invoked by the `gx` executable.

pub mod frontend;
pub mod semantic;
pub mod expr;
pub mod backend;

mod driver {
    extern crate getopts;

    use std::old_io::stdin;
    use std::old_io::Chars;
    use std::old_io::BufferedReader;
    use std::old_io::stdio::StdReader;
    use frontend::tree::*;
    use frontend::lexer;
    use frontend::parser;
    use semantic::types;

    fn parse(ch: Chars<BufferedReader<StdReader>>) -> Input {
        parser::Parser::new(lexer::Lexer::new(ch)).file()
    }

    pub fn main() {
        /*
        let matches = match getopts::getopts(args.tail(), [
            getopts::optopt("o", "", "set output file name", "NAME"),
        ]) {
            Ok(m)   => m,
            Err(e)  => panic!(e.to_string()),
        };

        let output = match matches.opt_str("o") {
            None    => String::from_str("a.out"),
            Some(x) => x,
        };
        */

        // read an Input from the stream
        let mut s = stdin();
        let f = parse(s.lock().chars());
        let mut d = DumpContext::new();
        for unit in f.iter() {
            unit.dump(&mut d);
        }
        d.end();
    }

}

fn main() {
    driver::main();
}
