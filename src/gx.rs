// gx language implementation
// Copyright (C) 2014-present Alex Iadicicco <http://ajitek.net>
//
// For licensing information, refer to the COPYING file
// in the project root

#![unstable = "awaiting end-to-end implementation"]

// These were added to shut rustc up. They need to be removed eventually and
// dealt with accordingly.
#![feature(collections)]
#![feature(core)] // required for FromStrRadix in frontend::lexer::read_int()
#![feature(old_io)]
#![feature(unicode)]
#![feature(env)] // required for std::env::args()

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
    use std::env;
    use frontend::tree::*;
    use frontend::lexer;
    use frontend::parser;

    fn parse(ch: Chars<BufferedReader<StdReader>>) -> Input {
        parser::Parser::new(lexer::Lexer::new(ch)).file()
    }

    fn dump(input: &Input) {
        let mut d = DumpContext::new();
        for unit in input {
            unit.dump(&mut d);
        }
        d.end();
    }

    pub fn main() {
        let mut opts = getopts::Options::new();

        // opts.optopt("o", "", "set output file name", "NAME");
        opts.optflag("", "dump", "dump parse tree");

        let args: Vec<String> = env::args().collect();
        let matches = match opts.parse(args.tail()) {
            Ok(m)   => m,
            Err(e)  => panic!(e.to_string()),
        };

        // read an Input from the stream
        let mut s = stdin();
        let f = parse(s.lock().chars());

        if matches.opt_present("dump") {
            dump(&f);
        }

        // let output = match matches.opt_str("o") {
        //     None    => String::from_str("a.out"),
        //     Some(x) => x,
        // };
    }
}

fn main() {
    driver::main();
}
