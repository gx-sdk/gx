// gx language implementation
// Copyright (C) 2014-present Alex Iadicicco <http://ajitek.net>
//
// For licensing information, refer to the COPYING file
// in the project root

// These were added to shut rustc up. They need to be removed eventually and
// dealt with accordingly.
#![feature(io)]

//! This crate and all its modules are the components of the reference
//! implementation of the `gx` language. A small driver program is provided
//! as well that is invoked by the `gx` executable.

pub mod backend;
pub mod dump;
pub mod expr;
pub mod frontend;
pub mod msg;
pub mod semantic;

mod driver {
    extern crate getopts;

    use std::env;
    use std::convert;
    use std::fmt;
    use std::fs;
    use std::io;

    use dump::*;
    use frontend::lexer;
    use frontend::parser;
    use frontend::tree::*;
    use msg;
    use semantic::unit;

    pub enum Error {
        Messages   (msg::MessageList),
        IO         (String, io::Error), // (Path, Error)
        GetOpts    (getopts::Fail),
    }

    impl fmt::Display for Error {
        fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
            use self::Error::*;
            match *self {
                IO(ref p, ref e) => write!(f, "Could not read file `{}`: {}", p, e),
                GetOpts(ref e) => write!(f, "Invalid invocation of gx: {}", e),
                Messages(ref m) => write!(f, "{}Compile failed", m),
            }
        }
    }

    impl convert::From<getopts::Fail> for Error {
        fn from(err: getopts::Fail) -> Error {
            Error::GetOpts(err)
        }
    }

    fn parse(ch: io::Chars<io::BufReader<fs::File>>) -> parser::Result<Input> {
        let nm = "<input>".to_string();
        parser::Parser::new(lexer::Lexer::new(nm, ch)).file()
    }

    fn dump(input: &Input) {
        let mut d = DumpContext::new();
        for unit in input {
            unit.dump(&mut d);
        }
        d.end();
    }

    pub fn main() -> Result<(), Error> {
        let mut opts = getopts::Options::new();

        opts.optflag("", "dump", "dump parse tree");

        let args: Vec<String> = env::args().skip(1).collect();
        let matches = try!(opts.parse(args));

        // Ensure all files can be read before proceeding
        let mut files = Vec::new();
        for path in &matches.free {
            let file = match fs::File::open(path) {
                Ok(f) => f,
                Err(e) => { return Err(Error::IO(path.clone(), e)) },
            };
            files.push(file);
        }

        let mut inputs = Vec::new();
        for file in files {
            use std::io::Read;
            let mut p = match parse(io::BufReader::new(file).chars()) {
                Ok(p) => p,
                Err(m) => return Err(Error::Messages(m))
            };
            inputs.append(&mut p);
        }

        if matches.opt_present("dump") {
            dump(&inputs);
        }

        let prog = match unit::Unit::from_tree_input(&inputs) {
            Ok(p) => p,
            Err(m) => return Err(Error::Messages(m)),
        };

        let mut d = DumpContext::new();
        prog.dump(&mut d);
        d.end();

        Ok(())
    }
}

fn main() {
    if let Err(e) = driver::main() {
        panic!("exited with error: {}", e);
    }
}
