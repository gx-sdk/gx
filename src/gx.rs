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
#![feature(io)]
#![feature(unicode)]
#![feature(exit_status)] // for std::env::set_exit_status

//! This crate and all its modules are the components of the reference
//! implementation of the `gx` language. A small driver program is provided
//! as well that is invoked by the `gx` executable.

pub mod frontend;
pub mod semantic;
pub mod expr;
pub mod backend;

mod driver {
    extern crate getopts;

    use std::env;
    use std::error;
    use std::fmt;
    use std::fs;
    use std::io;
    use frontend::tree::*;
    use frontend::lexer;
    use frontend::parser;

    pub enum Error {
        IO         (String, io::Error), // (Path, Error)
        GetOpts    (getopts::Fail),
    }

    impl fmt::Display for Error {
        fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
            use self::Error::*;
            match *self {
                IO(ref p, ref e) => write!(f, "Could not read file `{}`: {}", p, e),
                GetOpts(ref e) => write!(f, "Invalid invocation of gx: {}", e),
            }
        }
    }

    impl error::FromError<getopts::Fail> for Error {
        fn from_error(err: getopts::Fail) -> Error {
            Error::GetOpts(err)
        }
    }

    fn parse(ch: io::Chars<io::BufReader<fs::File>>) -> Input {
        parser::Parser::new(lexer::Lexer::new(ch)).file()
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

        let args: Vec<String> = env::args().collect();
        let matches = try!(opts.parse(args.tail()));

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
            use std::io::ReadExt;
            let mut p = parse(io::BufReader::new(file).chars());
            inputs.append(&mut p);
        }

        if matches.opt_present("dump") {
            dump(&inputs);
        }

        Ok(())
    }
}

fn main() {
    let result = driver::main();
    if let Err(e) = result {
        println!("{}", e);
        std::env::set_exit_status(1);
    }
}
