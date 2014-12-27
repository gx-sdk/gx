#![feature(globs)]

pub mod compiler;

mod driver {
    extern crate getopts;

    use std::io::stdin;
    use std::io::Chars;
    use std::io::BufferedReader;
    use std::io::stdio::StdReader;
    use compiler::tree::*;
    use compiler::lexer;
    use compiler::parser;

    fn parse(mut ch: Chars<BufferedReader<StdReader>>) -> Input {
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

        /* read an Input from the stream */
        let f = parse(stdin().lock().chars());
    }

}

fn main() {
    driver::main();
}
