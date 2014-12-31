#![feature(globs)]

pub mod frontend;

mod driver {
    extern crate getopts;

    use std::io::stdin;
    use std::io::Chars;
    use std::io::BufferedReader;
    use std::io::stdio::StdReader;
    use frontend::tree::*;
    use frontend::lexer;
    use frontend::parser;

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

        /* read an Input from the stream */
        let f = parse(stdin().lock().chars());
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
