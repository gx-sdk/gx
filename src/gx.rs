#![feature(globs)]

pub mod compiler;

mod driver {
    extern crate getopts;

    use std::io::stdin;
    use compiler::tree::*;
    use compiler::lexer;
    use compiler::parser;

    /*fn parse<B: Buffer>(mut f: B) -> StatementList {
        parser::Parser::new(lexer::Lexer::new(f.chars())).stmt_list()
    }*/

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

        /* read a statement list from the stream */
        /*let st = parse(stdin());*/
    }

}

fn main() {
    driver::main();
}
