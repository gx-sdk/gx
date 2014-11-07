#![feature(globs)]

pub mod compiler;

mod driver {
    extern crate getopts;

    use std::io::stdin;
    use compiler::lexer;
    use compiler::parser;

    pub fn main(args: Vec<String>) {
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

        println!("write something:");

        /* read an expr from the stream */
        let ex = parser::Parser::new(lexer::Lexer::new(stdin().chars())).expr();

        if ex.is_constant() {
            println!(" = {}", ex.get_value());
        } else {
            println!("expression not constant");
        }
    }

}

fn main() {
    driver::main(std::os::args());
}
