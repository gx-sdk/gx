#![feature(globs)]
#![feature(struct_variant)]

pub mod compiler;

mod driver {
    extern crate getopts;

    use std::io::stdin;
    use compiler::tree::*;
    use compiler::ir;
    use compiler::lexer;
    use compiler::parser;
    use compiler::front;

    fn parse<B: Buffer>(mut f: B) -> StatementList {
        parser::Parser::new(lexer::Lexer::new(f.chars())).stmt_list()
    }

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

        /* read a statement list from the stream */
        let st = parse(stdin());

        /* build IR */
        let f = ir::Function::new();
        front::stmt_list_to_ir(&st, &f);
    }

}

fn main() {
    driver::main(std::os::args());
}
