mod compiler {
    pub mod lexer;
}

mod driver {
    extern crate getopts;

    use std::io::stdin;
    use compiler::lexer;

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
        for x in lexer::Lexer::new(stdin().chars()) {
            print!("{} ", x.to_repr());
        }
        println!("(eof)");
    }

}

fn main() {
    driver::main(std::os::args());
}
