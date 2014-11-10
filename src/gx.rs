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

    fn dump_bb(bb: &ir::BasicBlock) {
        println!("block{}:", bb.num);

        for x in bb.insts.borrow().iter() {
            match x.opr {
                ir::OpAdd          => print!("  %{} = add", x.num),
                ir::OpSubtract     => print!("  %{} = sub", x.num),
                ir::OpMultiply     => print!("  %{} = mul", x.num),
                ir::OpDivide       => print!("  %{} = div", x.num),

                ir::OpPrint        => print!("  print"),
                ir::OpTestNonzero  => print!("  test"),
                ir::OpAssign       => print!("  assign"),
            }

            for v in x.opn.iter() {
                match *v {
                    ir::Constant(c)     => print!(" {}", c),
                    ir::Location(ref s) => print!(" {}", s),
                    ir::InstResult(i)   => print!(" %{}", i.num),
                }
            }

            print!("\n");
        }

        match bb.on_false.get() {
            Some(fb) => {
                println!("  bf block{}", fb.num);
                if fb.num == bb.num + 1 {
                    dump_bb(fb);
                }
            },

            None => { },
        }

        match bb.on_true.get() {
            Some(tb) => {
                if tb.num == bb.num + 1 {
                    dump_bb(tb);
                } else {
                    println!("  j block{}", tb.num);
                }
            },

            None =>
                println!("  exit"),
        }
    }

    fn dump_ir(f: &ir::Function) {
        match f.get_entry() {
            Some(bb) => dump_bb(bb),
            None => println!("function has no entry BasicBlock!"),
        }
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

        /* dump IR */
        dump_ir(&f);
    }

}

fn main() {
    driver::main(std::os::args());
}
