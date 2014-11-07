#![feature(globs)]

pub mod compiler;

mod driver {
    extern crate getopts;

    use std::io::stdin;
    use std::collections::TreeMap;
    use compiler::tree::*;
    use compiler::lexer;
    use compiler::parser;

    fn parse<B: Buffer>(mut f: B) -> StatementList {
        parser::Parser::new(lexer::Lexer::new(f.chars())).stmt_list()
    }

    fn eval_opn<'a>(opn: &'a Operand, mem: &TreeMap<&'a str, uint>) -> uint {
        match *opn {
            Operand::OpnExpression(box ref ex) =>
                eval_expr(ex, mem),
            Operand::OpnConstant(c) =>
                c,
            Operand::OpnIdentifier(ref id) => match mem.find(&id.as_slice()) {
                Some(v)  => *v,
                None     =>  0,
            },

            _ => 0,
        }
    }

    fn eval_expr<'a>(expr: &'a Expression, mem: &TreeMap<&'a str, uint>) -> uint {
        expr.opr.apply(
            eval_opn(&expr.opn0, mem),
            eval_opn(&expr.opn1, mem)
        )
    }

    fn eval_stmt<'a>(stmt: &'a Statement, mem: &mut TreeMap<&'a str, uint>) {
        match *stmt {
            Statement::Print(ref expr) => {
                println!("{}", eval_expr(expr, mem));
            },

            Statement::IfBlock(ref expr, box ref stmts) => {
                if eval_expr(expr, mem) != 0 {
                    eval(stmts, mem)
                };
            },

            Statement::Assign(ref to, ref expr) => {
                let x = eval_expr(expr, mem);
                mem.insert(to.as_slice(), x);
            },
        }
    }

    fn eval<'a>(stmts: &'a StatementList, mem: &mut TreeMap<&'a str, uint>) {
        let mut cur = stmts;

        while !cur.is_nil() {
            match *cur {
                StatementList::Node(ref st, box ref next) => {
                    cur = next;
                    eval_stmt(st, mem);
                },

                _ => { /* ICE!! */ }
            }
        }
    }

    fn eval_stmts<'a>(stmts: &'a StatementList) {
        let ref mut mem = TreeMap::new();

        eval(stmts, mem)
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

        /* evaluate it! */
        eval_stmts(&st);
    }

}

fn main() {
    driver::main(std::os::args());
}
