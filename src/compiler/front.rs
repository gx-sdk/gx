/* converts parse tree to IR */

use compiler::tree::*;
use compiler::ir::*;

pub fn opn_to_value<'a, 'b>(
    opn: &'a Operand,
    bb:  &'b BasicBlock<'b>
) -> Value<'b> {
    match *opn {
        Operand::Expression(opr, box ref opn0, box ref opn1) => {
            let v0 = opn_to_value(opn0, bb);
            let v1 = opn_to_value(opn1, bb);

            InstResult(bb.add_inst(
                match opr {
                    Operator::Add       => OpAdd,
                    Operator::Subtract  => OpSubtract,
                    Operator::Multiply  => OpMultiply,
                    Operator::Divide    => OpDivide,
                },

                vec!(v0, v1),
            ))
        },

        Operand::Constant(c) =>
            Constant(c),

        Operand::Identifier(ref id) =>
            Location(id.clone()),

        Operand::Nothing => {
            println!("warning: encountered Operand::Nothing");
            Constant(0)
        }
    }
}

pub fn add_stmt_to_cfg<'a, 'b>(
    st:  &'a Statement,
    bb:  &'b BasicBlock<'b>
) -> &'b BasicBlock<'b> {
    match *st {
        Statement::Print(ref opn) => {
            bb.add_inst(
                OpPrint,
                vec!(opn_to_value(opn, bb)),
            );
            return bb;
        },

        Statement::IfBlock(ref opn, box ref stmts) => {
            bb.add_inst(
                OpTestNonzero,
                vec!(opn_to_value(opn, bb))
            );
            let fb = bb.function.add_block();
            let tb = bb.function.add_block();
            add_stmts_to_cfg(stmts, tb);
            tb.set_true(Some(fb));
            bb.set_true(Some(fb));
            return fb;
        },

        Statement::Assign(ref to, ref opn) => {
            bb.add_inst(
                OpAssign,
                vec!(
                    Location(to.clone()),
                    opn_to_value(opn, bb)
                ),
            );
            return bb;
        },
    }
}

pub fn add_stmts_to_cfg<'a, 'b>(
    st:  &'a StatementList,
    bb:  &'b BasicBlock<'b>
) -> &'b BasicBlock<'b> {
    let mut exit = bb;

    for cur in st.iter() {
        exit = add_stmt_to_cfg(&**cur, exit);
    }

    return exit;
}

pub fn stmt_list_to_ir<'a, 'b>(st: &'a StatementList, f: &'b Function<'b>) {
    add_stmts_to_cfg(st, f.add_block());
}
