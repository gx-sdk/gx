// Copyright (C) 2015 Alex Iadicicco

//! Instructions. The structure here has been chosen in such a way to make
//! strength reduction optimizations easy. Strength reduction and clever
//! language design are the two most important kinds of optimization for the
//! project.

use std::fmt;

/// Register number. May eventually be range limited, but not that likely.
pub type Reg = u8;

/// Effective address.
pub enum EA {
    Data           (Reg),
    Addr           (Reg),
    IndAddr        (Reg),
    IndAddrPost    (Reg),
    IndAddrPre     (Reg),
    IndAddrDisp    (Reg, u16),
    IndPC          (u16),
    AbsShort       (u32),
    AbsLong        (u32),
    Imm            (u32),
    List           (u16),
    SpecialCCR,
    SpecialSR,
}

/// Instruction Size
pub enum Size {
    Byte,
    Short,
    Long,
}

/// Condition Code
pub enum CC {
    T,
}

impl fmt::Debug for EA {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        use self::EA::*;

        match *self {
            Data(n)            => f.write_fmt(format_args!("d{}", n)),
            Addr(n)            => f.write_fmt(format_args!("a{}", n)),
            IndAddr(n)         => f.write_fmt(format_args!("(a{})", n)),
            IndAddrPost(n)     => f.write_fmt(format_args!("(a{})+", n)),
            IndAddrPre(n)      => f.write_fmt(format_args!("-(a{})", n)),
            IndAddrDisp(n, d)  => f.write_fmt(format_args!("({},a{})", d, n)),
            IndPC(d)           => f.write_fmt(format_args!("({},pc)", d)),
            AbsShort(x)        => f.write_fmt(format_args!("({}).w", x)),
            AbsLong(x)         => f.write_fmt(format_args!("({}).l", x)),
            Imm(x)             => f.write_fmt(format_args!("#{}", x)),
            List(x)            => f.write_str("<list>"),
            SpecialCCR         => f.write_str("ccr"),
            SpecialSR          => f.write_str("sr"),
        }
    }
}

pub type Label = String;

/// Instruction
pub enum Inst {
    // data movement instructions
    EXG            (EA, EA),
    LEA            (EA, EA),
    LINK           (Size, EA, EA),
    MOVE           (Size, EA, EA),
    MOVEM          (Size, EA, EA),
    PEA            (EA),
    UNLK           (EA),

    // integer arithmetic operation
    ADD            (EA, EA),
    ADDX           (EA, EA),
    CLR            (EA),
    CMP            (EA, EA),
    DIVS           (EA, EA),
    DIVU           (EA, EA),
    EXT            (EA),
    MULS           (EA, EA),
    MULU           (EA, EA),
    NEG            (EA),
    NEGX           (EA),
    SUB            (EA, EA),
    SUBX           (EA, EA),

    // logical operation
    AND            (EA, EA),
    EOR            (EA, EA),
    NOT            (EA),
    OR             (EA, EA),

    // shift and rotate
    ASL            (EA, EA),
    ASR            (EA, EA),
    LSL            (EA, EA),
    LSR            (EA, EA),
    ROL            (EA, EA),
    ROR            (EA, EA),
    ROXL           (EA, EA),
    ROXR           (EA, EA),
    SWAP           (EA),

    // bit manipulation
    BCHG           (EA, EA),
    BCLR           (EA, EA),
    BSET           (EA, EA),
    BTST           (EA, EA),

    // binary-coded decimal instructions
    ABCD           (EA, EA),
    NBCD           (EA, EA),
    SBCD           (EA, EA),

    // program control instructions
    Bcc            (CC, Label),
    BRA            (Label),
    BSR            (Label),
    DBcc           (CC, EA, Label),
    JMP            (EA),
    JSR            (EA),
    NOP,
    RTR,
    RTS,
    Scc            (CC, EA),
    TST            (EA),

    // system control instructions
    CHK            (EA, EA),
    ILLEGAL,
    RTE,
    TRAP           (EA),
    TRAPV,
}
