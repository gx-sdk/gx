// gx language implementation
// Copyright (C) 2014-present Alex Iadicicco <http://ajitek.net>
//
// For licensing information, refer to the COPYING file
// in the project root

//! Big list of tokens. In its own module because it's a really big list.

#[derive(Debug,PartialEq)]
pub enum Token {
    EOF,

    // parameterized token types

    Number(usize),
    Identifier(String),
    String(String),
    Character(char),

    // glyphs

    LParen,      // (
    RParen,      // )
    LBrack,      // [
    RBrack,      // ]
    LBrace,      // {
    RBrace,      // }

    Dot,         // .
    Comma,       // ,
    Semicolon,   // ;
    Question,    // ?

    Colon,       // :
    DblColon,    // ::

    LArr,        // <-
    RArr,        // ->
    RDblArr,     // =>

    Plus,        // +
    Minus,       // -
    Star,        // *
    Slash,       // /
    Mod,         // %

    Tilde,       // ~
    Amp,         // &
    Pipe,        // |
    Caret,       // ^
    LShift,      // <<
    RShift,      // >>

    Incr,        // ++
    Decr,        // --

    Excl,        // !
    DblAmp,      // &&
    DblPipe,     // ||

    Eq,          // ==
    NotEq,       // !=
    Less,        // <
    Greater,     // >
    LessEq,      // <=
    GreaterEq,   // >=

    Assign,      // =
    PlusEq,      // +=
    MinusEq,     // -=
    StarEq,      // *=
    SlashEq,     // /=
    ModEq,       // %=
    BitNotEq,    // ~=
    BitAndEq,    // &=
    BitOrEq,     // |=
    BitXorEq,    // ^=
    LShiftEq,    // <<=
    RShiftEq,    // >>=

    // keywords

    As,
    Bitvec,
    Break,
    Case,
    Const,
    Continue,
    Default,
    Else,
    For,
    Fn,
    If,
    In,
    Loop,
    Pub,
    Region,
    Repeat,
    Return,
    Ram,
    Rom,
    Sizeof,
    Struct,
    Switch,
    Type,
    Unit,
    Use,
    Var,
    While,
}

pub fn find_keyword(id: &str) -> Option<Token> {
    match id {
        "as"        => Some(Token::As),
        "bitvec"    => Some(Token::Bitvec),
        "break"     => Some(Token::Break),
        "case"      => Some(Token::Case),
        "const"     => Some(Token::Const),
        "continue"  => Some(Token::Continue),
        "default"   => Some(Token::Default),
        "else"      => Some(Token::Else),
        "for"       => Some(Token::For),
        "fn"        => Some(Token::Fn),
        "if"        => Some(Token::If),
        "in"        => Some(Token::In),
        "loop"      => Some(Token::Loop),
        "pub"       => Some(Token::Pub),
        "region"    => Some(Token::Region),
        "repeat"    => Some(Token::Repeat),
        "return"    => Some(Token::Return),
        "ram"       => Some(Token::Ram),
        "rom"       => Some(Token::Rom),
        "sizeof"    => Some(Token::Sizeof),
        "struct"    => Some(Token::Struct),
        "switch"    => Some(Token::Switch),
        "type"      => Some(Token::Type),
        "unit"      => Some(Token::Unit),
        "use"       => Some(Token::Use),
        "var"       => Some(Token::Var),
        "while"     => Some(Token::While),
        _           => None,
    }
}

#[derive(Clone,Copy)]
pub struct TokenAttr(&'static str, &'static str);

impl Token {
    pub fn attrs(&self) -> TokenAttr {
        match *self {
            Token::EOF =>           TokenAttr("end of file", "(eof)"),

            Token::Number(_) =>     TokenAttr("number literal", "(num)"),
            Token::Identifier(_) => TokenAttr("identifier", "(id)"),
            Token::String(_) =>     TokenAttr("string literal", "(str)"),
            Token::Character(_) =>  TokenAttr("character literal", "(char)"),

            Token::LParen =>        TokenAttr("'('",   "("),
            Token::RParen =>        TokenAttr("')'",   ")"),
            Token::LBrack =>        TokenAttr("'['",   "["),
            Token::RBrack =>        TokenAttr("']'",   "]"),
            Token::LBrace =>        TokenAttr("'{'",   "{"),
            Token::RBrace =>        TokenAttr("'}'",   "}"),

            Token::Dot =>           TokenAttr("'.'",   "."),
            Token::Comma =>         TokenAttr("','",   ","),
            Token::Semicolon =>     TokenAttr("';'",   ";"),
            Token::Question =>      TokenAttr("'?'",   "?"),

            Token::Colon =>         TokenAttr("':'",   ":"),
            Token::DblColon =>      TokenAttr("'::'",  "::"),

            Token::LArr =>          TokenAttr("'<-'",  "<-"),
            Token::RArr =>          TokenAttr("'->'",  "->"),
            Token::RDblArr =>       TokenAttr("'=>'",  "=>"),

            Token::Plus =>          TokenAttr("'+'",   "+"),
            Token::Minus =>         TokenAttr("'-'",   "-"),
            Token::Star =>          TokenAttr("'*'",   "*"),
            Token::Slash =>         TokenAttr("'/'",   "/"),
            Token::Mod =>           TokenAttr("'%'",   "%"),

            Token::Tilde =>         TokenAttr("'~'",   "~"),
            Token::Amp =>           TokenAttr("'&'",   "&"),
            Token::Pipe =>          TokenAttr("'|'",   "|"),
            Token::Caret =>         TokenAttr("'^'",   "^"),
            Token::LShift =>        TokenAttr("'<<'",  "<<"),
            Token::RShift =>        TokenAttr("'>>'",  ">>"),

            Token::Incr =>          TokenAttr("'++'",  "++"),
            Token::Decr =>          TokenAttr("'--'",  "--"),

            Token::Excl =>          TokenAttr("'!'",   "!"),
            Token::DblAmp =>        TokenAttr("'&&'",  "&&"),
            Token::DblPipe =>       TokenAttr("'||'",  "||"),

            Token::Eq =>            TokenAttr("'=='",  "=="),
            Token::NotEq =>         TokenAttr("'!='",  "!="),
            Token::Less =>          TokenAttr("'<'",   "<"),
            Token::Greater =>       TokenAttr("'>'",   ">"),
            Token::LessEq =>        TokenAttr("'<='",  "<="),
            Token::GreaterEq =>     TokenAttr("'>='",  ">="),

            Token::Assign =>        TokenAttr("'='",   "="),
            Token::PlusEq =>        TokenAttr("'+='",  "+="),
            Token::MinusEq =>       TokenAttr("'-='",  "-="),
            Token::StarEq =>        TokenAttr("'*='",  "*="),
            Token::SlashEq =>       TokenAttr("'/='",  "/="),
            Token::ModEq =>         TokenAttr("'%='",  "%="),
            Token::BitNotEq =>      TokenAttr("'~='",  "~="),
            Token::BitAndEq =>      TokenAttr("'&='",  "&="),
            Token::BitOrEq =>       TokenAttr("'|='",  "|="),
            Token::BitXorEq =>      TokenAttr("'^='",  "^="),
            Token::LShiftEq =>      TokenAttr("'<<='", "<<="),
            Token::RShiftEq =>      TokenAttr("'>>='", ">>="),

            Token::As =>            TokenAttr("'as'", "as"),
            Token::Bitvec =>        TokenAttr("'bitvec'", "bitvec"),
            Token::Break =>         TokenAttr("'break'", "break"),
            Token::Case =>          TokenAttr("'case'", "case"),
            Token::Const =>         TokenAttr("'const'", "const"),
            Token::Continue =>      TokenAttr("'continue'", "continue"),
            Token::Default =>       TokenAttr("'default'", "default"),
            Token::Else =>          TokenAttr("'else'", "else"),
            Token::For =>           TokenAttr("'for'", "for"),
            Token::Fn =>            TokenAttr("'fn'", "fn"),
            Token::If =>            TokenAttr("'if'", "if"),
            Token::In =>            TokenAttr("'in'", "in"),
            Token::Loop =>          TokenAttr("'loop'", "loop"),
            Token::Pub =>           TokenAttr("'pub'", "pub"),
            Token::Region =>        TokenAttr("'region'", "region"),
            Token::Repeat =>        TokenAttr("'repeat'", "repeat"),
            Token::Return =>        TokenAttr("'return'", "return"),
            Token::Ram =>           TokenAttr("'ram'", "ram"),
            Token::Rom =>           TokenAttr("'rom'", "rom"),
            Token::Sizeof =>        TokenAttr("'sizeof'", "sizeof"),
            Token::Struct =>        TokenAttr("'struct'", "struct"),
            Token::Switch =>        TokenAttr("'switch'", "switch"),
            Token::Type =>          TokenAttr("'type'", "type"),
            Token::Unit =>          TokenAttr("'unit'", "unit"),
            Token::Use =>           TokenAttr("'use'", "use"),
            Token::Var =>           TokenAttr("'var'", "var"),
            Token::While =>         TokenAttr("'while'", "while"),
        }
    }

    pub fn to_repr<'a>(&'a self) -> String {
        match *self {
            Token::Number(x) => x.to_string(),
            Token::Identifier(ref x) => x.clone(),
            Token::String(ref x) => format!("\"{}\"", escape_str(&x[..])),
            Token::Character(x) => format!("'{}'", escape_char(x)),
            _ => match self.attrs() { TokenAttr(_, x) => x.to_owned() }
        }
    }
}

fn escape_char(c: char) -> String {
    let s = String::new();
    c.escape_default();
    return s
}

fn escape_str(s: &str) -> String {
    let t = String::new();
    for c in s.chars() { c.escape_default(); }
    return t;
}
