#[deriving(Show,PartialEq)]
pub enum Token {
    /* this will never be yielded from the lexer; it is used internally to
       indicate that another iteration of the input loop is needed */
    Ignore,

    /* parameterized token types */

    Number(uint),
    Identifier(String),
    String(String),
    Character(char),

    /* glyphs */

    LParen,      /*  (  */
    RParen,      /*  )  */
    LBrack,      /*  [  */
    RBrack,      /*  ]  */
    LBrace,      /*  {  */
    RBrace,      /*  }  */

    Dot,         /*  .  */
    Comma,       /*  ,  */
    Colon,       /*  :  */
    Semicolon,   /*  ;  */
    Question,    /*  ?  */

    LArr,        /*  <- */
    RArr,        /*  -> */
    RDblArr,     /*  => */

    Plus,        /*  +  */
    Minus,       /*  -  */
    Star,        /*  *  */
    Slash,       /*  /  */
    Mod,         /*  %  */

    BitNot,      /*  ~  */
    BitAnd,      /*  &  */
    BitOr,       /*  |  */
    BitXor,      /*  ^  */
    LShift,      /*  << */
    RShift,      /*  >> */

    Incr,        /*  ++ */
    Decr,        /*  -- */

    BoolNot,     /*  !  */
    BoolAnd,     /*  && */
    BoolOr,      /*  || */

    Eq,          /*  == */
    NotEq,       /*  != */
    Less,        /*  <  */
    Greater,     /*  >  */
    LessEq,      /*  <= */
    GreaterEq,   /*  >= */

    Assign,      /*  =  */
    PlusEq,      /*  += */
    MinusEq,     /*  -= */
    StarEq,      /*  *= */
    SlashEq,     /*  /= */
    ModEq,       /*  %= */
    BitNotEq,    /*  ~= */
    BitAndEq,    /*  &= */
    BitOrEq,     /*  |= */
    BitXorEq,    /*  ^= */
    LShiftEq,    /* <<= */
    RShiftEq,    /* >>= */

    /* keywords */

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
    Loop,
    Pub,
    Region,
    Repeat,
    Return,
    Ram,
    Rom,
    Sizeof,
    Struct,
    Unit,
    Var,
    While,
}

pub fn find_keyword(id: &str) -> Option<Token> {
    match id {
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
        "loop"      => Some(Token::Loop),
        "pub"       => Some(Token::Pub),
        "region"    => Some(Token::Region),
        "repeat"    => Some(Token::Repeat),
        "return"    => Some(Token::Return),
        "ram"       => Some(Token::Ram),
        "rom"       => Some(Token::Rom),
        "sizeof"    => Some(Token::Sizeof),
        "struct"    => Some(Token::Struct),
        "unit"      => Some(Token::Unit),
        "var"       => Some(Token::Var),
        "while"     => Some(Token::While),
        _           => None,
    }
}

pub struct TokenAttr(&'static str, &'static str);

impl Token {
    pub fn attrs(&self) -> TokenAttr {
        match *self {
            Token::Ignore =>        TokenAttr("Token::Ignore", "Token::Ignore"),

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
            Token::Colon =>         TokenAttr("':'",   ":"),
            Token::Semicolon =>     TokenAttr("';'",   ";"),
            Token::Question =>      TokenAttr("'?'",   "?"),

            Token::LArr =>          TokenAttr("'<-'",  "<-"),
            Token::RArr =>          TokenAttr("'->'",  "->"),
            Token::RDblArr =>       TokenAttr("'=>'",  "=>"),

            Token::Plus =>          TokenAttr("'+'",   "+"),
            Token::Minus =>         TokenAttr("'-'",   "-"),
            Token::Star =>          TokenAttr("'*'",   "*"),
            Token::Slash =>         TokenAttr("'/'",   "/"),
            Token::Mod =>           TokenAttr("'%'",   "%"),

            Token::BitNot =>        TokenAttr("'~'",   "~"),
            Token::BitAnd =>        TokenAttr("'&'",   "&"),
            Token::BitOr =>         TokenAttr("'|'",   "|"),
            Token::BitXor =>        TokenAttr("'^'",   "^"),
            Token::LShift =>        TokenAttr("'<<'",  "<<"),
            Token::RShift =>        TokenAttr("'>>'",  ">>"),

            Token::Incr =>          TokenAttr("'++'",  "++"),
            Token::Decr =>          TokenAttr("'--'",  "--"),

            Token::BoolNot =>       TokenAttr("'!'",   "!"),
            Token::BoolAnd =>       TokenAttr("'&&'",  "&&"),
            Token::BoolOr =>        TokenAttr("'||'",  "||"),

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
            Token::Loop =>          TokenAttr("'loop'", "loop"),
            Token::Pub =>           TokenAttr("'pub'", "pub"),
            Token::Region =>        TokenAttr("'region'", "region"),
            Token::Repeat =>        TokenAttr("'repeat'", "repeat"),
            Token::Return =>        TokenAttr("'return'", "return"),
            Token::Ram =>           TokenAttr("'ram'", "ram"),
            Token::Rom =>           TokenAttr("'rom'", "rom"),
            Token::Sizeof =>        TokenAttr("'sizeof'", "sizeof"),
            Token::Struct =>        TokenAttr("'struct'", "struct"),
            Token::Unit =>          TokenAttr("'unit'", "unit"),
            Token::Var =>           TokenAttr("'var'", "var"),
            Token::While =>         TokenAttr("'while'", "while"),
        }
    }

    pub fn to_repr<'a>(&'a self) -> String {
        match *self {
            Token::Number(x) => x.to_string(),
            Token::Identifier(ref x) => x.clone(),
            Token::String(ref x) => format!("\"{}\"", escape_str(x.as_slice())),
            Token::Character(x) => format!("'{}'", escape_char(x)),
            _ => match self.attrs() { TokenAttr(_, x) => String::from_str(x) }
        }
    }
}

fn escape_char(c: char) -> String {
    let mut s = String::new();
    c.escape_default();
    return s
}

fn escape_str(s: &str) -> String {
    let mut t = String::new();
    for c in s.chars() { c.escape_default(); }
    return t;
}
