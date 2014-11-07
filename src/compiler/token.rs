#[deriving(Show)]
pub enum Token {
    /* this will never be yielded from the lexer; it is used internally to
       indicate that another iteration of the input loop is needed */
    TokIgnore,

    /* parameterized token types */

    TokNumber(uint),
    TokIdentifier(String),
    TokString(String),
    TokCharacter(char),

    /* glyphs */

    TokLParen,      /*  (  */
    TokRParen,      /*  )  */
    TokLBrack,      /*  [  */
    TokRBrack,      /*  ]  */
    TokLBrace,      /*  {  */
    TokRBrace,      /*  }  */

    TokDot,         /*  .  */
    TokComma,       /*  ,  */
    TokColon,       /*  :  */
    TokSemicolon,   /*  ;  */
    TokQuestion,    /*  ?  */

    TokLArr,        /*  <- */
    TokRArr,        /*  -> */
    TokRDblArr,     /*  => */

    TokPlus,        /*  +  */
    TokMinus,       /*  -  */
    TokStar,        /*  *  */
    TokSlash,       /*  /  */
    TokMod,         /*  %  */

    TokBitNot,      /*  ~  */
    TokBitAnd,      /*  &  */
    TokBitOr,       /*  |  */
    TokBitXor,      /*  ^  */
    TokLShift,      /*  << */
    TokRShift,      /*  >> */

    TokIncr,        /*  ++ */
    TokDecr,        /*  -- */

    TokBoolNot,     /*  !  */
    TokBoolAnd,     /*  && */
    TokBoolOr,      /*  || */

    TokEq,          /*  == */
    TokNotEq,       /*  != */
    TokLess,        /*  <  */
    TokGreater,     /*  >  */
    TokLessEq,      /*  <= */
    TokGreaterEq,   /*  >= */

    TokAssign,      /*  =  */
    TokPlusEq,      /*  += */
    TokMinusEq,     /*  -= */
    TokStarEq,      /*  *= */
    TokSlashEq,     /*  /= */
    TokModEq,       /*  %= */
    TokBitNotEq,    /*  ~= */
    TokBitAndEq,    /*  &= */
    TokBitOrEq,     /*  |= */
    TokBitXorEq,    /*  ^= */
    TokLShiftEq,    /* <<= */
    TokRShiftEq,    /* >>= */

    /* keywords */

    TokIf,
    TokWhile,
    TokFor,
    TokStruct,
    TokUnion,
    TokEnum,
}

pub fn find_keyword(id: &str) -> Option<Token> {
    match id {
        "if"      => Some(TokIf),
        "while"   => Some(TokWhile),
        "for"     => Some(TokFor),
        "struct"  => Some(TokStruct),
        "union"   => Some(TokUnion),
        "enum"    => Some(TokEnum),
        _         => None,
    }
}

pub struct TokenAttr(&'static str, &'static str);

impl Token {
    pub fn attrs(&self) -> TokenAttr {
        match *self {
            TokIgnore =>        TokenAttr("TokIgnore", "TokIgnore"),

            TokNumber(_) =>     TokenAttr("number literal", "(num)"),
            TokIdentifier(_) => TokenAttr("identifier", "(id)"),
            TokString(_) =>     TokenAttr("string literal", "(str)"),
            TokCharacter(_) =>  TokenAttr("character literal", "(char)"),

            TokLParen =>        TokenAttr("'('",   "("),
            TokRParen =>        TokenAttr("')'",   ")"),
            TokLBrack =>        TokenAttr("'['",   "["),
            TokRBrack =>        TokenAttr("']'",   "]"),
            TokLBrace =>        TokenAttr("'{'",   "{"),
            TokRBrace =>        TokenAttr("'}'",   "}"),

            TokDot =>           TokenAttr("'.'",   "."),
            TokComma =>         TokenAttr("','",   ","),
            TokColon =>         TokenAttr("':'",   ":"),
            TokSemicolon =>     TokenAttr("';'",   ";"),
            TokQuestion =>      TokenAttr("'?'",   "?"),

            TokLArr =>          TokenAttr("'<-'",  "<-"),
            TokRArr =>          TokenAttr("'->'",  "->"),
            TokRDblArr =>       TokenAttr("'=>'",  "=>"),

            TokPlus =>          TokenAttr("'+'",   "+"),
            TokMinus =>         TokenAttr("'-'",   "-"),
            TokStar =>          TokenAttr("'*'",   "*"),
            TokSlash =>         TokenAttr("'/'",   "/"),
            TokMod =>           TokenAttr("'%'",   "%"),

            TokBitNot =>        TokenAttr("'~'",   "~"),
            TokBitAnd =>        TokenAttr("'&'",   "&"),
            TokBitOr =>         TokenAttr("'|'",   "|"),
            TokBitXor =>        TokenAttr("'^'",   "^"),
            TokLShift =>        TokenAttr("'<<'",  "<<"),
            TokRShift =>        TokenAttr("'>>'",  ">>"),

            TokIncr =>          TokenAttr("'++'",  "++"),
            TokDecr =>          TokenAttr("'--'",  "--"),

            TokBoolNot =>       TokenAttr("'!'",   "!"),
            TokBoolAnd =>       TokenAttr("'&&'",  "&&"),
            TokBoolOr =>        TokenAttr("'||'",  "||"),

            TokEq =>            TokenAttr("'=='",  "=="),
            TokNotEq =>         TokenAttr("'!='",  "!="),
            TokLess =>          TokenAttr("'<'",   "<"),
            TokGreater =>       TokenAttr("'>'",   ">"),
            TokLessEq =>        TokenAttr("'<='",  "<="),
            TokGreaterEq =>     TokenAttr("'>='",  ">="),

            TokAssign =>        TokenAttr("'='",   "="),
            TokPlusEq =>        TokenAttr("'+='",  "+="),
            TokMinusEq =>       TokenAttr("'-='",  "-="),
            TokStarEq =>        TokenAttr("'*='",  "*="),
            TokSlashEq =>       TokenAttr("'/='",  "/="),
            TokModEq =>         TokenAttr("'%='",  "%="),
            TokBitNotEq =>      TokenAttr("'~='",  "~="),
            TokBitAndEq =>      TokenAttr("'&='",  "&="),
            TokBitOrEq =>       TokenAttr("'|='",  "|="),
            TokBitXorEq =>      TokenAttr("'^='",  "^="),
            TokLShiftEq =>      TokenAttr("'<<='", "<<="),
            TokRShiftEq =>      TokenAttr("'>>='", ">>="),

            TokIf =>            TokenAttr("'if'", "if"),
            TokWhile =>         TokenAttr("'while'", "while"),
            TokFor =>           TokenAttr("'for'", "for"),
            TokStruct =>        TokenAttr("'struct'", "struct"),
            TokUnion =>         TokenAttr("'union'", "union"),
            TokEnum =>          TokenAttr("'enum'", "enum"),
        }
    }

    pub fn to_repr<'a>(&'a self) -> String {
        match *self {
            TokNumber(x) => x.to_string(),
            TokIdentifier(ref x) => x.clone(),
            TokString(ref x) => format!("\"{}\"", escape_str(x.as_slice())),
            TokCharacter(x) => format!("'{}'", escape_char(x)),
            _ => match self.attrs() { TokenAttr(_, x) => String::from_str(x) }
        }
    }
}

fn escape_char(c: char) -> String {
    let mut s = String::new();
    c.escape_default(|x| { s.push(x) });
    return s
}

fn escape_str(s: &str) -> String {
    let mut t = String::new();
    for c in s.chars() { c.escape_default(|x| { t.push(x) }); }
    return t;
}
