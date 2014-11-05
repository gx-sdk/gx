use std::io::IoResult;
use std::char::is_whitespace;
use std::char::is_digit;
use std::char::is_digit_radix;
use std::char::is_alphanumeric;
use std::uint;

fn is_identifier_char(c: char) -> bool { is_alphanumeric(c) || c == '_' }

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

/* The Lexer struct is the gx-lang lexer implementation. It implements
   the Iterator trait, which yields a stream of tokens lexed out of
   the input character iterator. */

/* Internally, Lexer is also capable of un-reading a character from
   the input stream, using an 'ungot' stack, which is an essential
   lexer implementation feature.
                   input pointer
                            |
                            v
   +---+---+---+---+---+---+---+---+- - -
   | a | = | b | * | c | ; |\n |\t | rest of input
   +---+---+---+---+---+---+---+---+- - -
             ungot growth  :
                    <------: ungot bottom

   when ungot is used as a stack in this manner, characters will be
   read back in the opposite order they are ungot. */

pub struct Lexer<It> {
    input: It,
    ungot: Vec<char>,
    line_number: uint,
}

impl <It: Iterator<IoResult<char>>> Lexer<It> {
    pub fn new(input: It) -> Lexer<It> {
        Lexer {
            input: input,
            ungot: Vec::with_capacity(5),
            line_number: 1,
        }
    }

    fn die(&mut self, msg: &str) -> ! {
        panic!("lexer error: {}: {}", self.line_number, msg)
    }

    fn ungetc(&mut self, c: char) {
        if c != '\0' {
            if c == '\n' {
                self.line_number -= 1;
            }
            self.ungot.push(c);
        }
    }

    fn real_getc(&mut self) -> Option<char> {
        match self.ungot.pop() {
            Some(c) => Some(c),
            None => match self.input.next() {
                Some(Ok(c)) => Some(c),
                Some(Err(_)) => None,
                None => None,
            }
        }
    }

    fn getc(&mut self) -> Option<char> {
        let c = self.real_getc();
        match c {
            Some('\n') => self.line_number += 1,
            _ => { }
        }
        c
    }

    fn getc_or_die(&mut self) -> char {
        match self.getc() {
            Some(c) => c,
            None => self.die("lexer error: unexpected end of file"),
        }
    }

    fn getc_or_zero(&mut self) -> char {
        match self.getc() {
            Some(c) => c,
            None => '\0',
        }
    }

    fn getc_while(&mut self, f: |char| -> bool) -> String {
        let mut s = String::with_capacity(5);

        loop {
            let c = match self.getc() {
                Some(c) => c,
                None => break
            };

            if f(c) {
                s.push(c);
            } else {
                self.ungetc(c);
                break;
            }
        }

        return s;
    }

    fn read_int(&mut self) -> Option<Token> {
        let s = self.getc_while(|c| {
                is_digit_radix(c, 16) || c == 'x' || c == 'X'
            });

        let (base, num) =
            if s.starts_with("0x") || s.starts_with("0X") {
                (16u, s.slice_from(2))
            } else if s.starts_with("0") {
                (8u,  s.as_slice())
            } else {
                (10u, s.as_slice())
            };

        match uint::parse_bytes(num.as_bytes(), base) {
            Some(x) => Some(TokNumber(x)),
            None => self.die(
                format!("invalid base {} constant {}", base, s).as_slice()
                )
        }
    }

    fn read_identifier(&mut self) -> Option<Token> {
        let s = self.getc_while(|c| { is_identifier_char(c) });

        match find_keyword(s.as_slice()) {
            Some(tok) => Some(tok),
            None      => Some(TokIdentifier(s)),
        }
    }

    fn read_char_expr(&mut self) -> char {
        match self.getc_or_die() {
            '\\' => match self.getc_or_die() {
                'n' => '\n',
                'r' => '\r',
                't' => '\t',
                'x' => {
                    let u = self.getc_or_die() as u8;
                    let v = self.getc_or_die() as u8;

                    match uint::parse_bytes(&[u, v], 16) {
                        Some(x) => (x as u8) as char,
                        None => self.die("invalid \\xNN escape sequence")
                    }
                },
                c => c,
            },
            c => c,
        }
    }

    fn read_string(&mut self) -> Option<Token> {
        let mut s = String::with_capacity(10);

        loop {
            let c = match self.getc() {
                Some(c) => c,
                None => self.die("unexpected end of file"),
            };

            match c {
                '"' => break,
                _ => {
                    self.ungetc(c);
                    s.push(self.read_char_expr())
                }
            }
        }

        Some(TokString(s))
    }

    fn read_char(&mut self) -> Option<Token> {
        let c = self.read_char_expr();

        match self.getc() {
            Some('\'') => Some(TokCharacter(c)),
            _ => self.die("invalid character constant")
        }
    }

    fn try_token(&mut self) -> Option<Token> {
        let c = match self.getc() {
            Some(c) => c,
            None => return None
        };

        return if is_whitespace(c) {
            Some(TokIgnore)

        } else if is_digit(c) {
            self.ungetc(c);
            self.read_int()

        } else if is_identifier_char(c) {
            self.ungetc(c);
            self.read_identifier()

        } else { match c {
            '-' => match self.getc_or_zero() {
                '>' => Some(TokRArr),
                '=' => Some(TokMinusEq),
                '-' => Some(TokDecr),
                d   => { self.ungetc(d); Some(TokMinus) }
            },

            '+' => match self.getc_or_zero() {
                '+' => Some(TokIncr),
                '=' => Some(TokPlusEq),
                d   => { self.ungetc(d); Some(TokPlus) }
            },

            '(' => Some(TokLParen),
            ')' => Some(TokRParen),
            '[' => Some(TokLBrack),
            ']' => Some(TokRBrack),
            '{' => Some(TokLBrace),
            '}' => Some(TokRBrace),

            '.' => Some(TokDot),
            ',' => Some(TokComma),
            ':' => Some(TokColon),
            ';' => Some(TokSemicolon),
            '?' => Some(TokQuestion),

            '=' => match self.getc_or_zero() {
                '=' => Some(TokEq),
                '>' => Some(TokRDblArr),
                d   => { self.ungetc(d); Some(TokAssign) }
            },

            '*' => match self.getc_or_zero() {
                '=' => Some(TokStarEq),
                d   => { self.ungetc(d); Some(TokStar) }
            },
            '/' => match self.getc_or_zero() {
                '/' => {
                    self.getc_while(|c| { c != '\n' });
                    Some(TokIgnore)
                },
                '*' => {
                    loop {
                        self.getc_while(|c| { c != '*' });
                        self.getc();
                        match self.getc() {
                            Some('/') => break,
                            Some(_) => { },
                            None => self.die("unexpected end of file"),
                        }
                    }
                    Some(TokIgnore)
                },
                '=' => Some(TokSlashEq),
                d   => { self.ungetc(d); Some(TokSlash) }
            },
            '%' => match self.getc_or_zero() {
                '=' => Some(TokModEq),
                d   => { self.ungetc(d); Some(TokMod) }
            },

            '~' => match self.getc_or_zero() {
                '=' => Some(TokBitNotEq),
                d   => { self.ungetc(d); Some(TokBitNot) }
            },
            '&' => match self.getc_or_zero() {
                '&' => Some(TokBoolAnd),
                '=' => Some(TokBitAndEq),
                d   => { self.ungetc(d); Some(TokBitAnd) }
            },
            '|' => match self.getc_or_zero() {
                '|' => Some(TokBoolOr),
                '=' => Some(TokBitOrEq),
                d   => { self.ungetc(d); Some(TokBitOr) }
            },
            '^' => match self.getc_or_zero() {
                '=' => Some(TokBitXorEq),
                d   => { self.ungetc(d); Some(TokBitXor) }
            },

            '<' => match self.getc_or_zero() {
                '-' => Some(TokLArr),
                '=' => Some(TokLessEq),
                '<' => match self.getc_or_zero() {
                    '=' => Some(TokLShiftEq),
                    e   => { self.ungetc(e); Some(TokLShift) },
                },
                d   => { self.ungetc(d); Some(TokLess) }
            },

            '>' => match self.getc_or_zero() {
                '=' => Some(TokGreaterEq),
                '>' => match self.getc_or_zero() {
                    '=' => Some(TokRShiftEq),
                    e   => { self.ungetc(e); Some(TokRShift) },
                },
                d   => { self.ungetc(d); Some(TokGreater) }
            },

            '!' => match self.getc_or_zero() {
                '=' => Some(TokNotEq),
                d   => { self.ungetc(d); Some(TokBoolNot) }
            },

            '#' => { /* to-eol comment, rofl */
                self.getc_while(|c| { c != '\n' });
                Some(TokIgnore)
            },

            '"' => self.read_string(),
            '\'' => self.read_char(),

            _ => self.die(format!("lexer error: unexpected char {}", c).as_slice())
        } }
    }
}

impl <It: Iterator<IoResult<char>>> Iterator<Token> for Lexer<It> {
    fn next(&mut self) -> Option<Token> {
        loop {
            match self.try_token() {
                Some(TokIgnore) => {}
                Some(x) => return Some(x),
                None => return None
            }
        }
    }
}
