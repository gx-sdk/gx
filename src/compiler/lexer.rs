use std::io::IoResult;
use std::char::is_whitespace;
use std::char::is_digit;
use std::char::is_digit_radix;
use std::char::is_alphanumeric;
use std::num::FromStrRadix;

use compiler::token::*;

fn is_identifier_char(c: char) -> bool { is_alphanumeric(c) || c == '_' }

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

        match FromStrRadix::from_str_radix(num, base) {
            Some(x) => Some(Token::Number(x)),
            None => self.die(
                format!("invalid base {} constant {}", base, s).as_slice()
                )
        }
    }

    fn read_identifier(&mut self) -> Option<Token> {
        let s = self.getc_while(|c| { is_identifier_char(c) });

        match find_keyword(s.as_slice()) {
            Some(tok) => Some(tok),
            None      => Some(Token::Identifier(s)),
        }
    }

    fn read_char_expr(&mut self) -> char {
        match self.getc_or_die() {
            '\\' => match self.getc_or_die() {
                'n' => '\n',
                'r' => '\r',
                't' => '\t',
                'x' => {
                    let u = self.getc_or_die();
                    let v = self.getc_or_die();
                    let string = String::from_chars(&[u, v]);
                    let res: Option<u8> = FromStrRadix::from_str_radix(&*string, 16);
                    match res {
                        Some(x) => x as char,
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

        Some(Token::String(s))
    }

    fn read_char(&mut self) -> Option<Token> {
        let c = self.read_char_expr();

        match self.getc() {
            Some('\'') => Some(Token::Character(c)),
            _ => self.die("invalid character constant")
        }
    }

    fn try_token(&mut self) -> Option<Token> {
        let c = match self.getc() {
            Some(c) => c,
            None => return None
        };

        return if is_whitespace(c) {
            Some(Token::Ignore)

        } else if is_digit(c) {
            self.ungetc(c);
            self.read_int()

        } else if is_identifier_char(c) {
            self.ungetc(c);
            self.read_identifier()

        } else { match c {
            '-' => match self.getc_or_zero() {
                '>' => Some(Token::RArr),
                '=' => Some(Token::MinusEq),
                '-' => Some(Token::Decr),
                d   => { self.ungetc(d); Some(Token::Minus) }
            },

            '+' => match self.getc_or_zero() {
                '+' => Some(Token::Incr),
                '=' => Some(Token::PlusEq),
                d   => { self.ungetc(d); Some(Token::Plus) }
            },

            '(' => Some(Token::LParen),
            ')' => Some(Token::RParen),
            '[' => Some(Token::LBrack),
            ']' => Some(Token::RBrack),
            '{' => Some(Token::LBrace),
            '}' => Some(Token::RBrace),

            '.' => Some(Token::Dot),
            ',' => Some(Token::Comma),
            ':' => Some(Token::Colon),
            ';' => Some(Token::Semicolon),
            '?' => Some(Token::Question),

            '=' => match self.getc_or_zero() {
                '=' => Some(Token::Eq),
                '>' => Some(Token::RDblArr),
                d   => { self.ungetc(d); Some(Token::Assign) }
            },

            '*' => match self.getc_or_zero() {
                '=' => Some(Token::StarEq),
                d   => { self.ungetc(d); Some(Token::Star) }
            },
            '/' => match self.getc_or_zero() {
                '/' => {
                    self.getc_while(|c| { c != '\n' });
                    Some(Token::Ignore)
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
                    Some(Token::Ignore)
                },
                '=' => Some(Token::SlashEq),
                d   => { self.ungetc(d); Some(Token::Slash) }
            },
            '%' => match self.getc_or_zero() {
                '=' => Some(Token::ModEq),
                d   => { self.ungetc(d); Some(Token::Mod) }
            },

            '~' => match self.getc_or_zero() {
                '=' => Some(Token::BitNotEq),
                d   => { self.ungetc(d); Some(Token::BitNot) }
            },
            '&' => match self.getc_or_zero() {
                '&' => Some(Token::BoolAnd),
                '=' => Some(Token::BitAndEq),
                d   => { self.ungetc(d); Some(Token::BitAnd) }
            },
            '|' => match self.getc_or_zero() {
                '|' => Some(Token::BoolOr),
                '=' => Some(Token::BitOrEq),
                d   => { self.ungetc(d); Some(Token::BitOr) }
            },
            '^' => match self.getc_or_zero() {
                '=' => Some(Token::BitXorEq),
                d   => { self.ungetc(d); Some(Token::BitXor) }
            },

            '<' => match self.getc_or_zero() {
                '-' => Some(Token::LArr),
                '=' => Some(Token::LessEq),
                '<' => match self.getc_or_zero() {
                    '=' => Some(Token::LShiftEq),
                    e   => { self.ungetc(e); Some(Token::LShift) },
                },
                d   => { self.ungetc(d); Some(Token::Less) }
            },

            '>' => match self.getc_or_zero() {
                '=' => Some(Token::GreaterEq),
                '>' => match self.getc_or_zero() {
                    '=' => Some(Token::RShiftEq),
                    e   => { self.ungetc(e); Some(Token::RShift) },
                },
                d   => { self.ungetc(d); Some(Token::Greater) }
            },

            '!' => match self.getc_or_zero() {
                '=' => Some(Token::NotEq),
                d   => { self.ungetc(d); Some(Token::BoolNot) }
            },

            '#' => { /* to-eol comment, rofl */
                self.getc_while(|c| { c != '\n' });
                Some(Token::Ignore)
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
                Some(Token::Ignore) => {}
                Some(x) => return Some(x),
                None => return None
            }
        }
    }
}
