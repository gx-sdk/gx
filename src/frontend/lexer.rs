// gx language implementation
// Copyright (C) 2014-present Alex Iadicicco <http://ajitek.net>
//
// For licensing information, refer to the COPYING file
// in the project root

//! Lexer for the `gx` language. Very simple lexer.

use std::io;
use std::num::FromStrRadix;

use frontend::token::*;

fn is_identifier_char(c: char) -> bool { c.is_alphanumeric() || c == '_' }

fn nib_from_hex(nib: char) -> Result<u8, ()> {
    match nib {
        '0'...'9' => Ok(nib as u8 - '0' as u8),
        'a'...'f' => Ok(nib as u8 - 'a' as u8 + 10),
        'A'...'F' => Ok(nib as u8 - 'A' as u8 + 10),
        _ => Err(()),
    }
}

fn byte_from_hex_pair(hi: char, lo: char) -> Result<u8, ()> {
    let top = try!(nib_from_hex(hi));
    let bot = try!(nib_from_hex(lo));
    Ok((top << 4) | bot)
}

/// The Lexer struct is the gx-lang lexer implementation. It implements
/// the Iterator trait, which yields a stream of tokens lexed out of
/// the input character iterator.
///
/// Internally, Lexer is also capable of un-reading a character from
/// the input stream, using an 'ungot' stack, which is an essential
/// lexer implementation feature.
///
/// ```plain
///                 input pointer
///                          |
///                          v
/// +---+---+---+---+---+---+---+---+- - -
/// | a | = | b | * | c | + | d | . | rest of input
/// +---+---+---+---+---+---+---+---+- - -
///           ungot growth  :
///                  <------: ungot bottom
/// ```
///
/// when ungot is used as a stack in this manner, characters will be
/// read back in the opposite order they are ungot.

pub struct Lexer<It> {
    input: It,
    ungot: Vec<char>,
    line_number: usize,
    sent_eof: bool,
}

enum LexerStep {
    Step(Token),
    Again,
    EndOfInput,
}

impl<It: Iterator<Item = Result<char, io::CharsError>>> Lexer<It> {
    pub fn new(input: It) -> Lexer<It> {
        Lexer {
            input: input,
            ungot: Vec::with_capacity(5),
            line_number: 1,
            sent_eof: false,
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

    fn getc_while<T: Fn(char) -> bool>(&mut self, f: T) -> String {
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

    fn read_int(&mut self) -> LexerStep {
        let s = self.getc_while(|c| {
                c.is_digit(16) || c == 'x' || c == 'X'
            });

        let (base, num) =
            if s.starts_with("0x") || s.starts_with("0X") {
                (16u32, &s[2..])
            } else if s.starts_with("0") {
                (8u32,  &s[..])
            } else {
                (10u32, &s[..])
            };

        match FromStrRadix::from_str_radix(num, base) {
            Ok(x) => LexerStep::Step(Token::Number(x)),
            Err(e) => self.die(
                &format!("invalid base {} constant {}: {}", base, s, e)
                )
        }
    }

    fn read_identifier(&mut self) -> LexerStep {
        let s = self.getc_while(|c| { is_identifier_char(c) });

        match find_keyword(&s) {
            Some(tok) => LexerStep::Step(tok),
            None      => LexerStep::Step(Token::Identifier(s)),
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
                    let res = byte_from_hex_pair(u, v);
                    match res {
                        Ok(x) => x as char,
                        Err(_) => self.die("invalid \\xNN escape sequence")
                    }
                },
                c => c,
            },
            c => c,
        }
    }

    fn read_string(&mut self) -> LexerStep {
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

        LexerStep::Step(Token::String(s))
    }

    fn read_char(&mut self) -> LexerStep {
        let c = self.read_char_expr();

        match self.getc() {
            Some('\'') => LexerStep::Step(Token::Character(c)),
            _ => self.die("invalid character constant")
        }
    }

    fn try_token(&mut self) -> LexerStep {
        use self::LexerStep::*;

        let c = match self.getc() {
            Some(c) => c,
            None => return EndOfInput
        };

        return if c.is_whitespace() {
            Again

        } else if c.is_digit(10) {
            self.ungetc(c);
            self.read_int()

        } else if is_identifier_char(c) {
            self.ungetc(c);
            self.read_identifier()

        } else { match c {
            '-' => match self.getc_or_zero() {
                '>' => Step(Token::RArr),
                '=' => Step(Token::MinusEq),
                '-' => Step(Token::Decr),
                d   => { self.ungetc(d); Step(Token::Minus) }
            },

            '+' => match self.getc_or_zero() {
                '+' => Step(Token::Incr),
                '=' => Step(Token::PlusEq),
                d   => { self.ungetc(d); Step(Token::Plus) }
            },

            '(' => Step(Token::LParen),
            ')' => Step(Token::RParen),
            '[' => Step(Token::LBrack),
            ']' => Step(Token::RBrack),
            '{' => Step(Token::LBrace),
            '}' => Step(Token::RBrace),

            '.' => Step(Token::Dot),
            ',' => Step(Token::Comma),
            ';' => Step(Token::Semicolon),
            '?' => Step(Token::Question),

            ':' => match self.getc_or_zero() {
                ':' => Step(Token::DblColon),
                d   => { self.ungetc(d); Step(Token::Colon) }
            },

            '=' => match self.getc_or_zero() {
                '=' => Step(Token::Eq),
                '>' => Step(Token::RDblArr),
                d   => { self.ungetc(d); Step(Token::Assign) }
            },

            '*' => match self.getc_or_zero() {
                '=' => Step(Token::StarEq),
                d   => { self.ungetc(d); Step(Token::Star) }
            },
            '/' => match self.getc_or_zero() {
                '/' => {
                    self.getc_while(|c| { c != '\n' });
                    Again
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
                    Again
                },
                '=' => Step(Token::SlashEq),
                d   => { self.ungetc(d); Step(Token::Slash) }
            },
            '%' => match self.getc_or_zero() {
                '=' => Step(Token::ModEq),
                d   => { self.ungetc(d); Step(Token::Mod) }
            },

            '~' => match self.getc_or_zero() {
                '=' => Step(Token::BitNotEq),
                d   => { self.ungetc(d); Step(Token::Tilde) }
            },
            '&' => match self.getc_or_zero() {
                '&' => Step(Token::DblAmp),
                '=' => Step(Token::BitAndEq),
                d   => { self.ungetc(d); Step(Token::Amp) }
            },
            '|' => match self.getc_or_zero() {
                '|' => Step(Token::DblPipe),
                '=' => Step(Token::BitOrEq),
                d   => { self.ungetc(d); Step(Token::Pipe) }
            },
            '^' => match self.getc_or_zero() {
                '=' => Step(Token::BitXorEq),
                d   => { self.ungetc(d); Step(Token::Caret) }
            },

            '<' => match self.getc_or_zero() {
                '-' => Step(Token::LArr),
                '=' => Step(Token::LessEq),
                '<' => match self.getc_or_zero() {
                    '=' => Step(Token::LShiftEq),
                    e   => { self.ungetc(e); Step(Token::LShift) },
                },
                d   => { self.ungetc(d); Step(Token::Less) }
            },

            '>' => match self.getc_or_zero() {
                '=' => Step(Token::GreaterEq),
                '>' => match self.getc_or_zero() {
                    '=' => Step(Token::RShiftEq),
                    e   => { self.ungetc(e); Step(Token::RShift) },
                },
                d   => { self.ungetc(d); Step(Token::Greater) }
            },

            '!' => match self.getc_or_zero() {
                '=' => Step(Token::NotEq),
                d   => { self.ungetc(d); Step(Token::Excl) }
            },

            '#' => {
                self.getc_while(|c| { c != '\n' });
                Again
            },

            '"' => self.read_string(),
            '\'' => self.read_char(),

            _ => self.die(&format!("lexer error: unexpected char {}", c))
        } }
    }
}

impl<It: Iterator<Item = Result<char, io::CharsError>>> Iterator for Lexer<It> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        loop {
            match self.try_token() {
                LexerStep::Step(x) => return Some(x),
                LexerStep::Again => { },
                LexerStep::EndOfInput => {
                    if self.sent_eof {
                        return None
                    } else {
                        self.sent_eof = true;
                        return Some(Token::EOF)
                    }
                },
            }
        }
    }
}
