// Copyright 2020(c) Wael El Oraiby
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.
#![allow(non_snake_case, non_camel_case_types)]

use alt_std::*;
use alt_std::{format};

pub struct ParseError {
    message : String,
    offset  : usize
}

pub enum ParseResult<T> {
    PROk(T),
    PRErr(ParseError)
}

use ParseResult::*;

impl<T : core::cmp::PartialEq> PartialEq for ParseResult<T> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (PROk(s), PROk(o)) => *s == *o,
            (PRErr (ParseError{ message: msg1, offset: offset1 }), PRErr (ParseError{ message: msg2, offset: offset2 })) => *msg1 == *msg2 && *offset1 == *offset2,
            _ => false
        }
    }
}

#[derive(Clone)]
pub enum Exp {
    Bool(bool),
    Char(char),
    Int(i64),
    Float(f64),
    String(String),
    Symbol(String),
    List(Vec<Exp>),
}

impl PartialEq<Exp> for Exp {
    fn eq(&self, other: &Exp) -> bool {
        match (self, other) {
            (Self::Bool(b0),            Self::Bool(bo))     => b0 == bo,
            (Self::Char(c0),            Self::Char(c1))     => c0 == c1,
            (Self::Int(i0),             Self::Int(i1))      => i0 == i1,
            (Self::Float(f0),           Self::Float(f1))    => f0 == f1,
            (Self::String(s0),          Self::String(s1))   => s0 == s1,
            (Self::Symbol(s0),          Self::Symbol(s1))   => s0 == s1,
            (Self::List(s), Self::List(o)) => {
                if s.len() != o.len() { return false }
                for i in 0..s.len() {
                    if !Self::eq(&s[i], &o[i]) { return false }
                }
                true
            },
            _ => false
        }
    }
}
impl Exp {
    fn peek(src: &[u8], offset: usize) -> Option<u8> {
        if src.len() <= offset {
            None
        } else {
            Some(src[offset])
        }
    }

    fn getchar(src: &[u8], offset: &mut usize) -> Option<u8> {
        match Self::peek(src, *offset) {
            None => None,
            Some(c) => { *offset += 1; Some(c) },
        }
    }

    fn isDigit(c: u8) -> bool {
        match c as char {
            c if c >= '0' && c <= '9' => true,
            _ => false
        }
    }

    fn isAlpha(c: u8) -> bool {
        match c as char {
            c if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') => true,
            _ => false
        }
    }


    fn isOp(c: u8) -> bool {
        match c as char {
            '+' | '-' | '*' | '/' | '%' | '~' | '!' | '@' | '#' | '$' | '^' | '&' | '|' | '_' | '=' | '<' | '>' | '?' | '.' | ':' | '\\' | '\'' => true,
            _ => false
        }
    }

    fn isWS(c: u8) -> bool {
        match c as char {
            ' ' | '\n' | '\t' => true,
            _ => false
        }
    }

    fn isSeparator(c: u8) -> bool {
        match c as char {
            '(' | ')' | '{' | '}' | ',' | '\'' | '"' => true,
            x if Self::isWS(x as u8) => true,
            _ => false
        }
    }

    pub fn parseNumber(src: &[u8], offset: &mut usize) -> ParseResult<Exp> {
        let mut s = String::new();
        loop {
            match Self::peek(src, *offset) {
                Some(c) if c == b'+' || c == b'-' || c == b'.' || c == b'e' || c == b'E' || Self::isDigit(c) => {
                    s.add(c);
                    Self::getchar(src, offset);
                },
                Some(c) if Self::isSeparator(c) => break,
                None => break,
                _ => return PRErr (ParseError { message: String::from("Unexpected end of stream (sign)"), offset: *offset })
            }
        }

        match str::parse::<i64>(s.toStr()) {
            Ok(i) => return ParseResult::PROk(Exp::Int(i)),
            _ => ()
        }

        match str::parse::<f64>(s.toStr()) {
            Ok(f) => return ParseResult::PROk(Exp::Float(f)),
            _ => ()
        }

        PRErr (ParseError { message: String::from("invalid number format"), offset: *offset })
    }

    fn parseString(src: &[u8], offset: &mut usize) -> ParseResult<String> {
        let mut s = String::new();
        match Self::peek(src, *offset) {
            Some(c) if c as char == '"' => (),
            _ => return PRErr (ParseError{ message: String::from("Expected \""), offset: *offset })
        }

        Self::getchar(src, offset);
        // TODO: handle '\' case
        loop {
            match Self::getchar(src, offset) {
                None => return PRErr (ParseError{ message: String::from("Unexpected end of stream (string)"), offset: *offset }),
                Some(c) if c as char == '"' => break,
                Some(c) => s.add(c),
            }
        }

        return PROk(s)
    }

    fn parseSymbol(src: &[u8], offset: &mut usize) -> ParseResult<String> {
        let mut s = String::new();
        match Self::peek(src, *offset) {
            Some(c) if Self::isAlpha(c) || Self::isOp(c) => (),
            _ => return PRErr (ParseError{ message: String::from("Expected alpha/operator"), offset: *offset })
        }

        loop {
            match Self::peek(src, *offset) {
                Some(c) if Self::isAlpha(c) || Self::isOp(c) || Self::isDigit(c) => s.add(c),
                _ => break,
            }
            Self::getchar(src, offset);
        }

        return PROk(s)
    }

    fn skipWS(src: &[u8], offset: &mut usize) {
        loop {
            match Self::peek(src, *offset) {
                Some(c) if Self::isWS(c) => { Self::getchar(src, offset); },
                _ => break
            }
        }
    }

    fn parseToken(src: &[u8], offset: &mut usize) -> ParseResult<Exp> {
        match Self::peek(src, *offset) {
            Some(c) if c as char == '"' => {
                let stringRes = Self::parseString(src, offset);
                match stringRes {
                    PROk(r) => PROk(Exp::String(r)),
                    PRErr(err) => PRErr(err)
                }
            },
            Some(c) if Self::isDigit(c) || ((c as char == '+' || c as char == '-') && match Self::peek(src, *offset + 1) { Some(c) if Self::isDigit(c) => true, _ => false })  => {
                let numRes = Self::parseNumber(src, offset);
                match numRes {
                    PROk(r) => PROk(r),
                    PRErr(err) => PRErr(err)
                }
            },
            Some(c) if Self::isAlpha(c) || Self::isOp(c) => {
                let symbolRes = Self::parseSymbol(src, offset);
                match symbolRes {
                    PROk(r) => PROk(Exp::Symbol(r)),
                    PRErr(err) => PRErr(err)
                }
            },
            Some(c) if c as char == '(' => Self::parseList(src, offset),
            Some(_) => PRErr(ParseError { message: String::from("unexpected char (token)"), offset: *offset}),
            None => PRErr(ParseError { message: String::from("unexpected end of stream (token)"), offset: *offset}),
        }
    }

    fn parseList(src: &[u8], offset: &mut usize) -> ParseResult<Exp> {
        match Self::getchar(src, offset) {
            Some(c) if c as char == '(' => (),
            Some(_) => return PRErr(ParseError { message: String::from("unexpected character (list)"), offset: *offset}),
            None => return PRErr(ParseError { message: String::from("unexpected end of stream (list)"), offset: *offset}),
        }

        let mut cells = Vec::new();
        loop {
            Self::skipWS(src, offset);
            match Self::peek(src, *offset) {
                Some(c) if c as char == ')' => {
                    Self::getchar(src, offset);
                    return PROk(Exp::List(cells))
                },
                Some(_) => {
                    match Self::parseToken(src, offset) {
                        PROk(c) => cells.pushBack(c),
                        PRErr(err) => return PRErr(err),
                    }
                },
                None => return PRErr(ParseError { message: String::from("unexpected end of stream (list)"), offset: *offset})
            }
        }
    }

    pub fn fromSExp(src: &[u8]) -> ParseResult<Exp> {
        let mut offset : usize = 0;
        Self::skipWS(src, &mut offset);
        Self::parseToken(src, &mut offset)
    }

    pub fn toString(&self) -> String {
        match self {
            Self::Bool(b) => format!("{}", b),
            Self::Char(c) => format!("{}", c),
            Self::Int(i) => format!("{}", i),
            Self::Float(f) => format!("{}", f),
            Self::String(s) => {
                let mut sr = String::new();
                sr.add('"' as u8);
                let a = s.asArray();
                for i in a.iter() {
                    sr.add(*i);
                }
                sr.add('"' as u8);
                sr
            },
            Self::Symbol(s) => s.clone(),
            Self::List(l) => {
                let mut s = String::new();
                s.add('(' as u8);
                for i in 0..l.len() {
                    s.append(&(l[i].toString()));
                    if i != l.len() - 1 {
                        s.add(' ' as u8);
                    }
                }
                s.add(')' as u8);
                s
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn testParseInt() {
        let s = String::from("1234");
        let mut offset = 0;
        let res = Exp::parseNumber(s.asArray(), &mut offset);
        assert!(res == PROk(Exp::Int(1234)));

        let s = String::from("-001234");
        let mut offset = 0;
        let res = Exp::parseNumber(s.asArray(), &mut offset);
        assert!(res == PROk(Exp::Int(-1234)));

        let s = String::from("-1234");
        let mut offset = 0;
        let res = Exp::parseNumber(s.asArray(), &mut offset);
        assert!(res == PROk(Exp::Int(-1234)));

        let s = String::from("-1234 ");
        let mut offset = 0;
        let res = Exp::parseNumber(s.asArray(), &mut offset);
        assert!(res == PROk(Exp::Int(-1234)));

        let s = String::from("-1234+");
        let mut offset = 0;
        let res = Exp::parseNumber(s.asArray(), &mut offset);
        assert!(res != PROk(Exp::Int(-1234)));

        let s = String::from("-1234a");
        let mut offset = 0;
        let res = Exp::parseNumber(s.asArray(), &mut offset);
        assert!(res != PROk(Exp::Int(-1234)));
    }

    #[test]
    fn testParseFloat() {
        let s = String::from("1234.");
        let mut offset = 0;
        let res = Exp::parseNumber(s.asArray(), &mut offset);
        assert!(res == PROk(Exp::Float(1234.)));

        let s = String::from("1234.0");
        let mut offset = 0;
        let res = Exp::parseNumber(s.asArray(), &mut offset);
        assert!(res == PROk(Exp::Float(1234.)));

        let s = String::from("-001234.0");
        let mut offset = 0;
        let res = Exp::parseNumber(s.asArray(), &mut offset);
        assert!(res == PROk(Exp::Float(-1234.)));

        let s = String::from("-1234.0");
        let mut offset = 0;
        let res = Exp::parseNumber(s.asArray(), &mut offset);
        assert!(res == PROk(Exp::Float(-1234.)));

        let s = String::from("-1234.0 ");
        let mut offset = 0;
        let res = Exp::parseNumber(s.asArray(), &mut offset);
        assert!(res == PROk(Exp::Float(-1234.)));

        let s = String::from("-1234.0+");
        let mut offset = 0;
        let res = Exp::parseNumber(s.asArray(), &mut offset);
        assert!(res != PROk(Exp::Float(-1234.)));

        let s = String::from("-1234.0a");
        let mut offset = 0;
        let res = Exp::parseNumber(s.asArray(), &mut offset);
        assert!(res != PROk(Exp::Float(-1234.)));

        let s = String::from("-001234.0E10");
        let mut offset = 0;
        let res = Exp::parseNumber(s.asArray(), &mut offset);
        assert!(res == PROk(Exp::Float(-1234.0E10)));

        let s = String::from("-001234.0E-10");
        let mut offset = 0;
        let res = Exp::parseNumber(s.asArray(), &mut offset);
        assert!(res == PROk(Exp::Float(-1234.0E-10)));
    }

    #[test]
    fn testParseString() {
        let s = String::from("\"1234\"");
        let mut offset = 0;
        let res = Exp::parseString(s.asArray(), &mut offset);
        assert!(res == PROk(String::from("1234")));

        let s = String::from("\"1234");
        let mut offset = 0;
        let res = Exp::parseString(s.asArray(), &mut offset);
        assert!(res != PROk(String::from("1234")));
    }

    #[test]
    fn testParseSymbol() {
        let s = String::from("#t");
        let mut offset = 0;
        let res = Exp::parseSymbol(s.asArray(), &mut offset);
        assert!(res == PROk(String::from("#t")));

        let s = String::from("t123");
        let mut offset = 0;
        let res = Exp::parseSymbol(s.asArray(), &mut offset);
        assert!(res == PROk(String::from("t123")));

        let s = String::from("t123(");
        let mut offset = 0;
        let res = Exp::parseSymbol(s.asArray(), &mut offset);
        assert!(res == PROk(String::from("t123")));

        let s = String::from("t123+=");
        let mut offset = 0;
        let res = Exp::parseSymbol(s.asArray(), &mut offset);
        assert!(res == PROk(String::from("t123+=")));

        let s = String::from("12t123");
        let mut offset = 0;
        let res = Exp::parseSymbol(s.asArray(), &mut offset);
        assert!(res != PROk(String::from("12t123")));
    }

    #[test]
    fn testParseList() {

        let cells : [Exp; 3] = [Exp::Symbol(String::from("abcd")), Exp::Int(123), Exp::Symbol(String::from("abc"))];

        let sexp = String::from("(abcd 123 abc)");
        let res : ParseResult<Exp> = Exp::fromSExp(sexp.asArray());
        match res {
            PROk(r) => {
                let mut v = Vec::new();
                for c in cells.iter() {
                    v.pushBack(c.clone());
                }
                let e = Exp::List(Vec::from(v));
                assert!(Exp::eq(&e, &r))
            },
            PRErr(err) => panic!("{}", err.message.toStr())
        }
    }

    #[test]
    fn testParseListString() {
        let sexp = String::from("(abcd 123 abc)");
        let res = Exp::fromSExp(sexp.asArray());
        match res {
            PROk(r) => {
                let s = r.toString();
                assert!(s == "(abcd 123 abc)")
            },
            PRErr(err) => panic!("{}", err.message.toStr())
        }
    }
}
