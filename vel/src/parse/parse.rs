//! Vel parser.

use super::cst::{Entry, FnDef, NameTy, Stmt, TopLevel, Whole};
use super::lex::{Eof, Just, Lexer, OrEof, Token};
use std::mem::replace;
use std::ops::Range;

/// Parser.
pub struct Parser<'a> {
    /// String.
    s: &'a str,
    /// Lexer.
    lexer: Lexer<'a>,
    /// Head token.
    head: OrEof<(Token, Range<usize>)>,
    /// Ignored tokens.
    ignored: Vec<(Token, Range<usize>)>,
}

impl<'a> Parser<'a> {
    /// Creates a new parser.
    pub fn new(s: &'a str) -> Self {
        let mut lexer = Lexer::new(s);
        let mut ignored = Vec::new();
        let head = loop {
            match lexer.next() {
                None => break Eof,
                Some((tok, span)) => {
                    if Self::is_ignored_tok(&tok) {
                        ignored.push((tok, span));
                        continue;
                    } else {
                        break Just((tok, span));
                    }
                }
            }
        };
        Self {
            s,
            lexer,
            head,
            ignored,
        }
    }

    /// Moves to the next token, returning the current state.
    fn mov(&mut self) -> (Token, Range<usize>, Vec<(Token, Range<usize>)>) {
        let old_head = match replace(&mut self.head, Eof) {
            Eof => panic!("Should not call mov when the head is EOF"),
            Just(head) => head,
        };
        let old_ignored = replace(&mut self.ignored, Vec::new());
        self.head = loop {
            match self.lexer.next() {
                None => break Eof,
                Some((tok, span)) => {
                    if Self::is_ignored_tok(&tok) {
                        self.ignored.push((tok, span));
                        continue;
                    } else {
                        break Just((tok, span));
                    }
                }
            }
        };
        (old_head.0, old_head.1, old_ignored)
    }

    /// Judges if the token is ignored always.
    fn is_ignored_tok(tok: &Token) -> bool {
        match tok {
            Token::Whitespace { .. }
            | Token::LineComment { .. }
            | Token::BlockComment { .. }
            | Token::Error { .. } => true,
            _ => false,
        }
    }

    /// Parses a whole file.
    pub fn parse_whole(&mut self) -> Whole<'a> {
        let mut top_levels = Vec::new();
        loop {
            match &self.head {
                Eof => break,
                Just(_) => top_levels.push(self.parse_top_level()),
            }
        }
        Whole { top_levels }
    }

    /// Parses a top-level item.
    pub fn parse_top_level(&mut self) -> TopLevel<'a> {
        match self.mov().0 {
            Token::Fn => {
                let fn_def = self.parse_fn_def();
                TopLevel::FnDef(fn_def)
            }
            _ => todo!(),
        }
    }

    /// Parses a function definition, with `fn` consumed.
    pub fn parse_fn_def(&mut self) -> FnDef<'a> {
        let name = match self.mov() {
            (Token::Ident, r, _) => &self.s[r],
            _ => todo!(),
        };
        match self.mov().0 {
            Token::LBrack => {}
            _ => todo!(),
        };
        let stat_ins = self.parse_args();
        match self.mov().0 {
            Token::RBrack => {}
            _ => todo!(),
        };
        match self.mov().0 {
            Token::LParen => {}
            _ => todo!(),
        };
        let dyn_ins = self.parse_args();
        match self.mov().0 {
            Token::RParen => {}
            _ => todo!(),
        };
        match self.mov().0 {
            Token::DashGt => {}
            _ => todo!(),
        };
        match self.mov().0 {
            Token::LParen => {}
            _ => todo!(),
        };
        let outs = self.parse_args();
        match self.mov().0 {
            Token::RParen => {}
            _ => todo!(),
        };
        let body = self.parse_stmt();
        FnDef {
            name,
            stat_ins,
            dyn_ins,
            outs,
            body,
        }
    }

    pub fn parse_args(&mut self) -> Vec<Entry<'a>> {
        let mut args = Vec::new();
        args.push(self.parse_arg());
        loop {
            match &self.head {
                Just((Token::Comma, _)) => {
                    self.mov();
                    args.push(self.parse_arg());
                }
                _ => break,
            }
        }
        args
    }

    pub fn parse_arg(&mut self) -> Entry<'a> {
        let name = match self.mov() {
            (Token::Ident, r, _) => &self.s[r],
            _ => todo!(),
        };
        match self.mov().0 {
            Token::Colon => {}
            _ => todo!(),
        };
        let ty_name = match self.mov() {
            (Token::Ident, r, _) => &self.s[r],
            _ => todo!(),
        };
        Entry {
            name,
            ty: NameTy(ty_name),
        }
    }

    pub fn parse_stmt(&mut self) -> Stmt {
        match self.mov().0 {
            Token::LCurly => {}
            _ => todo!(),
        };
        match self.mov().0 {
            Token::RCurly => {}
            _ => todo!(),
        };
        Stmt::Empty
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Parses.
    #[test]
    fn parse() {
        let text = r"// Function
fn foo[i: int](a: I64, b: U64) -> (c: I64) {
}";
        let mut parser = Parser::new(text);
        assert_eq!(
            parser.parse_whole(),
            Whole {
                top_levels: vec![FnDef(FnDef {
                    name: "foo",
                    stat_ins: vec![Entry {
                        name: "i",
                        ty: NameTy("int")
                    }],
                    dyn_ins: vec![
                        Entry {
                            name: "a",
                            ty: NameTy("I64")
                        },
                        Entry {
                            name: "b",
                            ty: NameTy("U64")
                        }
                    ],
                    outs: vec![Entry {
                        name: "c",
                        ty: NameTy("I64")
                    }],
                    body: Stmt::Empty
                })]
            }
        );
    }
}
