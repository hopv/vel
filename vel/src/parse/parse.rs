//! Vel parser.

use std::marker::PhantomData;
use std::mem::replace;
use std::ops::Range;

use super::lex::{Eof, Just, Lexer, OrEof, Token};

/// Copy.
pub trait CopyExt {
    fn copy(&self) -> Self;
}

impl<T: Copy> CopyExt for Range<T> {
    /// Copy a range.
    fn copy(&self) -> Range<T> {
        self.start..self.end
    }
}

/// Contents of a whole file.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Whole {
    /// Top-level items.
    pub top_levels: Vec<TopLevel>,
    /// Gap before EOF.
    pub gap_eof: Gap,
}

/// Top-level item.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum TopLevel {
    /// Function definition.
    FnDef(FnDef),
}

pub use TopLevel::*;

/// Function definition.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct FnDef {
    /// Gap before `fn`.
    pub gap_fn: Gap,
    /// Function name.
    pub name: Ident,
    /// External lifetimes.
    pub ext_lfts: BrackedCommaed<LftIdent>,
    /// Inputs.
    pub inputs: ParenedCommaed<Entry>,
    /// Outputs.
    pub outputs: ParenedCommaed<Entry>,
    /// Body.
    pub body: Stmt,
}

/// Entry.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Entry {
    /// Name of the argument.
    pub name: Ident,
    /// Type of the argument.
    pub ty: Ty,
}

/// Type.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Ty {
    /// Name.
    NameTy(Ident),
}
pub use Ty::*;

/// Statement.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Stmt {
    /// Empty
    Empty,
}
pub use Stmt::*;

/// Identifier.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Ident {
    gap: Gap,
    span: Range<usize>,
}

impl Ident {
    /// The string for the identifier.
    pub fn str<'a>(&self, s: &'a str) -> &'a str {
        &s[self.span.copy()]
    }
}

/// Lifetime identifier.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct LftIdent {
    gap: Gap,
    span: Range<usize>,
}

impl LftIdent {
    /// The string for the lifetime identifier.
    pub fn str<'a>(&self, s: &'a str) -> &'a str {
        &s[self.span.copy()]
    }
}

/// Comma-separated items delimited by a delimiter.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct DelimedCommaed<Delim, T> {
    /// Phantom data of the delimiter.
    pub delim: PhantomData<Delim>,
    /// Gap before `{`.
    pub gap_open: Gap,
    /// Items.
    pub items: Box<[T]>,
    /// Gap before `}`.
    pub gap_close: Gap,
}

/// Block delimited by parentheses.
pub type ParenedCommaed<T> = DelimedCommaed<Paren, T>;
/// Parentheses `(`, `)`.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Paren {}

/// Block delimited by square brackets.
pub type BrackedCommaed<T> = DelimedCommaed<Brack, T>;
/// Square Brackets `[`, `]`.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Brack {}

/// Block delimited by curly braces.
pub type CurlyedCommaed<T> = DelimedCommaed<Curly, T>;
/// Curly braces `{`, `}`.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Curly {}

/// Gap tokens.
pub type Gap = Vec<(Token, Range<usize>)>;

/// Parser.
pub struct Parser<'a> {
    /// Lexer.
    lexer: Lexer<'a>,
    /// Head token.
    head: OrEof<(Token, Range<usize>)>,
    /// Gap.
    gap: Gap,
}

impl<'a> Parser<'a> {
    /// Creates a new parser.
    pub fn new(s: &'a str) -> Self {
        let mut lexer = Lexer::new(s);
        let mut gap = Vec::new();
        let head = loop {
            match lexer.next() {
                None => break Eof,
                Some((tok, span)) => {
                    if Self::is_gap_tok(&tok) {
                        gap.push((tok, span));
                        continue;
                    } else {
                        break Just((tok, span));
                    }
                }
            }
        };
        Self { lexer, head, gap }
    }

    /// Moves to the next token, returning the current state.
    fn mov(&mut self) -> (Token, Range<usize>, Vec<(Token, Range<usize>)>) {
        let old_head = match replace(&mut self.head, Eof) {
            Eof => panic!("Should not call mov when the head is EOF"),
            Just(head) => head,
        };
        let old_gap = replace(&mut self.gap, Vec::new());
        self.head = loop {
            match self.lexer.next() {
                None => break Eof,
                Some((tok, span)) => {
                    if Self::is_gap_tok(&tok) {
                        self.gap.push((tok, span));
                        continue;
                    } else {
                        break Just((tok, span));
                    }
                }
            }
        };
        (old_head.0, old_head.1, old_gap)
    }

    /// Judges if the token always goes to a gap.
    fn is_gap_tok(tok: &Token) -> bool {
        match tok {
            Token::Whitespace { .. }
            | Token::LineComment { .. }
            | Token::BlockComment { .. }
            | Token::Error { .. } => true,
            _ => false,
        }
    }

    pub fn parse<T: Parse>(&mut self) -> T {
        T::parse(self)
    }
}

/// Parsable.
pub trait Parse {
    fn parse(parser: &mut Parser<'_>) -> Self;
}

impl Parse for Whole {
    fn parse(parser: &mut Parser<'_>) -> Self {
        let fn_def = parser.parse();
        let gap_eof = replace(&mut parser.gap, Vec::new());
        Whole {
            top_levels: vec![FnDef(fn_def)],
            gap_eof,
        }
    }
}

impl Parse for FnDef {
    fn parse(parser: &mut Parser<'_>) -> Self {
        let gap_fn = match parser.mov() {
            (Token::Fn, _, gap) => gap,
            _ => todo!(),
        };
        let name = parser.parse();
        let ext_lfts = parser.parse();
        let inputs = parser.parse();
        match parser.mov() {
            (Token::DashGt, _, _) => {}
            _ => todo!(),
        }
        let outputs = parser.parse();
        let body = parser.parse();
        FnDef {
            gap_fn,
            name,
            ext_lfts,
            inputs,
            outputs,
            body,
        }
    }
}

impl Parse for Stmt {
    fn parse(parser: &mut Parser<'_>) -> Self {
        match parser.mov() {
            (Token::LCurly, _, _) => {}
            _ => todo!(),
        }
        match parser.mov() {
            (Token::RCurly, _, _) => {}
            _ => todo!(),
        }
        Empty
    }
}

impl Parse for Entry {
    fn parse(parser: &mut Parser<'_>) -> Self {
        let name = parser.parse();
        match parser.mov() {
            (Token::Colon, _, _) => {}
            _ => todo!(),
        }
        let ty = parser.parse();
        Entry { name, ty }
    }
}

impl Parse for Ty {
    fn parse(parser: &mut Parser<'_>) -> Self {
        let name = parser.parse();
        NameTy(name)
    }
}

impl Parse for Ident {
    fn parse(parser: &mut Parser<'_>) -> Self {
        match parser.mov() {
            (Token::Ident, span, gap) => Ident { gap, span },
            _ => todo!(),
        }
    }
}

impl Parse for LftIdent {
    fn parse(parser: &mut Parser<'_>) -> Self {
        match parser.mov() {
            (Token::LftIdent, span, gap) => LftIdent { gap, span },
            _ => todo!(),
        }
    }
}

impl<D: Delim, T: Parse> Parse for DelimedCommaed<D, T> {
    fn parse(parser: &mut Parser<'_>) -> Self {
        let gap_open = match parser.mov() {
            (tok, _, gap) if tok == D::open() => gap,
            _ => todo!(),
        };
        let mut items = vec![T::parse(parser)];
        loop {
            match parser.head {
                Just((Token::Comma, _)) => {
                    let _ = parser.mov();
                    items.push(parser.parse());
                    continue;
                }
                _ => break,
            }
        }
        let gap_close = match parser.mov() {
            (tok, _, gap) if tok == D::close() => gap,
            _ => todo!(),
        };
        DelimedCommaed {
            delim: PhantomData,
            gap_open,
            items: items.into(),
            gap_close,
        }
    }
}

/// Delimiter.
pub trait Delim {
    /// Open delimiter.
    fn open() -> Token;
    /// Close delimiter.
    fn close() -> Token;
}

impl Delim for Paren {
    #[inline]
    fn open() -> Token {
        Token::LParen
    }
    #[inline]
    fn close() -> Token {
        Token::RParen
    }
}

impl Delim for Brack {
    #[inline]
    fn open() -> Token {
        Token::LBrack
    }
    #[inline]
    fn close() -> Token {
        Token::RBrack
    }
}

impl Delim for Curly {
    #[inline]
    fn open() -> Token {
        Token::LCurly
    }
    #[inline]
    fn close() -> Token {
        Token::RCurly
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Parses.
    #[test]
    fn test() {
        let text = r"// Function
fn foo['lft](a: I64, b: U64) -> (c: I128) {
}";
        let mut parser = Parser::new(text);
        let whole: Whole = parser.parse();
        match whole.top_levels.as_slice() {
            [FnDef(FnDef {
                name,
                ext_lfts: DelimedCommaed {
                    items: ext_lfts, ..
                },
                inputs,
                outputs,
                body: Empty,
                ..
            })] => {
                assert_eq!(name.str(text), "foo");
                match ext_lfts.as_ref() {
                    [lft] => assert_eq!(lft.str(text), "'lft"),
                    _ => panic!(),
                }
                match inputs.items.as_ref() {
                    [Entry {
                        name: name_a,
                        ty: NameTy(ty_name_a),
                    }, Entry {
                        name: name_b,
                        ty: NameTy(ty_name_b),
                    }] => {
                        assert_eq!(name_a.str(text), "a");
                        assert_eq!(ty_name_a.str(text), "I64");
                        assert_eq!(name_b.str(text), "b");
                        assert_eq!(ty_name_b.str(text), "U64");
                    }
                    _ => panic!(),
                }
                match outputs.items.as_ref() {
                    [Entry {
                        name,
                        ty: NameTy(ty_name),
                    }] => {
                        assert_eq!(name.str(text), "c");
                        assert_eq!(ty_name.str(text), "I128");
                    }
                    _ => panic!(),
                }
            }
            _ => panic!(),
        }
    }
}
