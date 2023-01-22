//! Vel parser.

use std::marker::PhantomData;
use std::mem::replace;

use super::lex::{LexErr, Lexer, Span, Token};
use crate::util::basic::{CopyExt, Eof, Just, OrEof};

////////////////////////////////////////////////////////////////////////////////

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
    span: Span,
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
    span: Span,
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

/// Gap.
pub type Gap = Vec<(GapToken, Span)>;

/// Token in a gap
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum GapToken {
    /// Line comment, `//...`.
    LineComment,
    /// Block comment, `/* ... */` (nestable).
    BlockComment,
    /// Whitespace.
    Whitespace {
        /// The number of newlines `\n`.
        newline_cnt: usize,
    },
    /// Lexing error.
    LexErr(LexErr),
    /// Skipped token
    Skipped(Token),
}
use GapToken::*;

////////////////////////////////////////////////////////////////////////////////

/// Parser.
pub struct Parser<'a> {
    /// Lexer.
    lexer: Lexer<'a>,
    /// Head token.
    head: OrEof<(Token, Span)>,
    /// Gap.
    gap: Gap,
}

impl<'a> Parser<'a> {
    /// Creates a new parser.
    pub fn new(s: &'a str) -> Self {
        let mut res = Self {
            lexer: Lexer::new(s),
            head: Eof,
            gap: Vec::new(),
        };
        res.skim();
        res
    }

    /// Gets the head token.
    fn head_tok(&self) -> OrEof<&Token> {
        match &self.head {
            Eof => Eof,
            Just((tok, _)) => Just(tok),
        }
    }

    /// Moves to the next token, returning the current state.
    fn mov(&mut self) -> (Token, Span, Gap) {
        let old_head = match replace(&mut self.head, Eof) {
            Eof => panic!("Should not call mov when the head is EOF"),
            Just(head) => head,
        };
        let old_gap = replace(&mut self.gap, Vec::new());
        self.skim();
        (old_head.0, old_head.1, old_gap)
    }

    /// Inputs tokens until reaching a non-gap token and sets the head token.
    fn skim(&mut self) {
        self.head = loop {
            match self.lexer.next() {
                None => break Eof,
                Some((tok, span)) => {
                    let gaptok = match tok {
                        Token::Whitespace { newline_cnt } => Whitespace { newline_cnt },
                        Token::LineComment => LineComment,
                        Token::BlockComment => BlockComment,
                        Token::Error(e) => LexErr(e),
                        _ => break Just((tok, span)),
                    };
                    self.gap.push((gaptok, span));
                }
            }
        }
    }

    /// Skip the head token.
    pub fn skip(&mut self) {
        let (tok, span) = replace(&mut self.head, Eof).unwrap();
        self.gap.push((Skipped(tok), span));
        self.skim();
    }

    /// Parses an object.
    pub fn parse<T: Parse>(&mut self) -> T {
        T::parse(self)
    }

    /// Damps the current gap.
    pub fn damp_gap(&mut self) -> Gap {
        replace(&mut self.gap, Vec::new())
    }
}

/// Parsable.
pub trait Parse {
    fn parse(parser: &mut Parser<'_>) -> Self;
}

////////////////////////////////////////////////////////////////////////////////

impl Parse for Whole {
    fn parse(parser: &mut Parser<'_>) -> Self {
        let mut top_levels = Vec::new();
        loop {
            match parser.head_tok() {
                Eof => break,
                // Function definition.
                Just(Token::Fn) => top_levels.push(FnDef(parser.parse())),
                // Skips when the head token is not a top-level item.
                _ => parser.skip(),
            }
        }
        let gap_eof = parser.damp_gap();
        Whole {
            top_levels,
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

////////////////////////////////////////////////////////////////////////////////

#[cfg(test)]
mod tests {
    use super::*;

    /// Parses.
    #[test]
    fn test() {
        let text = r"+ // Function
fn foo['lft](a: I64, b: U64) -> (c: I128) {
}";
        let mut parser = Parser::new(text);
        let whole: Whole = parser.parse();
        match whole.top_levels.as_slice() {
            [FnDef(FnDef {
                gap_fn,
                name,
                ext_lfts: DelimedCommaed {
                    items: ext_lfts, ..
                },
                inputs,
                outputs,
                body: Empty,
            })] => {
                assert_eq!(&gap_fn[0].0, &Skipped(Token::Plus));
                assert_eq!(&gap_fn[2].0, &LineComment);
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
