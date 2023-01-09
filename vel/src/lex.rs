use crate::ctx::Ctx;
use crate::intern::Intern;
use crate::util::pos::{span, Pos, Span};
use std::fmt::{Display, Formatter, Result};

/// A character or EOF
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum CharOrEof {
    /// Character
    Char(char),
    /// EOF, or the end of the input stream
    Eof,
}
use CharOrEof::*;

impl Display for CharOrEof {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Char(c) => write!(f, "{}", c),
            Eof => write!(f, "EOF"),
        }
    }
}

/// Vel token.
///
/// Has the information to retrieve the original source code.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum Token<'arn> {
    /// `(`
    LParen,
    /// `)`
    RParen,
    /// `[`
    LBrack,
    /// `]`
    RBrack,
    /// `{`
    LCurly,
    /// `}`
    RCurly,
    /// `=`
    Eq,
    /// `==`
    Eq2,
    /// `~=`
    Neq,
    /// `*`
    Ast,
    /// `&&`
    And,
    /// `||`
    Or,
    /// `~`
    Not,
    /// `<`
    Lt,
    /// `<=`
    Leq,
    /// `>`
    Gt,
    /// `>=`
    Geq,
    /// `+`
    Plus,
    /// `-`
    Minus,
    /// `/`
    Div,
    /// `fn`
    Fn,
    /// `let`
    Let,
    /// Number
    Num(Intern<'arn, str>),
    /// Binary number
    BinNum(Intern<'arn, str>),
    /// Hexadecimal number
    HexNum(Intern<'arn, str>),
    /// Identifier
    Ident(Intern<'arn, str>),
    /// Line comment, `//...`
    Comment(Intern<'arn, str>),
    /// Whitespace
    Ws(Intern<'arn, str>),

    /// Stray `&`
    StrayAmp(CharOrEof),
    /// Stray `|`
    StrayBar(CharOrEof),
    /// Stray character
    InvalidChar(char),
}
use Token::*;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
/// Vel lexing error.
pub enum LexError {
    /// Stray `&`
    EStrayAmp(CharOrEof),
    /// `|`
    EStrayBar(CharOrEof),
    /// Character
    EInvalidChar(char),
}
use LexError::*;

impl Display for Token<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            LParen => write!(f, "("),
            RParen => write!(f, ")"),
            LBrack => write!(f, "["),
            RBrack => write!(f, "]"),
            LCurly => write!(f, "{{"),
            RCurly => write!(f, "}}"),
            Eq => write!(f, "="),
            Eq2 => write!(f, "=="),
            Neq => write!(f, "~="),
            Ast => write!(f, "*"),
            And => write!(f, "&&"),
            Or => write!(f, "||"),
            Not => write!(f, "~"),
            Lt => write!(f, "<"),
            Leq => write!(f, "<="),
            Gt => write!(f, ">"),
            Geq => write!(f, ">="),
            Plus => write!(f, "+"),
            Minus => write!(f, "-"),
            Div => write!(f, "/"),
            Fn => write!(f, "fn"),
            Let => write!(f, "let"),
            Num(s) => write!(f, "{}", s),
            BinNum(s) => write!(f, "0b{}", s),
            HexNum(s) => write!(f, "0x{}", s),
            Ident(s) => write!(f, "{}", s),
            Comment(s) => write!(f, "//{}", s),
            Ws(s) => write!(f, "{}", s),

            StrayAmp(_) => write!(f, "&"),
            StrayBar(_) => write!(f, "|"),
            InvalidChar(c) => write!(f, "{}", c),
        }
    }
}

impl Token<'_> {
    /// Turns a token possibly into a lex error
    pub fn as_error(self) -> Option<LexError> {
        match self {
            StrayAmp(ce) => Some(EStrayAmp(ce)),
            StrayBar(ce) => Some(EStrayBar(ce)),
            InvalidChar(c) => Some(EInvalidChar(c)),
            _ => None,
        }
    }
}

impl Display for LexError {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match *self {
            EStrayAmp(ce) => write!(f, "{} was found after &, but only & is allowed", ce),
            EStrayBar(ce) => write!(f, "{} was found after |, but only | is allowed", ce),
            EInvalidChar(c) => write!(f, "Invalid character: {}", c),
        }
    }
}

/// Lexes from a character stream to output Vel tokens.
#[inline]
pub fn lex<'arn>(
    from: Pos,
    ins: impl Iterator<Item = char>,
    out: impl FnMut(Token<'arn>, Span),
    ctx: &Ctx<'arn>,
) {
    LexState::new(from, ins, out, ctx).lex();
}

/// State of a lexer.
struct LexState<'ctx, 'arn, I, O> {
    /// Position
    pos: Pos,
    /// Input
    ins: I,
    /// Output
    out: O,
    /// Context
    ctx: &'ctx Ctx<'arn>,
}

impl<'ctx, 'arn, I: Iterator<Item = char>, O: FnMut(Token<'arn>, Span)> LexState<'ctx, 'arn, I, O> {
    /// Create a lex state
    #[inline]
    fn new(pos: Pos, ins: I, out: O, ctx: &'ctx Ctx<'arn>) -> Self {
        Self { pos, ins, out, ctx }
    }

    /// Input one character
    #[inline]
    fn inc(&mut self) -> CharOrEof {
        match self.ins.next() {
            None => Eof,
            Some(c) => {
                self.pos = self.pos.after(c);
                Char(c)
            }
        }
    }

    /// Output a token and a span
    #[inline]
    fn out(&mut self, tok: Token<'arn>, span: Span) {
        (self.out)(tok, span);
    }

    /// Intern a string
    #[inline]
    fn str(&self, s: &str) -> Intern<'arn, str> {
        self.ctx.str(s)
    }

    /// Lexes, without the head character
    #[inline]
    fn lex(mut self) {
        let from = self.pos;
        let ce = self.inc();
        return self.lex_may_any(ce, from);
    }

    /// Lexes, starting possibly with a character.
    #[inline]
    fn lex_may_any(self, ce: CharOrEof, from: Pos) {
        if let Char(c) = ce {
            return self.lex_any(c, from);
        }
    }

    /// Lexes, starting with any character.
    fn lex_any(mut self, c: char, from: Pos) {
        let tok = match c {
            // Determined at this point
            '(' => LParen,
            ')' => RParen,
            '[' => LBrack,
            ']' => RBrack,
            '{' => LCurly,
            '}' => RCurly,
            '*' => Ast,
            '+' => Plus,
            '-' => Minus,
            // Starting with `=`
            '=' => return self.lex_eq(from),
            // Starting with `~`
            '~' => return self.lex_tilde(from),
            // Starting with `/`
            '/' => return self.lex_slash(from),
            // Starting with `&`
            '&' => return self.lex_amp(from),
            // Starting with `|`
            '|' => return self.lex_bar(from),
            // Starting with `<`
            '<' => return self.lex_lt(from),
            // Starting with `>`
            '>' => return self.lex_gt(from),
            // Starting with zero
            '0' => return self.lex_zero(from),
            // Number, starting with a nonzero digit, `1`-`9`
            '1'..='9' => return self.lex_num(c, from),
            // Name, starting with '_' or an alphabet character
            '_' => return self.lex_name(c, from),
            _ if c.is_alphabetic() => return self.lex_name(c, from),
            // Starting with a whitespace character
            _ if c.is_whitespace() => return self.lex_ws(c, from),
            // Invalid character
            _ => {
                self.out(InvalidChar(c), span(from, self.pos));
                return self.lex();
            }
        };
        self.out(tok, span(from, self.pos));
        return self.lex();
    }

    /// Lexes, starting with `=`.
    fn lex_eq(mut self, from: Pos) {
        let eq_to = self.pos;
        let ce = match self.inc() {
            Eof => Eof,
            Char(c) => match c {
                // `==`
                '=' => {
                    let eq2_to = self.pos;
                    self.out(Eq2, span(from, eq2_to));
                    return self.lex();
                }
                _ => Char(c),
            },
        };
        // `=`
        self.out(Eq, span(from, eq_to));
        return self.lex_may_any(ce, eq_to);
    }

    /// Lexes, starting with `~`.
    fn lex_tilde(mut self, from: Pos) {
        let not_to = self.pos;
        let ce = match self.inc() {
            Eof => Eof,
            Char(c) => match c {
                // `~=`
                '=' => {
                    let neq_to = self.pos;
                    self.out(Neq, span(from, neq_to));
                    return self.lex();
                }
                _ => Char(c),
            },
        };
        // `~`
        self.out(Not, span(from, not_to));
        return self.lex_may_any(ce, not_to);
    }

    /// Lexes, starting with `/`.
    fn lex_slash(mut self, from: Pos) {
        let slash_to = self.pos;
        let ce = match self.inc() {
            Eof => Eof,
            Char(c) => match c {
                // Comment, starting with `//`
                '/' => return self.lex_comment(from),
                _ => Char(c),
            },
        };
        // `/`
        self.out(Div, span(from, slash_to));
        return self.lex_may_any(ce, slash_to);
    }

    /// Lexes a comment, starting with `//`.
    fn lex_comment(mut self, from: Pos) {
        // Lexes a line comment
        let mut s = String::new();
        loop {
            match self.inc() {
                Eof => break,
                Char(c) => {
                    s.push(c);
                    if c == '\n' {
                        break;
                    }
                }
            }
        }
        self.out(Comment(self.str(&s)), span(from, self.pos));
        return self.lex();
    }

    /// Lexes, starting with `&`.
    fn lex_amp(mut self, from: Pos) {
        let amp_to = self.pos;
        let ce = match self.inc() {
            Eof => Eof,
            Char(c) => match c {
                // `&&`
                '&' => {
                    let and_to = self.pos;
                    self.out(And, span(from, and_to));
                    return self.lex();
                }
                _ => Char(c),
            },
        };
        // `&`
        self.out(StrayAmp(ce), span(from, amp_to));
        return self.lex_may_any(ce, amp_to);
    }

    /// Lexes, starting with `|`.
    fn lex_bar(mut self, from: Pos) {
        let bar_to = self.pos;
        let ce = match self.inc() {
            Eof => Eof,
            Char(c) => match c {
                // `||`
                '|' => {
                    let or_to = self.pos;
                    self.out(Or, span(from, or_to));
                    return self.lex();
                }
                _ => Char(c),
            },
        };
        // `|`
        self.out(StrayBar(ce), span(from, bar_to));
        return self.lex_may_any(ce, bar_to);
    }

    /// Lexes, starting with `<`.
    fn lex_lt(mut self, from: Pos) {
        let lt_to = self.pos;
        let ce = match self.inc() {
            Eof => Eof,
            Char(c) => match c {
                // `<=`
                '=' => {
                    let leq_to = self.pos;
                    self.out(Leq, span(from, leq_to));
                    return self.lex();
                }
                _ => Char(c),
            },
        };
        // `<`
        self.out(Lt, span(from, lt_to));
        return self.lex_may_any(ce, lt_to);
    }

    /// Lexes, starting with `>`.
    fn lex_gt(mut self, from: Pos) {
        let gt_to = self.pos;
        let ce = match self.inc() {
            Eof => Eof,
            Char(c) => match c {
                // `>=`
                '=' => {
                    let geq_to = self.pos;
                    self.out(Geq, span(from, geq_to));
                    return self.lex();
                }
                _ => Char(c),
            },
        };
        // `>`
        self.out(Gt, span(from, gt_to));
        return self.lex_may_any(ce, gt_to);
    }

    /// Lexes, starting with zero.
    fn lex_zero(mut self, from: Pos) {
        let zero_to = self.pos;
        let ce = match self.inc() {
            Eof => Eof,
            Char(c) => match c {
                // Binary number, starting with `0b`
                'b' => return self.lex_bin_num(from),
                // Hexadecimal number, starting with `0x`
                'x' => return self.lex_hex_num(from),
                _ => Char(c),
            },
        };
        // `0`
        self.out(Num(self.str("0")), span(from, zero_to));
        return self.lex_may_any(ce, zero_to);
    }

    /// Lexes a binary number, starting with `0b`.
    fn lex_bin_num(mut self, from: Pos) {
        let mut s = String::new();
        let (ce, to) = loop {
            let to = self.pos;
            match self.inc() {
                Eof => break (Eof, to),
                Char(c) => match c {
                    // Continues for `_` or a digit `0`/`1`.
                    '_' | '0' | '1' => s.push(c),
                    _ => break (Char(c), to),
                },
            }
        };
        self.out(BinNum(self.str(&s)), span(from, to));
        return self.lex_may_any(ce, to);
    }

    /// Lexes a hexadecimal number, starting with `0x`.
    fn lex_hex_num(mut self, from: Pos) {
        let mut s = String::new();
        let (ce, to) = loop {
            let to = self.pos;
            match self.inc() {
                Eof => break (Eof, to),
                Char(c) => match c {
                    // Continues for `_` or a digit `0`-`9`, `a`-`f`.
                    '_' | '0'..='9' | 'a'..='f' => s.push(c),
                    _ => break (Char(c), to),
                },
            }
        };
        self.out(HexNum(self.str(&s)), span(from, to));
        return self.lex_may_any(ce, to);
    }

    /// Lexes a number, starting with a digit `1`-`9`.
    fn lex_num(mut self, c: char, from: Pos) {
        let mut s = c.to_string();
        let (ce, to) = loop {
            let to = self.pos;
            match self.inc() {
                Eof => break (Eof, to),
                Char(c) => match c {
                    // Continues for `_` or a digit `0`-`9`.
                    '_' | '0'..='9' => s.push(c),
                    _ => break (Char(c), to),
                },
            }
        };
        self.out(Num(self.str(&s)), span(from, to));
        return self.lex_may_any(ce, to);
    }

    /// Lexes a name.
    fn lex_name(mut self, c: char, from: Pos) {
        let mut s = c.to_string();
        let (ce, to) = loop {
            let to = self.pos;
            match self.inc() {
                Eof => break (Eof, to),
                Char(c) => match c {
                    // Continues for `_`, `0`-`9`, or an alphabet character.
                    '_' | '0'..='9' => s.push(c),
                    _ if c.is_alphabetic() => s.push(c),
                    _ => break (Char(c), to),
                },
            };
        };
        self.out(self.name_token(&s), span(from, to));
        return self.lex_may_any(ce, from);
    }

    /// Gets the token for the name.
    fn name_token(&self, s: &str) -> Token<'arn> {
        match s {
            "fn" => Fn,
            "let" => Let,
            _ => Ident(self.str(s)),
        }
    }

    /// Lexes, starting with a whitespace character.
    fn lex_ws(mut self, c: char, from: Pos) {
        let mut s = c.to_string();
        let (ce, to) = loop {
            let to = self.pos;
            match self.inc() {
                Eof => break (Eof, to),
                Char(c) => match c {
                    // Continues for a whitespace character.
                    _ if c.is_whitespace() => s.push(c),
                    _ => break (Char(c), to),
                },
            }
        };
        self.out(Ws(self.str(&s)), span(from, to));
        return self.lex_may_any(ce, to);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{span, util::pos::pos};
    use bumpalo::Bump;
    use std::fmt::Write;

    /// Big code containing all kinds of tokens
    const BIG: &str = r"
() [] {} // xxx
= == ~= *
&& || ~
< <= > >=
+ - /
fn let
123 0b01 0xa0f
abcde
& | ♡";

    /// Tests that lexing and displaying code retrieves the original code.
    #[test]
    fn test_lex_display() {
        let mut buf = String::new();
        let arena = Bump::new();
        let ctx = Ctx::new(&arena);
        let out = |tok, _| {
            let _ = write!(buf, "{}", tok);
        };
        lex(pos(0, 0), BIG.chars(), out, &ctx);
        assert_eq!(BIG, buf);
    }

    /// Tests lexing `s` against `get_want` parametrized over the context.
    fn test_lex(s: &str, get_want: impl for<'arn> FnOnce(&Ctx<'arn>) -> Vec<(Token<'arn>, Span)>) {
        let arena = Bump::new();
        let ctx = Ctx::new(&arena);
        let mut res = Vec::new();
        let out = |tok, span| match tok {
            Ws(_) => {}
            _ => res.push((tok, span)),
        };
        lex(pos(0, 0), s.chars(), out, &ctx);
        let want = get_want(&ctx);
        for (ts, want_ts) in res.iter().zip(want.iter()) {
            assert_eq!(ts, want_ts);
        }
        assert!(res.len() == want.len());
    }

    /// Tests lexing all kinds of tokens.
    #[test]
    fn test_lex_all() {
        test_lex(BIG, |ctx| {
            vec![
                (LParen, span!((1, 0)..(1, 1))),
                (RParen, span!((1, 1)..(1, 2))),
                (LBrack, span!((1, 3)..(1, 4))),
                (RBrack, span!((1, 4)..(1, 5))),
                (LCurly, span!((1, 6)..(1, 7))),
                (RCurly, span!((1, 7)..(1, 8))),
                (Comment(ctx.str(" xxx\n")), span!((1, 9)..(2, 0))),
                (Eq, span!((2, 0)..(2, 1))),
                (Eq2, span!((2, 2)..(2, 4))),
                (Neq, span!((2, 5)..(2, 7))),
                (Ast, span!((2, 8)..(2, 9))),
                (And, span!((3, 0)..(3, 2))),
                (Or, span!((3, 3)..(3, 5))),
                (Not, span!((3, 6)..(3, 7))),
                (Lt, span!((4, 0)..(4, 1))),
                (Leq, span!((4, 2)..(4, 4))),
                (Gt, span!((4, 5)..(4, 6))),
                (Geq, span!((4, 7)..(4, 9))),
                (Plus, span!((5, 0)..(5, 1))),
                (Minus, span!((5, 2)..(5, 3))),
                (Div, span!((5, 4)..(5, 5))),
                (Fn, span!((6, 0)..(6, 2))),
                (Let, span!((6, 3)..(6, 6))),
                (Num(ctx.str("123")), span!((7, 0)..(7, 3))),
                (BinNum(ctx.str("01")), span!((7, 4)..(7, 8))),
                (HexNum(ctx.str("a0f")), span!((7, 9)..(7, 14))),
                (Ident(ctx.str("abcde")), span!((8, 0)..(8, 5))),
                (StrayAmp(Char(' ')), span!((9, 0)..(9, 1))),
                (StrayBar(Char(' ')), span!((9, 2)..(9, 3))),
                (InvalidChar('♡'), span!((9, 4)..(9, 5))),
            ]
        });
    }

    /// Tests lexing identifiers.
    #[test]
    fn test_lex_ident() {
        test_lex("ab1_cde 漢字", |ctx| {
            vec![
                (Ident(ctx.str("ab1_cde")), span!((0, 0)..(0, 7))),
                (Ident(ctx.str("漢字")), span!((0, 8)..(0, 10))),
            ]
        })
    }

    /// Tests lexing numbers.
    #[test]
    fn test_lex_number() {
        test_lex(
            r"0 1_234_567
0b0101_1010
0xab_01_ef",
            |ctx| {
                vec![
                    (Num(ctx.str("0")), span!((0, 0)..(0, 1))),
                    (Num(ctx.str("1_234_567")), span!((0, 2)..(0, 11))),
                    (BinNum(ctx.str("0101_1010")), span!((1, 0)..(1, 11))),
                    (HexNum(ctx.str("ab_01_ef")), span!((2, 0)..(2, 10))),
                ]
            },
        )
    }
}
