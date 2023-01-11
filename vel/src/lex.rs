use crate::util::arena::{AString, AStringExt, Arena, ArenaExt};
use crate::util::pos::{span, Pos, Span};
use std::fmt::{Display, Formatter, Result};

/// A character or EOF.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum CharOrEof {
    /// Character.
    Char(char),
    /// EOF, or the end of the input stream.
    Eof,
}
use CharOrEof::*;

impl Display for CharOrEof {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match *self {
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
    /// `(`.
    LParen,
    /// `)`.
    RParen,
    /// `[`.
    LBrack,
    /// `]`.
    RBrack,
    /// `{`.
    LCurly,
    /// `}`.
    RCurly,
    /// `=`.
    Eq,
    /// `==`.
    Eq2,
    /// `~=`.
    Neq,
    /// `*`.
    Ast,
    /// `&&`.
    And,
    /// `||`.
    Or,
    /// `~`.
    Not,
    /// `<`.
    Lt,
    /// `<=`.
    Leq,
    /// `>`.
    Gt,
    /// `>=`.
    Geq,
    /// `+`.
    Plus,
    /// `-`.
    Minus,
    /// `/`.
    Div,
    /// `fn`.
    Fn,
    /// `let`.
    Let,
    /// Number.
    Num(&'arn str),
    /// Binary number.
    BinNum(&'arn str),
    /// Hexadecimal number.
    HexNum(&'arn str),
    /// Identifier.
    Ident(&'arn str),
    /// Line comment, `//...`.
    Comment(&'arn str),
    /// Whitespace.
    Ws(&'arn str),
    /// Error token.
    ErrTok(LexError<'arn>),
}
use Token::*;

/// Vel lexing error.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum LexError<'arn> {
    /// Empty binary number.
    EmptyBinNum(&'arn str),
    /// Empty hexadecimal number.
    EmptyHexNum(&'arn str),
    /// Stray `&`.
    StrayAmp(CharOrEof),
    /// Stray `|`.
    StrayBar(CharOrEof),
    /// Invalid character.
    InvalidChar(char),
}
use LexError::*;

impl Display for Token<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match *self {
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

            ErrTok(e) => match e {
                EmptyBinNum(s) => write!(f, "0b{}", s),
                EmptyHexNum(s) => write!(f, "0x{}", s),
                StrayAmp(_) => write!(f, "&"),
                StrayBar(_) => write!(f, "|"),
                InvalidChar(c) => write!(f, "{}", c),
            },
        }
    }
}

impl Display for LexError<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match *self {
            EmptyBinNum(s) => write!(f, "Empty binary number: 0b{}", s),
            EmptyHexNum(s) => write!(f, "Empty hexadecimal number: 0x{}", s),
            StrayAmp(ce) => write!(f, "Stray & (before {})", ce),
            StrayBar(ce) => write!(f, "Stray | (before {})", ce),
            InvalidChar(c) => write!(f, "Invalid character: {}", c),
        }
    }
}

/// Lexes from a character stream to output Vel tokens.
#[inline]
pub fn lex<'arn>(
    from: Pos,
    ins: impl Iterator<Item = char>,
    out: impl FnMut(Token<'arn>, Span),
    arn: &'arn Arena,
) {
    LexState::new(from, ins, out, arn).lex();
}

/// State of a lexer.
struct LexState<'arn, I, O> {
    /// Position.
    pos: Pos,
    /// Input.
    ins: I,
    /// Output.
    out: O,
    /// Arena.
    arn: &'arn Arena,
}

impl<'ctx, 'arn, I: Iterator<Item = char>, O: FnMut(Token<'arn>, Span)> LexState<'arn, I, O> {
    /// Creates a lex state.
    #[inline]
    fn new(pos: Pos, ins: I, out: O, arn: &'arn Arena) -> Self {
        Self { pos, ins, out, arn }
    }

    /// Inputs one character.
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

    /// Outputs a token and a span.
    #[inline]
    fn out(&mut self, tok: Token<'arn>, span: Span) {
        (self.out)(tok, span);
    }

    /// Creates a new `AString`.
    fn new_astring(&self) -> AString<'arn> {
        self.arn.new_astring()
    }

    /// Lexes, without the head character.
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
            '1'..='9' => return self.lex_nonzero(c, from),
            // Name, starting with '_' or an alphabet character
            '_' => return self.lex_name(c, from),
            _ if c.is_alphabetic() => return self.lex_name(c, from),
            // Starting with a whitespace character
            _ if c.is_whitespace() => return self.lex_ws(c, from),
            // Invalid character
            _ => {
                self.out(ErrTok(InvalidChar(c)), span(from, self.pos));
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

    /// Lexes a line comment, starting with `//`.
    fn lex_comment(mut self, from: Pos) {
        let mut s = self.new_astring();
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
        self.out(Comment(s.into_str()), span(from, self.pos));
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
        self.out(ErrTok(StrayAmp(ce)), span(from, amp_to));
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
        self.out(ErrTok(StrayBar(ce)), span(from, bar_to));
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
        let (s, ce, to) = match self.inc() {
            Eof => ("0", Eof, zero_to),
            Char(c) => match c {
                // Binary number, starting with `0b`
                'b' => return self.lex_bin_num(from),
                // Hexadecimal number, starting with `0x`
                'x' => return self.lex_hex_num(from),
                '_' | '0'..='9' => {
                    let mut s = self.new_astring();
                    s.push('0');
                    s.push(c);
                    loop {
                        let to = self.pos;
                        match self.inc() {
                            Eof => break (s.into_str(), Eof, to),
                            Char(c) => match c {
                                '_' | '0'..='9' => s.push(c),
                                _ => break (s.into_str(), Char(c), to),
                            },
                        }
                    }
                }
                _ => ("0", Char(c), zero_to),
            },
        };
        self.out(Num(s), span(from, to));
        return self.lex_may_any(ce, to);
    }

    /// Lexes a binary number, starting with `0b`.
    fn lex_bin_num(mut self, from: Pos) {
        let mut s = self.new_astring();
        let mut has_digit = false;
        let (ce, to) = loop {
            let to = self.pos;
            match self.inc() {
                Eof => break (Eof, to),
                Char(c) => match c {
                    // Continues for `_` or a digit `0`/`1`.
                    '_' => s.push(c),
                    '0' | '1' => {
                        has_digit = true;
                        s.push(c);
                    }
                    _ => break (Char(c), to),
                },
            }
        };
        let s = s.into_str();
        let tok = if has_digit {
            BinNum(s)
        } else {
            ErrTok(EmptyBinNum(s))
        };
        self.out(tok, span(from, to));
        return self.lex_may_any(ce, to);
    }

    /// Lexes a hexadecimal number, starting with `0x`.
    fn lex_hex_num(mut self, from: Pos) {
        let mut s = self.new_astring();
        let mut has_digit = false;
        let (ce, to) = loop {
            let to = self.pos;
            match self.inc() {
                Eof => break (Eof, to),
                Char(c) => match c {
                    // Continues for `_` or a digit `0`-`9`, `a`-`f`, `A`-`F`.
                    '_' => s.push(c),
                    '0'..='9' | 'a'..='f' | 'A'..='F' => {
                        has_digit = true;
                        s.push(c);
                    }
                    _ => break (Char(c), to),
                },
            }
        };
        let s = s.into_str();
        let tok = if has_digit {
            HexNum(s)
        } else {
            ErrTok(EmptyHexNum(s))
        };
        self.out(tok, span(from, to));
        return self.lex_may_any(ce, to);
    }

    /// Lexes a number, starting with a nonzero digit `1`-`9`.
    fn lex_nonzero(mut self, c: char, from: Pos) {
        let mut s = self.new_astring();
        s.push(c);
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
        self.out(Num(s.into_str()), span(from, to));
        return self.lex_may_any(ce, to);
    }

    /// Lexes a name.
    fn lex_name(mut self, c: char, from: Pos) {
        let mut s = self.new_astring();
        s.push(c);
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
        self.out(self.name_token(s.into_str()), span(from, to));
        return self.lex_may_any(ce, from);
    }

    /// Gets the token for the name.
    fn name_token(&self, s: &'arn str) -> Token<'arn> {
        match s {
            "fn" => Fn,
            "let" => Let,
            _ => Ident(s),
        }
    }

    /// Lexes, starting with a whitespace character.
    fn lex_ws(mut self, c: char, from: Pos) {
        let mut s = self.new_astring();
        s.push(c);
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
        self.out(Ws(s.into_str()), span(from, to));
        return self.lex_may_any(ce, to);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{span, util::pos::pos};
    use std::fmt::Write;

    /// Big text containing all kinds of tokens.
    const BIG: &str = r"() [] {} // xxx
= == ~= *
&& || ~
< <= > >=
+ - /
fn let
123 0b01 0xa0f
abcde
0b__ 0x__
& | ♡";

    /// Tests that lexing and displaying code retrieves the original code.
    #[test]
    fn test_lex_display() {
        let arn = Arena::new();
        let mut buf = arn.new_astring();
        let out = |tok, _| {
            let _ = write!(buf, "{}", tok);
        };
        lex(pos(0, 0), BIG.chars(), out, &arn);
        assert_eq!(BIG, buf);
    }

    /// Tests lexing `s` against `get_want` parametrized over the context.
    fn test_lex(s: &str, want: Vec<(Token, Span)>) {
        let arn = Arena::new();
        let mut res = Vec::new();
        let out = |tok, span| match tok {
            Ws(_) => {}
            _ => res.push((tok, span)),
        };
        lex(pos(0, 0), s.chars(), out, &arn);
        for (ts, want_ts) in res.iter().zip(want.iter()) {
            assert_eq!(ts, want_ts);
        }
        assert!(res.len() == want.len());
    }

    /// Tests lexing all kinds of tokens.
    #[test]
    fn test_lex_all() {
        test_lex(
            BIG,
            vec![
                (LParen, span!((0, 0)..(0, 1))),
                (RParen, span!((0, 1)..(0, 2))),
                (LBrack, span!((0, 3)..(0, 4))),
                (RBrack, span!((0, 4)..(0, 5))),
                (LCurly, span!((0, 6)..(0, 7))),
                (RCurly, span!((0, 7)..(0, 8))),
                (Comment(" xxx\n"), span!((0, 9)..(1, 0))),
                (Eq, span!((1, 0)..(1, 1))),
                (Eq2, span!((1, 2)..(1, 4))),
                (Neq, span!((1, 5)..(1, 7))),
                (Ast, span!((1, 8)..(1, 9))),
                (And, span!((2, 0)..(2, 2))),
                (Or, span!((2, 3)..(2, 5))),
                (Not, span!((2, 6)..(2, 7))),
                (Lt, span!((3, 0)..(3, 1))),
                (Leq, span!((3, 2)..(3, 4))),
                (Gt, span!((3, 5)..(3, 6))),
                (Geq, span!((3, 7)..(3, 9))),
                (Plus, span!((4, 0)..(4, 1))),
                (Minus, span!((4, 2)..(4, 3))),
                (Div, span!((4, 4)..(4, 5))),
                (Fn, span!((5, 0)..(5, 2))),
                (Let, span!((5, 3)..(5, 6))),
                (Num("123"), span!((6, 0)..(6, 3))),
                (BinNum("01"), span!((6, 4)..(6, 8))),
                (HexNum("a0f"), span!((6, 9)..(6, 14))),
                (Ident("abcde"), span!((7, 0)..(7, 5))),
                (ErrTok(EmptyBinNum("__")), span!((8, 0)..(8, 4))),
                (ErrTok(EmptyHexNum("__")), span!((8, 5)..(8, 9))),
                (ErrTok(StrayAmp(Char(' '))), span!((9, 0)..(9, 1))),
                (ErrTok(StrayBar(Char(' '))), span!((9, 2)..(9, 3))),
                (ErrTok(InvalidChar('♡')), span!((9, 4)..(9, 5))),
            ],
        );
    }

    /// Tests lexing identifiers.
    #[test]
    fn test_lex_ident() {
        test_lex(
            "ab1_cde 漢字",
            vec![
                (Ident("ab1_cde"), span!((0, 0)..(0, 7))),
                (Ident("漢字"), span!((0, 8)..(0, 10))),
            ],
        )
    }

    /// Tests lexing numbers.
    #[test]
    fn test_lex_number() {
        test_lex(
            r"0 0_123 1_234_567
0b0101_1010
0xab_01_EF",
            vec![
                (Num("0"), span!((0, 0)..(0, 1))),
                (Num("0_123"), span!((0, 2)..(0, 7))),
                (Num("1_234_567"), span!((0, 8)..(0, 17))),
                (BinNum("0101_1010"), span!((1, 0)..(1, 11))),
                (HexNum("ab_01_EF"), span!((2, 0)..(2, 10))),
            ],
        )
    }
}
