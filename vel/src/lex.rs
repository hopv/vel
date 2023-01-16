//! Vel lexer.

use crate::util::pos::{span, Pos, Span};
use std::fmt::{Display, Formatter, Result};

/// Something or EOF.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum OrEof<T> {
    /// Has a content.
    Just(T),
    /// EOF, or the end of the input stream.
    Eof,
}
use OrEof::*;

impl<T> From<Option<T>> for OrEof<T> {
    fn from(o: Option<T>) -> Self {
        match o {
            Some(v) => Just(v),
            None => Eof,
        }
    }
}

impl<T: Display> Display for OrEof<T> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Just(v) => write!(f, "{}", v),
            Eof => write!(f, "EOF"),
        }
    }
}

/// Vel token.
///
/// Has the information to retrieve the original source code.
#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum Token {
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
    /// `,`.
    Comma,
    /// `;`.
    Semi,
    /// `:`.
    Colon,
    /// `.`.
    Dot,
    /// `..`.
    Dot2,
    /// `..=`.
    Dot2Eq,
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
    /// Number literal.
    Num { body: NumLit, val: i64 },
    /// Identifier.
    Ident { name: String },
    /// Line comment, `//...`.
    LineComment { body: String },
    /// Block comment, `/* ... */` (nestable).
    BlockComment { body: String },
    /// Whitespace.
    Whitespace {
        str: String,
        /// The number of newlines `\n`.
        nl_cnt: usize,
    },
    /// Error token.
    Error(LexErr),
}
pub use Token::*;

/// Utility for creating a number literal token.
#[inline]
pub fn num(body: NumLit, val: i64) -> Token {
    Num { body, val }
}

/// Number literal.
#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum NumLit {
    /// Decimal.
    Dec(String),
    /// Binary.
    Bin(String),
    /// Hexadecimal.
    Hex(String),
}
pub use NumLit::*;

/// Vel lexing error.
#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum LexErr {
    /// Empty binary number.
    EmptyBinNum { body: String },
    /// Empty hexadecimal number.
    EmptyHexNum { body: String },
    /// Stray `&`.
    StrayAmp {
        /// The character after `&`.
        next: OrEof<char>,
    },
    /// Stray `|`.
    StrayBar {
        /// The character after `|`.
        next: OrEof<char>,
    },
    /// Unclosed block comment.
    UnclosedBlockComment { body: String, open_cnt: usize },
    /// Invalid character.
    InvalidChar { c: char },
}
pub use LexErr::*;

impl Display for Token {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            LParen => write!(f, "("),
            RParen => write!(f, ")"),
            LBrack => write!(f, "["),
            RBrack => write!(f, "]"),
            LCurly => write!(f, "{{"),
            RCurly => write!(f, "}}"),
            Comma => write!(f, ","),
            Semi => write!(f, ";"),
            Colon => write!(f, ":"),
            Dot => write!(f, "."),
            Dot2 => write!(f, ".."),
            Dot2Eq => write!(f, "..="),
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
            Num { body, .. } => write!(f, "{}", body),
            Ident { name } => write!(f, "{}", name),
            LineComment { body } => write!(f, "//{}", body),
            BlockComment { body } => write!(f, "/*{}*/", body),
            Whitespace { str, .. } => write!(f, "{}", str),
            Error(e) => write!(f, "{}", e),
        }
    }
}

impl Display for NumLit {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Dec(s) => write!(f, "{}", s),
            Bin(s) => write!(f, "0b{}", s),
            Hex(s) => write!(f, "0x{}", s),
        }
    }
}

impl Display for LexErr {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            EmptyBinNum { body } => write!(f, "0b{}", body),
            EmptyHexNum { body } => write!(f, "0x{}", body),
            StrayAmp { .. } => write!(f, "&"),
            StrayBar { .. } => write!(f, "|"),
            UnclosedBlockComment { body, .. } => write!(f, "/*{}", body),
            InvalidChar { c } => write!(f, "{}", c),
        }
    }
}

impl LexErr {
    /// Displays the error message.
    #[inline]
    pub fn msg(self) -> LexErrMsg {
        LexErrMsg(self)
    }
}

/// Displays the error message for LexErr.
pub struct LexErrMsg(LexErr);

impl Display for LexErrMsg {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match &self.0 {
            EmptyBinNum { body } => write!(f, "Binary number without digits 0b{}", body),
            EmptyHexNum { body } => write!(f, "Hexadecimal number without digits 0x{}", body),
            StrayAmp { next } => write!(f, "Stray & (before {}), && is expected", next),
            StrayBar { next } => write!(f, "Stray | (before {}), || is expected", next),
            UnclosedBlockComment { open_cnt, .. } => {
                write!(
                    f,
                    "Unclosed block comment (waiting for {} */{})",
                    open_cnt,
                    if *open_cnt > 1 { "s" } else { "" }
                )
            }
            InvalidChar { c } => write!(f, "Invalid character {}", c),
        }
    }
}

/// Lexer.
/// Works in linear time, with only one lookahead.
pub struct Lexer<I> {
    /// Current head character.
    head: OrEof<char>,
    /// Current position.
    pos: Pos,
    /// Input iterator.
    input: I,
}

/// Creates a lexer.
#[inline]
pub fn lexer<I: Iterator<Item = char>>(pos: Pos, input: I) -> Lexer<I> {
    Lexer::new(pos, input)
}

impl<I: Iterator<Item = char>> Lexer<I> {
    /// Creates a lexer.
    pub fn new(pos: Pos, mut input: I) -> Self {
        let head = input.next().into();
        Self { head, pos, input }
    }

    /// Gets the current position.
    pub fn pos(&self) -> Pos {
        self.pos
    }

    /// Moves the cursor one character ahead.
    /// Returns itself for convenience.
    #[inline]
    fn mov(&mut self) -> &mut Self {
        match self.head {
            Eof => panic!("Cannot perform mov when the head is EOF"),
            Just(c) => {
                self.pos = self.pos.after(c);
                self.head = self.input.next().into();
            }
        }
        self
    }

    /// Lexes the next token.
    pub fn lex(&mut self) -> OrEof<Token> {
        let head = match self.head {
            Eof => return Eof,
            Just(c) => c,
        };
        let tok = match head {
            // Determined at this point
            '(' => LParen,
            ')' => RParen,
            '[' => LBrack,
            ']' => RBrack,
            '{' => LCurly,
            '}' => RCurly,
            ',' => Comma,
            ';' => Semi,
            ':' => Colon,
            '*' => Ast,
            '+' => Plus,
            '-' => Minus,
            // Starting with `.`
            '.' => return self.mov().lex_dot(),
            // Starting with `=`
            '=' => return self.mov().lex_eq(),
            // Starting with `~`
            '~' => return self.mov().lex_tilde(),
            // Starting with `/`
            '/' => return self.mov().lex_slash(),
            // Starting with `&`
            '&' => return self.mov().lex_amp(),
            // Starting with `|`
            '|' => return self.mov().lex_bar(),
            // Starting with `<`
            '<' => return self.mov().lex_lt(),
            // Starting with `>`
            '>' => return self.mov().lex_gt(),
            // Starting with zero
            '0' => return self.mov().lex_0(),
            // Number, starting with a nonzero digit, `1`-`9`
            '1'..='9' => return self.lex_dec_num(false),
            // Name, starting with '_' or an alphabet character
            '_' => return self.lex_name(),
            _ if head.is_alphabetic() => return self.lex_name(),
            // Starting with a whitespace character
            _ if head.is_whitespace() => return self.lex_whitespace(),
            // Invalid character
            _ => Error(InvalidChar { c: head }),
        };
        self.mov();
        Just(tok)
    }

    /// Lexes the next token, starting with `.`.
    fn lex_dot(&mut self) -> OrEof<Token> {
        match self.head {
            Eof => Just(Dot),
            Just(head) => match head {
                // `..`
                '.' => {
                    self.mov();
                    match self.head {
                        Eof => Just(Dot2),
                        Just(head) => match head {
                            // `..=`
                            '=' => {
                                self.mov();
                                Just(Dot2Eq)
                            }
                            _ => Just(Dot2),
                        },
                    }
                }
                _ => Just(Dot),
            },
        }
    }

    /// Lexes the next token, starting with `=`.
    fn lex_eq(&mut self) -> OrEof<Token> {
        match self.head {
            Eof => Just(Eq),
            Just(head) => match head {
                // `==`
                '=' => {
                    self.mov();
                    Just(Eq2)
                }
                _ => Just(Eq),
            },
        }
    }

    /// Lexes, starting with `~`.
    fn lex_tilde(&mut self) -> OrEof<Token> {
        match self.head {
            Eof => Just(Not),
            Just(head) => match head {
                // `~=`
                '=' => {
                    self.mov();
                    Just(Neq)
                }
                _ => Just(Not),
            },
        }
    }

    /// Lexes the next token, starting with `/`.
    fn lex_slash(&mut self) -> OrEof<Token> {
        match self.head {
            Eof => Just(Div),
            Just(head) => match head {
                // `//`
                '/' => self.mov().lex_slash2(),
                // `/*`
                '*' => self.mov().lex_slash_ast(),
                _ => Just(Div),
            },
        }
    }

    /// Lexes the next token, starting with `//`.
    fn lex_slash2(&mut self) -> OrEof<Token> {
        let mut body = String::new();
        loop {
            match self.head {
                Eof => break,
                Just(head) => {
                    if head == '\n' {
                        break;
                    }
                    body.push(head);
                    self.mov();
                }
            }
        }
        Just(LineComment { body })
    }

    /// Lexes the next token, starting with `/*`.
    fn lex_slash_ast(&mut self) -> OrEof<Token> {
        let mut body = String::new();
        let mut open_cnt = 1usize;
        loop {
            match self.head {
                Eof => return Just(Error(UnclosedBlockComment { body, open_cnt })),
                Just(head) => {
                    body.push(head);
                    self.mov();
                    match head {
                        '/' => match self.head {
                            // `/*`
                            Just('*') => {
                                open_cnt += 1;
                                body.push('*');
                                self.mov();
                            }
                            _ => {}
                        },
                        '*' => match self.head {
                            // `*/`
                            Just('/') => {
                                open_cnt -= 1;
                                body.push('/');
                                self.mov();
                                if open_cnt == 0 {
                                    body.pop();
                                    body.pop();
                                    return Just(BlockComment { body });
                                }
                            }
                            _ => {}
                        },
                        _ => {}
                    }
                }
            }
        }
    }

    /// Lexes the next token, starting with `&`.
    fn lex_amp(&mut self) -> OrEof<Token> {
        match self.head {
            Eof => Just(Error(StrayAmp { next: Eof })),
            Just(head) => match head {
                // `&&`
                '&' => {
                    self.mov();
                    Just(And)
                }
                _ => Just(Error(StrayAmp { next: Just(head) })),
            },
        }
    }

    /// Lexes the next token, starting with `|`.
    fn lex_bar(&mut self) -> OrEof<Token> {
        match self.head {
            Eof => Just(Error(StrayBar { next: Eof })),
            Just(head) => match head {
                // `||`
                '|' => {
                    self.mov();
                    Just(Or)
                }
                _ => Just(Error(StrayBar { next: Just(head) })),
            },
        }
    }

    /// Lexes the next token, starting with `<`.
    fn lex_lt(&mut self) -> OrEof<Token> {
        match self.head {
            Eof => Just(Lt),
            Just(head) => match head {
                // `<=`
                '=' => {
                    self.mov();
                    Just(Leq)
                }
                _ => Just(Lt),
            },
        }
    }

    /// Lexes the next token, starting with `>`.
    fn lex_gt(&mut self) -> OrEof<Token> {
        match self.head {
            Eof => Just(Gt),
            Just(head) => match head {
                // `>=`
                '=' => {
                    self.mov();
                    Just(Geq)
                }
                _ => Just(Gt),
            },
        }
    }

    /// Lexes the next token, starting with zero.
    fn lex_0(&mut self) -> OrEof<Token> {
        match self.head {
            Eof => self.lex_dec_num(true),
            Just(head) => match head {
                // Binary number, starting with `0b`
                'b' => self.mov().lex_0b(),
                // Hexadecimal number, starting with `0x`
                'x' => self.mov().lex_0x(),
                // Decimal number, starting with `0` not followed by `b`/`x`
                _ => self.lex_dec_num(true),
            },
        }
    }

    /// Lexes the next number literal token, starting with `0b`.
    fn lex_0b(&mut self) -> OrEof<Token> {
        let mut body = String::new();
        let mut val = 0i64;
        let mut has_digit = false;
        loop {
            match self.head {
                Eof => break,
                Just(head) => match head {
                    // Separator
                    '_' => {
                        body.push(head);
                        self.mov();
                    }
                    // Digit
                    '0' | '1' => {
                        has_digit = true;
                        val = val * 2 + (head as u8 - '0' as u8) as i64;
                        body.push(head);
                        self.mov();
                    }
                    _ => break,
                },
            }
        }
        let body = body;
        Just(if !has_digit {
            Error(EmptyBinNum { body })
        } else {
            num(Bin(body), val)
        })
    }

    /// Lexes the next token, a hexadecimal number starting with `0x`.
    fn lex_0x(&mut self) -> OrEof<Token> {
        let mut body = String::new();
        let mut val = 0i64;
        let mut has_digit = false;
        loop {
            match self.head {
                Eof => break,
                Just(head) => match head {
                    // Separator
                    '_' => {
                        body.push(head);
                        self.mov();
                    }
                    // Digit
                    '0'..='9' | 'a'..='f' | 'A'..='F' => {
                        has_digit = true;
                        val = val * 16 + head.to_digit(16).unwrap() as i64;
                        body.push(head);
                        self.mov();
                    }
                    _ => break,
                },
            }
        }
        let body = body;
        Just(if !has_digit {
            Error(EmptyHexNum { body })
        } else {
            num(Hex(body), val)
        })
    }

    /// Lexes the next decimal number token.
    fn lex_dec_num(&mut self, head_0: bool) -> OrEof<Token> {
        let mut body = String::new();
        if head_0 {
            body.push('0');
        }
        let mut val = 0i64;
        loop {
            match self.head {
                Eof => break,
                Just(head) => match head {
                    '_' => {
                        body.push(head);
                        self.mov();
                    }
                    '0'..='9' => {
                        body.push(head);
                        val = val * 10 + (head as u8 - '0' as u8) as i64;
                        self.mov();
                    }
                    _ => break,
                },
            }
        }
        Just(num(Dec(body), val))
    }

    /// Lexes a name.
    fn lex_name(&mut self) -> OrEof<Token> {
        let mut name = String::new();
        loop {
            match self.head {
                Eof => break,
                Just(head) => match head {
                    // Continues for `_`, `0`-`9`, or an alphabet character.
                    '_' | '0'..='9' => {
                        name.push(head);
                        self.mov();
                    }
                    _ if head.is_alphabetic() => {
                        name.push(head);
                        self.mov();
                    }
                    _ => break,
                },
            };
        }
        Just(match name.as_str() {
            "fn" => Fn,
            "let" => Let,
            _ => Ident { name },
        })
    }

    /// Lexes the next token, starting with a whitespace character.
    fn lex_whitespace(&mut self) -> OrEof<Token> {
        let mut str = String::new();
        let mut newline_cnt = 0;
        loop {
            match self.head {
                Eof => break,
                Just(head) => {
                    if head.is_whitespace() {
                        // Continues for a whitespace character.
                        str.push(head);
                        self.mov();
                        if head == '\n' {
                            newline_cnt += 1;
                        }
                    } else {
                        break;
                    }
                }
            }
        }
        Just(Whitespace {
            str: str,
            nl_cnt: newline_cnt,
        })
    }
}

impl<I: Iterator<Item = char>> Iterator for Lexer<I> {
    type Item = (Token, Span);
    fn next(&mut self) -> Option<Self::Item> {
        let from = self.pos;
        match self.lex() {
            Eof => None,
            Just(tok) => {
                let to = self.pos;
                Some((tok, span(from, to)))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{span, util::pos::pos};
    use std::fmt::Write;

    /// Big text containing all kinds of tokens.
    const BIG: &str = r"() [] {} // xxx
, ; : . .. ..= /*a/*b*/c*/
= == ~= *
&& || ~
< <= > >=
+ - /
fn let
123 0b101 0xa0f
abcde
0b__ 0x__
& | ♡ /*a/*b";

    /// Tests that lexing and displaying code retrieves the original code.
    #[test]
    fn test_next_display() {
        let mut buf = String::new();
        for (tok, _) in lexer(pos(0, 0), BIG.chars()) {
            let _ = write!(buf, "{}", tok);
        }
        assert_eq!(BIG, buf);
    }

    /// Tests lexing `s` against `get_want` parametrized over the context.
    fn test_lex(s: &str, want: Vec<(Token, Span)>) {
        let res: Vec<_> = lexer(pos(0, 0), s.chars())
            .filter(|tok| match tok.0 {
                Whitespace { .. } => false,
                _ => true,
            })
            .collect();
        for (ts, want_ts) in res.iter().zip(want.iter()) {
            assert_eq!(ts, want_ts);
        }
        assert!(res.len() == want.len());
    }

    /// Tests lexing all kinds of tokens.
    #[test]
    fn test_next_all() {
        test_lex(
            BIG,
            vec![
                (LParen, span!((0, 0)..(0, 1))),
                (RParen, span!((0, 1)..(0, 2))),
                (LBrack, span!((0, 3)..(0, 4))),
                (RBrack, span!((0, 4)..(0, 5))),
                (LCurly, span!((0, 6)..(0, 7))),
                (RCurly, span!((0, 7)..(0, 8))),
                (
                    LineComment {
                        body: " xxx".into(),
                    },
                    span!((0, 9)..(0, 15)),
                ),
                (Comma, span!((1, 0)..(1, 1))),
                (Semi, span!((1, 2)..(1, 3))),
                (Colon, span!((1, 4)..(1, 5))),
                (Dot, span!((1, 6)..(1, 7))),
                (Dot2, span!((1, 8)..(1, 10))),
                (Dot2Eq, span!((1, 11)..(1, 14))),
                (
                    BlockComment {
                        body: "a/*b*/c".into(),
                    },
                    span!((1, 15)..(1, 26)),
                ),
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
                (num(Dec("123".into()), 123), span!((7, 0)..(7, 3))),
                (num(Bin("101".into()), 0b101), span!((7, 4)..(7, 9))),
                (num(Hex("a0f".into()), 0xa0f), span!((7, 10)..(7, 15))),
                (
                    Ident {
                        name: "abcde".into(),
                    },
                    span!((8, 0)..(8, 5)),
                ),
                (
                    Error(EmptyBinNum { body: "__".into() }),
                    span!((9, 0)..(9, 4)),
                ),
                (
                    Error(EmptyHexNum { body: "__".into() }),
                    span!((9, 5)..(9, 9)),
                ),
                (Error(StrayAmp { next: Just(' ') }), span!((10, 0)..(10, 1))),
                (Error(StrayBar { next: Just(' ') }), span!((10, 2)..(10, 3))),
                (Error(InvalidChar { c: '♡' }), span!((10, 4)..(10, 5))),
                (
                    Error(UnclosedBlockComment {
                        body: "a/*b".into(),
                        open_cnt: 2,
                    }),
                    span!((10, 6)..(10, 12)),
                ),
            ],
        );
    }

    /// Tests lexing identifiers.
    #[test]
    fn test_next_ident() {
        test_lex(
            "ab1_cde 漢字",
            vec![
                (
                    Ident {
                        name: "ab1_cde".into(),
                    },
                    span!((0, 0)..(0, 7)),
                ),
                (
                    Ident {
                        name: "漢字".into(),
                    },
                    span!((0, 8)..(0, 10)),
                ),
            ],
        )
    }

    /// Tests lexing numbers.
    #[test]
    fn test_next_number() {
        test_lex(
            r"0 0_123 1_234_567_890
0b0101_1010
0xab_01_EF",
            vec![
                (num(Dec("0".into()), 0), span!((0, 0)..(0, 1))),
                (num(Dec("0_123".into()), 123), span!((0, 2)..(0, 7))),
                (
                    num(Dec("1_234_567_890".into()), 1_234_567_890),
                    span!((0, 8)..(0, 21)),
                ),
                (
                    num(Bin("0101_1010".into()), 0b0101_1010),
                    span!((1, 0)..(1, 11)),
                ),
                (
                    num(Hex("ab_01_EF".into()), 0xab_01_EF),
                    span!((2, 0)..(2, 10)),
                ),
            ],
        )
    }
}
