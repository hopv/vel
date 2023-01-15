use crate::util::arena::{AString, AStringExt, Arena, ArenaExt};
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
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Just(v) => write!(f, "{}", v),
            Eof => write!(f, "EOF"),
        }
    }
}

/// Vel token.
///
/// Has the information to retrieve the original source code.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum Token<'a> {
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
    /// Number literal.
    Num { body: NumLit<'a>, val: i64 },
    /// Identifier.
    Ident { name: &'a str },
    /// Doc line comment, `///...`.
    DocComment { body: &'a str },
    /// Line comment, `//...`.
    Comment { body: &'a str },
    /// Whitespace.
    Whitespace { str: &'a str },
    /// Error token.
    Error(LexErr<'a>),
}
pub use Token::*;

/// Number literal.
#[inline]
pub fn num<'a>(body: NumLit<'a>, val: i64) -> Token<'a> {
    Num { body, val }
}

/// Number literal.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum NumLit<'a> {
    /// Decimal.
    Dec(&'a str),
    /// Binary.
    Bin(&'a str),
    /// Hexadecimal.
    Hex(&'a str),
}
pub use NumLit::*;

/// Vel lexing error.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum LexErr<'a> {
    /// Empty binary number.
    EmptyBinNum { body: &'a str },
    /// Empty hexadecimal number.
    EmptyHexNum { body: &'a str },
    /// Stray `&`.
    StrayAmp { next: OrEof<char> },
    /// Stray `|`.
    StrayBar { next: OrEof<char> },
    /// Invalid character.
    InvalidChar { c: char },
}
pub use LexErr::*;

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
            Num { body, .. } => write!(f, "{}", body),
            Ident { name } => write!(f, "{}", name),
            Comment { body } => write!(f, "//{}", body),
            DocComment { body } => write!(f, "///{}", body),
            Whitespace { str } => write!(f, "{}", str),
            Error(e) => write!(f, "{}", e),
        }
    }
}

impl Display for NumLit<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match *self {
            Dec(s) => write!(f, "{}", s),
            Bin(s) => write!(f, "0b{}", s),
            Hex(s) => write!(f, "0x{}", s),
        }
    }
}

impl Display for LexErr<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match *self {
            EmptyBinNum { body } => write!(f, "0b{}", body),
            EmptyHexNum { body } => write!(f, "0x{}", body),
            StrayAmp { .. } => write!(f, "&"),
            StrayBar { .. } => write!(f, "|"),
            InvalidChar { c } => write!(f, "{}", c),
        }
    }
}

impl<'a> LexErr<'a> {
    /// Displays the error message.
    #[inline]
    pub fn msg(self) -> LexErrMsg<'a> {
        LexErrMsg(self)
    }
}

/// Displays the error message for LexErr.
pub struct LexErrMsg<'a>(LexErr<'a>);

impl Display for LexErrMsg<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self.0 {
            EmptyBinNum { body } => write!(f, "Binary number without digits 0b{}", body),
            EmptyHexNum { body } => write!(f, "Hexadecimal number without digits 0x{}", body),
            StrayAmp { next } => write!(f, "Stray & (before {}), && is expected", next),
            StrayBar { next } => write!(f, "Stray | (before {}), || is expected", next),
            InvalidChar { c } => write!(f, "Invalid character {}", c),
        }
    }
}

/// Lexer.
/// Works in linear time, with only one lookahead.
pub struct Lexer<'arn, I> {
    /// Current head character.
    head: OrEof<char>,
    /// Current position.
    pos: Pos,
    /// Input iterator.
    input: I,
    /// Arena.
    arn: &'arn Arena,
}

/// Creates a lexer.
#[inline]
pub fn lexer<'arn, I: Iterator<Item = char>>(
    pos: Pos,
    input: I,
    arn: &'arn Arena,
) -> Lexer<'arn, I> {
    Lexer::new(pos, input, arn)
}

impl<'arn, I: Iterator<Item = char>> Lexer<'arn, I> {
    /// Creates a lexer.
    pub fn new(pos: Pos, mut input: I, arn: &'arn Arena) -> Self {
        let head = input.next().into();
        Self {
            head,
            pos,
            input,
            arn,
        }
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

    /// Creates a new `AString`.
    fn new_astring(&self) -> AString<'arn> {
        self.arn.new_astring()
    }

    /// Lexes the next token.
    pub fn lex(&mut self) -> OrEof<Token<'arn>> {
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
            '*' => Ast,
            '+' => Plus,
            '-' => Minus,
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

    /// Lexes the next token, starting with `=`.
    fn lex_eq(&mut self) -> OrEof<Token<'arn>> {
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
    fn lex_tilde(&mut self) -> OrEof<Token<'arn>> {
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
    fn lex_slash(&mut self) -> OrEof<Token<'arn>> {
        match self.head {
            Eof => Just(Div),
            Just(head) => match head {
                // `//`
                '/' => self.mov().lex_slash2(),
                _ => Just(Div),
            },
        }
    }

    /// Lexes the next token, starting with `//`.
    fn lex_slash2(&mut self) -> OrEof<Token<'arn>> {
        let is_doc = match self.head {
            // `///`, a doc line comment
            Just('/') => {
                self.mov();
                true
            }
            _ => false,
        };
        let mut body = self.new_astring();
        loop {
            match self.head {
                Eof => break,
                Just(head) => {
                    body.push(head);
                    self.mov();
                    if head == '\n' {
                        break;
                    }
                }
            }
        }
        let body = body.into_str();
        Just(if is_doc {
            DocComment { body }
        } else {
            Comment { body }
        })
    }

    /// Lexes the next token, starting with `&`.
    fn lex_amp(&mut self) -> OrEof<Token<'arn>> {
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
    fn lex_bar(&mut self) -> OrEof<Token<'arn>> {
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
    fn lex_lt(&mut self) -> OrEof<Token<'arn>> {
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
    fn lex_gt(&mut self) -> OrEof<Token<'arn>> {
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
    fn lex_0(&mut self) -> OrEof<Token<'arn>> {
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
    fn lex_0b(&mut self) -> OrEof<Token<'arn>> {
        let mut body = self.new_astring();
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
        let body = body.into_str();
        Just(if !has_digit {
            Error(EmptyBinNum { body })
        } else {
            num(Bin(body), val)
        })
    }

    /// Lexes the next token, a hexadecimal number starting with `0x`.
    fn lex_0x(&mut self) -> OrEof<Token<'arn>> {
        let mut body = self.new_astring();
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
        let body = body.into_str();
        Just(if !has_digit {
            Error(EmptyHexNum { body })
        } else {
            num(Hex(body), val)
        })
    }

    /// Lexes the next decimal number token.
    fn lex_dec_num(&mut self, head_0: bool) -> OrEof<Token<'arn>> {
        let mut body = self.new_astring();
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
        Just(num(Dec(body.into_str()), val))
    }

    /// Lexes a name.
    fn lex_name(&mut self) -> OrEof<Token<'arn>> {
        let mut name = self.new_astring();
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
        Just(match name.into_str() {
            "fn" => Fn,
            "let" => Let,
            name => Ident { name },
        })
    }

    /// Lexes the next token, starting with a whitespace character.
    fn lex_whitespace(&mut self) -> OrEof<Token<'arn>> {
        let mut str = self.new_astring();
        loop {
            match self.head {
                Eof => break,
                Just(head) => {
                    if head.is_whitespace() {
                        // Continues for a whitespace character.
                        str.push(head);
                        self.mov();
                    } else {
                        break;
                    }
                }
            }
        }
        Just(Whitespace {
            str: str.into_str(),
        })
    }
}

impl<'arn, I: Iterator<Item = char>> Iterator for Lexer<'arn, I> {
    type Item = (Token<'arn>, Span);
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
    const BIG: &str = r"() [] {} /// xxx
= == ~= * // yy
&& || ~
< <= > >=
+ - /
fn let
123 0b101 0xa0f
abcde
0b__ 0x__
& | ♡";

    /// Tests that lexing and displaying code retrieves the original code.
    #[test]
    fn test_next_display() {
        let arn = Arena::new();
        let mut buf = arn.new_astring();
        for (tok, _) in lexer(pos(0, 0), BIG.chars(), &arn) {
            let _ = write!(buf, "{}", tok);
        }
        assert_eq!(BIG, buf);
    }

    /// Tests lexing `s` against `get_want` parametrized over the context.
    fn test_lex(s: &str, want: Vec<(Token, Span)>) {
        let arn = Arena::new();
        let res: Vec<_> = lexer(pos(0, 0), s.chars(), &arn)
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
                (DocComment { body: " xxx\n" }, span!((0, 9)..(1, 0))),
                (Eq, span!((1, 0)..(1, 1))),
                (Eq2, span!((1, 2)..(1, 4))),
                (Neq, span!((1, 5)..(1, 7))),
                (Ast, span!((1, 8)..(1, 9))),
                (Comment { body: " yy\n" }, span!((1, 10)..(2, 0))),
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
                (num(Dec("123"), 123), span!((6, 0)..(6, 3))),
                (num(Bin("101"), 0b101), span!((6, 4)..(6, 9))),
                (num(Hex("a0f"), 0xa0f), span!((6, 10)..(6, 15))),
                (Ident { name: "abcde" }, span!((7, 0)..(7, 5))),
                (Error(EmptyBinNum { body: "__" }), span!((8, 0)..(8, 4))),
                (Error(EmptyHexNum { body: "__" }), span!((8, 5)..(8, 9))),
                (Error(StrayAmp { next: Just(' ') }), span!((9, 0)..(9, 1))),
                (Error(StrayBar { next: Just(' ') }), span!((9, 2)..(9, 3))),
                (Error(InvalidChar { c: '♡' }), span!((9, 4)..(9, 5))),
            ],
        );
    }

    /// Tests lexing identifiers.
    #[test]
    fn test_next_ident() {
        test_lex(
            "ab1_cde 漢字",
            vec![
                (Ident { name: "ab1_cde" }, span!((0, 0)..(0, 7))),
                (Ident { name: "漢字" }, span!((0, 8)..(0, 10))),
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
                (num(Dec("0"), 0), span!((0, 0)..(0, 1))),
                (num(Dec("0_123"), 123), span!((0, 2)..(0, 7))),
                (
                    num(Dec("1_234_567_890"), 1_234_567_890),
                    span!((0, 8)..(0, 21)),
                ),
                (num(Bin("0101_1010"), 0b0101_1010), span!((1, 0)..(1, 11))),
                (num(Hex("ab_01_EF"), 0xab_01_EF), span!((2, 0)..(2, 10))),
            ],
        )
    }
}
