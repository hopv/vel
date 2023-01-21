//! Vel lexer.

use crate::util::linecol::{LineCol, Span};
use std::fmt::{Display, Formatter, Result};

/// Something or EOF.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum OrEof<T> {
    /// Has a content.
    Just(T),
    /// EOF, or the end of the input stream.
    Eof,
}
pub use OrEof::*;

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
    /// `?`.
    Quest,
    /// `!`.
    Bang,
    /// `.`.
    Dot,
    /// `..`.
    Dot2,
    /// `..=`.
    Dot2Eq,
    /// `-`.
    Dash,
    /// `->`.
    DashGt,
    /// `~`.
    Tilde,
    /// `=`.
    Eq,
    /// `==`.
    Eq2,
    /// `+`.
    Plus,
    /// `*`.
    Star,
    /// `#`.
    Hash,
    /// `<`.
    Lt,
    /// `<=`.
    LtEq,
    /// `<>`.
    LtGt,
    /// `>`.
    Gt,
    /// `>=`.
    GtEq,
    /// `&`.
    Amp,
    /// `&&`.
    Amp2,
    /// `|`.
    Bar,
    /// `||`.
    Bar2,
    /// `/`.
    Slash,
    /// `^`.
    Hat,
    /// `@`.
    At,
    /// `%`.
    Percent,
    /// `$`.
    Dollar,
    /// `fn`.
    Fn,
    /// `let`.
    Let,
    /// `if`.
    If,
    /// `else`.
    Else,
    /// `loop`.
    Loop,
    /// `while`.
    While,
    /// `break`.
    Break,
    /// `continue`.
    Continue,
    /// `return`.
    Return,
    /// `true`.
    True,
    /// `false`.
    False,
    /// Integer numeric literal.
    Num {
        /// Kind.
        kind: NumKind,
        /// Digits.
        digits: Box<str>,
        /// Suffix.
        suffix: Box<str>,
        /// Pre-calculated value.
        val: i128,
    },
    /// Identifier.
    Ident { name: Box<str> },
    /// Line comment, `//...`.
    LineComment { body: Box<str> },
    /// Block comment, `/* ... */` (nestable).
    BlockComment { body: Box<str> },
    /// Whitespace.
    Whitespace {
        str: Box<str>,
        /// The number of newlines `\n`.
        newline_cnt: usize,
    },
    /// Error token.
    Error(LexErr),
}
pub use Token::*;

/// Vel lexing error.
#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum LexErr {
    /// Integer numeric literal with empty digits.
    EmptyNum {
        /// Kind. Can't be Dec.
        kind: NumKind,
        /// Digits.
        digits: Box<str>,
        /// Suffix.
        suffix: Box<str>,
    },
    /// Unclosed block comment.
    UnclosedBlockComment { body: Box<str>, open_cnt: usize },
    /// Invalid character.
    InvalidChar { c: char },
}
pub use LexErr::*;

/// Integer numeric literal kind.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum NumKind {
    /// Decimal.
    Dec,
    /// Binary.
    Bin,
    /// Hexadecimal.
    Hex,
}
pub use NumKind::*;

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
            Quest => write!(f, "?"),
            Bang => write!(f, "!"),
            Dot => write!(f, "."),
            Dot2 => write!(f, ".."),
            Dot2Eq => write!(f, "..="),
            Dash => write!(f, "-"),
            DashGt => write!(f, "->"),
            Tilde => write!(f, "~"),
            Eq => write!(f, "="),
            Eq2 => write!(f, "=="),
            Plus => write!(f, "+"),
            Star => write!(f, "*"),
            Hash => write!(f, "#"),
            Lt => write!(f, "<"),
            LtEq => write!(f, "<="),
            LtGt => write!(f, "<>"),
            Gt => write!(f, ">"),
            GtEq => write!(f, ">="),
            Amp => write!(f, "&"),
            Amp2 => write!(f, "&&"),
            Bar => write!(f, "|"),
            Bar2 => write!(f, "||"),
            Slash => write!(f, "/"),
            Hat => write!(f, "^"),
            At => write!(f, "@"),
            Percent => write!(f, "%"),
            Dollar => write!(f, "$"),
            Fn => write!(f, "fn"),
            Let => write!(f, "let"),
            If => write!(f, "if"),
            Else => write!(f, "else"),
            Loop => write!(f, "loop"),
            While => write!(f, "while"),
            Break => write!(f, "break"),
            Continue => write!(f, "continue"),
            Return => write!(f, "return"),
            True => write!(f, "true"),
            False => write!(f, "false"),
            Num {
                kind,
                digits,
                suffix,
                ..
            } => write!(f, "{}{}{}", kind, digits, suffix),
            Ident { name } => write!(f, "{}", name),
            LineComment { body } => write!(f, "//{}", body),
            BlockComment { body } => write!(f, "/*{}*/", body),
            Whitespace { str, .. } => write!(f, "{}", str),
            Error(e) => write!(f, "{}", e),
        }
    }
}

impl Display for LexErr {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            EmptyNum {
                kind,
                digits,
                suffix,
                ..
            } => match kind {
                Dec => unreachable!("EmptyNum with Dec kind"),
                Bin => write!(f, "0b{}{}", digits, suffix),
                Hex => write!(f, "0x{}{}", digits, suffix),
            },
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
            err @ EmptyNum { kind, .. } => write!(
                f,
                "{} number without digits {}",
                match kind {
                    Dec => unreachable!("EmptyNum with Dec kind"),
                    Bin => "Binary",
                    Hex => "Hexadecimal",
                },
                err
            ),
            UnclosedBlockComment { open_cnt, .. } => {
                write!(
                    f,
                    "Unclosed block comment until EOF (waiting for {} occurrence{} of */)",
                    open_cnt,
                    if *open_cnt > 1 { "s" } else { "" }
                )
            }
            InvalidChar { c } => write!(f, "Invalid character {}", c),
        }
    }
}

impl Display for NumKind {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Dec => write!(f, ""),
            Bin => write!(f, "0b"),
            Hex => write!(f, "0x"),
        }
    }
}

impl NumKind {
    fn radix(self: NumKind) -> u32 {
        match self {
            Dec => 10,
            Bin => 2,
            Hex => 16,
        }
    }
}

/// Lexer.
/// Works in linear time, with only one lookahead.
pub struct Lexer<I> {
    /// Current head character.
    head: OrEof<char>,
    /// Current position.
    lc: LineCol,
    /// Input iterator.
    input: I,
}

/// Creates a lexer.
#[inline]
pub fn lexer<I: Iterator<Item = char>>(lc: LineCol, input: I) -> Lexer<I> {
    Lexer::new(lc, input)
}

impl<I: Iterator<Item = char>> Lexer<I> {
    /// Creates a lexer.
    pub fn new(lc: LineCol, mut input: I) -> Self {
        let head = input.next().into();
        Self { head, lc, input }
    }

    /// Gets the current position.
    pub fn lc(&self) -> LineCol {
        self.lc
    }

    /// Moves the cursor one character ahead.
    #[inline]
    fn mov(&mut self) {
        match self.head {
            Eof => panic!("Cannot perform mov when the head is EOF"),
            Just(c) => {
                self.lc = self.lc.after(c);
                self.head = self.input.next().into();
            }
        }
    }

    /// Moves and returns itself for convenience.
    #[inline]
    fn mov_and(&mut self) -> &mut Self {
        self.mov();
        self
    }

    /// Moves and returns the head for convenience.
    #[inline]
    fn mov_head(&mut self) -> OrEof<char> {
        self.mov();
        self.head
    }

    /// Moves and returns the input token for convenience.
    #[inline]
    fn mov_just(&mut self, tok: Token) -> OrEof<Token> {
        self.mov();
        Just(tok)
    }

    /// Lexes the next token.
    pub fn lex(&mut self) -> OrEof<Token> {
        let head = match self.head {
            Eof => return Eof,
            Just(c) => c,
        };
        match head {
            // `(`
            '(' => self.mov_just(LParen),
            // `)`
            ')' => self.mov_just(RParen),
            // `[`
            '[' => self.mov_just(LBrack),
            // `]`
            ']' => self.mov_just(RBrack),
            // `{`
            '{' => self.mov_just(LCurly),
            // `}`
            '}' => self.mov_just(RCurly),
            // `,`
            ',' => self.mov_just(Comma),
            // `;`
            ';' => self.mov_just(Semi),
            // `:`
            ':' => self.mov_just(Colon),
            // `.`
            '.' => match self.mov_head() {
                // `..`
                Just('.') => match self.mov_head() {
                    // `..=`
                    Just('=') => self.mov_just(Dot2Eq),
                    _ => Just(Dot2),
                },
                _ => Just(Dot),
            },
            // `?`
            '?' => self.mov_just(Quest),
            // `!`
            '!' => self.mov_just(Bang),
            // `-`
            '-' => match self.mov_head() {
                // `->`
                Just('>') => self.mov_just(DashGt),
                _ => Just(Dash),
            },
            // `~`
            '~' => self.mov_just(Tilde),
            // `+`
            '+' => self.mov_just(Plus),
            // `=`
            '=' => match self.mov_head() {
                // `==`
                Just('=') => self.mov_just(Eq2),
                _ => Just(Eq),
            },
            // `*`
            '*' => self.mov_just(Star),
            // `#`
            '#' => self.mov_just(Hash),
            // `<`
            '<' => match self.mov_head() {
                // `<=`
                Just('=') => self.mov_just(LtEq),
                // `<>`
                Just('>') => self.mov_just(LtGt),
                _ => Just(Lt),
            },
            // `>`
            '>' => match self.mov_head() {
                // `>=`
                Just('=') => self.mov_just(GtEq),
                _ => Just(Gt),
            },
            // `&`
            '&' => match self.mov_head() {
                // `&&`
                Just('&') => self.mov_just(Amp2),
                _ => Just(Amp),
            },
            // `|`
            '|' => match self.mov_head() {
                // `||`
                Just('|') => self.mov_just(Bar2),
                _ => Just(Bar),
            },
            // `/`
            '/' => match self.mov_head() {
                // `//`
                Just('/') => self.mov_and().lex_slash2(),
                // `/*`
                Just('*') => self.mov_and().lex_slash_ast(),
                _ => Just(Slash),
            },
            // `^`
            '^' => self.mov_just(Hat),
            // `@`
            '@' => self.mov_just(At),
            // `%`
            '%' => self.mov_just(Percent),
            // `$`
            '$' => self.mov_just(Dollar),
            // `0`
            '0' => return self.mov_and().lex_0(),
            // Number, starting with a nonzero digit, `1`-`9`
            '1'..='9' => return self.lex_num(Dec, false),
            // Name, starting with '_' or an alphabetic character
            _ if head == '_' || head.is_alphabetic() => return self.lex_name(),
            // Starting with a whitespace character
            _ if head.is_whitespace() => return self.lex_whitespace(),
            // Invalid character
            _ => self.mov_just(Error(InvalidChar { c: head })),
        }
    }

    /// Lexes the next token, starting with `//`.
    fn lex_slash2(&mut self) -> OrEof<Token> {
        let mut body = String::new();
        loop {
            match self.head {
                Eof | Just('\n') => break,
                Just(head) => {
                    body.push(head);
                    self.mov();
                }
            }
        }
        Just(LineComment { body: body.into() })
    }

    /// Lexes the next token, starting with `/*`.
    fn lex_slash_ast(&mut self) -> OrEof<Token> {
        let mut body = String::new();
        let mut open_cnt = 1usize;
        loop {
            match self.head {
                Eof => {
                    return Just(Error(UnclosedBlockComment {
                        body: body.into(),
                        open_cnt,
                    }))
                }
                Just(head) => {
                    body.push(head);
                    self.mov();
                    match (head, self.head) {
                        // `/*`
                        ('/', Just('*')) => {
                            open_cnt += 1;
                            body.push('*');
                            self.mov();
                        }
                        // `*/`
                        ('*', Just('/')) => {
                            open_cnt -= 1;
                            self.mov();
                            if open_cnt == 0 {
                                body.pop();
                                return Just(BlockComment { body: body.into() });
                            }
                            body.push('/');
                        }
                        _ => {}
                    }
                }
            }
        }
    }

    /// Lexes the next token, starting with `0`.
    fn lex_0(&mut self) -> OrEof<Token> {
        match self.head {
            // Binary number, starting with `0b`
            Just('b') => self.mov_and().lex_num(Bin, false),
            // Hexadecimal number, starting with `0x`
            Just('x') => self.mov_and().lex_num(Hex, false),
            // Decimal number, starting with `0` not followed by `b`/`x`
            _ => self.lex_num(Dec, true),
        }
    }

    /// Lexes the next number token of the radix.
    ///
    /// - `head_0`: Whether the number has a leading `0`.
    fn lex_num(&mut self, kind: NumKind, head_0: bool) -> OrEof<Token> {
        let mut has_digit = false;
        // Parse digits and calculate the value.
        let mut digits = String::new();
        if head_0 {
            digits.push('0');
            has_digit = true;
        }
        let mut val = 0i128;
        let radix = kind.radix();
        loop {
            match self.head {
                Just('_') => {
                    digits.push('_');
                    self.mov();
                }
                Just(head) if head.is_digit(radix) => {
                    has_digit = true;
                    val = val * (radix as i128) + head.to_digit(radix).unwrap() as i128;
                    digits.push(head);
                    self.mov();
                }
                _ => break,
            }
        }
        let digits = digits.into();

        // Parse suffix.
        let mut suffix = String::new();
        loop {
            match self.head {
                // Continues for `_`, `0`-`9`, or an alphabetic character.
                Just(head) if head == '_' || head.is_ascii_digit() || head.is_alphabetic() => {
                    suffix.push(head);
                    self.mov();
                }
                _ => break,
            };
        }
        let suffix = suffix.into();

        Just(if has_digit {
            Num {
                kind,
                digits,
                suffix,
                val,
            }
        } else {
            Error(EmptyNum {
                kind,
                digits,
                suffix,
            })
        })
    }

    /// Lexes a name.
    fn lex_name(&mut self) -> OrEof<Token> {
        let mut name = String::new();
        loop {
            match self.head {
                // Continues for `_`, `0`-`9`, or an alphabetic character.
                Just(head) if head == '_' || head.is_ascii_digit() || head.is_alphabetic() => {
                    name.push(head);
                    self.mov();
                }
                _ => break,
            };
        }
        Just(match name.as_str() {
            "fn" => Fn,
            "let" => Let,
            "if" => If,
            "else" => Else,
            "loop" => Loop,
            "while" => While,
            "break" => Break,
            "continue" => Continue,
            "return" => Return,
            "true" => True,
            "false" => False,
            _ => Ident { name: name.into() },
        })
    }

    /// Lexes the next token, starting with a whitespace character.
    fn lex_whitespace(&mut self) -> OrEof<Token> {
        let mut str = String::new();
        let mut newline_cnt = 0;
        loop {
            match self.head {
                // Continues for a whitespace character.
                Just(head) if head.is_whitespace() => {
                    str.push(head);
                    self.mov();
                    if head == '\n' {
                        newline_cnt += 1;
                    }
                }
                _ => break,
            }
        }
        Just(Whitespace {
            str: str.into(),
            newline_cnt,
        })
    }
}

impl<I: Iterator<Item = char>> Iterator for Lexer<I> {
    type Item = (Token, Span);
    fn next(&mut self) -> Option<Self::Item> {
        let from = self.lc;
        match self.lex() {
            Eof => None,
            Just(tok) => {
                let to = self.lc;
                Some((tok, from..to))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::util::linecol::linecol;
    use std::fmt::Write;

    /// Big text containing all kinds of tokens.
    const BIG: &str = r"/* a /* b */ c */ // xxx
() [] {}
, ; : . .. ..= ? !
- -> ~ = == + * #
< <= <> > >=
& && | || / ^ @ % $
fn let
if else loop while break continue return
true false 123i32
abcde
0b__xxx ♡ /*a/*b";

    /// Tests that lexing and displaying code retrieves the original code.
    #[test]
    fn test_next_display() {
        let mut buf = String::new();
        for (tok, _) in lexer(linecol(0, 0), BIG.chars()) {
            let _ = write!(buf, "{}", tok);
        }
        assert_eq!(BIG, buf);
    }

    /// Tests lexing `s` against `get_want` parametrized over the context.
    fn test_lex(s: &str, want: Vec<(Token, Span)>) {
        let res: Vec<_> = lexer(linecol(0, 0), s.chars())
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
                (
                    BlockComment {
                        body: " a /* b */ c ".into(),
                    },
                    linecol(0, 0)..linecol(0, 17),
                ),
                (
                    LineComment {
                        body: " xxx".into(),
                    },
                    linecol(0, 18)..linecol(0, 24),
                ),
                (LParen, linecol(1, 0)..linecol(1, 1)),
                (RParen, linecol(1, 1)..linecol(1, 2)),
                (LBrack, linecol(1, 3)..linecol(1, 4)),
                (RBrack, linecol(1, 4)..linecol(1, 5)),
                (LCurly, linecol(1, 6)..linecol(1, 7)),
                (RCurly, linecol(1, 7)..linecol(1, 8)),
                (Comma, linecol(2, 0)..linecol(2, 1)),
                (Semi, linecol(2, 2)..linecol(2, 3)),
                (Colon, linecol(2, 4)..linecol(2, 5)),
                (Dot, linecol(2, 6)..linecol(2, 7)),
                (Dot2, linecol(2, 8)..linecol(2, 10)),
                (Dot2Eq, linecol(2, 11)..linecol(2, 14)),
                (Quest, linecol(2, 15)..linecol(2, 16)),
                (Bang, linecol(2, 17)..linecol(2, 18)),
                (Dash, linecol(3, 0)..linecol(3, 1)),
                (DashGt, linecol(3, 2)..linecol(3, 4)),
                (Tilde, linecol(3, 5)..linecol(3, 6)),
                (Eq, linecol(3, 7)..linecol(3, 8)),
                (Eq2, linecol(3, 9)..linecol(3, 11)),
                (Plus, linecol(3, 12)..linecol(3, 13)),
                (Star, linecol(3, 14)..linecol(3, 15)),
                (Hash, linecol(3, 16)..linecol(3, 17)),
                (Lt, linecol(4, 0)..linecol(4, 1)),
                (LtEq, linecol(4, 2)..linecol(4, 4)),
                (LtGt, linecol(4, 5)..linecol(4, 7)),
                (Gt, linecol(4, 8)..linecol(4, 9)),
                (GtEq, linecol(4, 10)..linecol(4, 12)),
                (Amp, linecol(5, 0)..linecol(5, 1)),
                (Amp2, linecol(5, 2)..linecol(5, 4)),
                (Bar, linecol(5, 5)..linecol(5, 6)),
                (Bar2, linecol(5, 7)..linecol(5, 9)),
                (Slash, linecol(5, 10)..linecol(5, 11)),
                (Hat, linecol(5, 12)..linecol(5, 13)),
                (At, linecol(5, 14)..linecol(5, 15)),
                (Percent, linecol(5, 16)..linecol(5, 17)),
                (Dollar, linecol(5, 18)..linecol(5, 19)),
                (Fn, linecol(6, 0)..linecol(6, 2)),
                (Let, linecol(6, 3)..linecol(6, 6)),
                (If, linecol(7, 0)..linecol(7, 2)),
                (Else, linecol(7, 3)..linecol(7, 7)),
                (Loop, linecol(7, 8)..linecol(7, 12)),
                (While, linecol(7, 13)..linecol(7, 18)),
                (Break, linecol(7, 19)..linecol(7, 24)),
                (Continue, linecol(7, 25)..linecol(7, 33)),
                (Return, linecol(7, 34)..linecol(7, 40)),
                (True, linecol(8, 0)..linecol(8, 4)),
                (False, linecol(8, 5)..linecol(8, 10)),
                (
                    Num {
                        kind: Dec,
                        digits: "123".into(),
                        suffix: "i32".into(),
                        val: 123,
                    },
                    linecol(8, 11)..linecol(8, 17),
                ),
                (
                    Ident {
                        name: "abcde".into(),
                    },
                    linecol(9, 0)..linecol(9, 5),
                ),
                (
                    Error(EmptyNum {
                        kind: Bin,
                        digits: "__".into(),
                        suffix: "xxx".into(),
                    }),
                    linecol(10, 0)..linecol(10, 7),
                ),
                (
                    Error(InvalidChar { c: '♡' }),
                    linecol(10, 8)..linecol(10, 9),
                ),
                (
                    Error(UnclosedBlockComment {
                        body: "a/*b".into(),
                        open_cnt: 2,
                    }),
                    linecol(10, 10)..linecol(10, 16),
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
                    linecol(0, 0)..linecol(0, 7),
                ),
                (
                    Ident {
                        name: "漢字".into(),
                    },
                    linecol(0, 8)..linecol(0, 10),
                ),
            ],
        )
    }

    /// Tests lexing numbers.
    #[test]
    fn test_next_number() {
        test_lex(
            r"0 0123i32 1_234_567_890
0b0101_1010usize
0xab_01_EFu64",
            vec![
                (
                    Num {
                        kind: Dec,
                        digits: "0".into(),
                        suffix: "".into(),
                        val: 0,
                    },
                    linecol(0, 0)..linecol(0, 1),
                ),
                (
                    Num {
                        kind: Dec,
                        digits: "0123".into(),
                        suffix: "i32".into(),
                        val: 0123,
                    },
                    linecol(0, 2)..linecol(0, 9),
                ),
                (
                    Num {
                        kind: Dec,
                        digits: "1_234_567_890".into(),
                        suffix: "".into(),
                        val: 1_234_567_890,
                    },
                    linecol(0, 10)..linecol(0, 23),
                ),
                (
                    Num {
                        kind: Bin,
                        digits: "0101_1010".into(),
                        suffix: "usize".into(),
                        val: 0b0101_1010,
                    },
                    linecol(1, 0)..linecol(1, 16),
                ),
                (
                    Num {
                        kind: Hex,
                        digits: "ab_01_EF".into(),
                        suffix: "u64".into(),
                        val: 0xab_01_EF,
                    },
                    linecol(2, 0)..linecol(2, 13),
                ),
            ],
        )
    }
}
