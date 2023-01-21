//! Vel lexer.

use std::fmt::{Display, Formatter, Result};
use std::ops::Range;
use std::str::Chars;

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
    /// Integer numeric literal, non-negative.
    Num {
        /// Kind.
        kind: NumKind,
        /// Offset of the suffix within the literal.
        suffix_offset: usize,
        /// Pre-calculated value.
        val: u128,
    },
    /// Identifier.
    Ident,
    /// Lifetime identifier.
    LftIdent,
    /// Line comment, `//...`.
    LineComment,
    /// Block comment, `/* ... */` (nestable).
    BlockComment,
    /// Whitespace.
    Whitespace {
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
        /// Offset of the suffix within the literal.
        suffix_offset: usize,
    },
    /// Unclosed block comment.
    UnclosedBlockComment {
        /// Number of unclosed `/*`s.
        open_cnt: usize,
    },
    /// Stray quote.
    StrayQuote,
    /// Invalid character.
    InvalidChar(char),
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
            EmptyNum { kind, .. } => write!(
                f,
                "{} number without digits",
                match kind {
                    Dec => unreachable!("EmptyNum with Dec kind"),
                    Bin => "Binary",
                    Hex => "Hexadecimal",
                },
            ),
            UnclosedBlockComment { open_cnt, .. } => {
                write!(
                    f,
                    "Unclosed block comment until EOF (waiting for {} occurrence{} of */)",
                    open_cnt,
                    if *open_cnt > 1 { "s" } else { "" }
                )
            }
            StrayQuote => write!(f, "Stray quote (')"),
            InvalidChar(c) => write!(f, "Invalid character {}", c),
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
pub struct Lexer<'a> {
    /// String.
    s: &'a str,
    /// Input iterator.
    chars: Chars<'a>,
    /// Current head character.
    head: OrEof<char>,
    /// Current offset in bytes.
    offset: usize,
}

impl<'a> Lexer<'a> {
    /// Creates a lexer.
    pub fn new(s: &'a str) -> Self {
        let mut chars = s.chars();
        let head = chars.next().into();
        Self {
            s,
            chars,
            head,
            offset: 0,
        }
    }

    /// Gets the current offset.
    pub fn offset(&self) -> usize {
        self.offset
    }

    /// Moves the cursor one character ahead.
    #[inline]
    fn mov(&mut self) {
        match self.head {
            Eof => panic!("Cannot perform mov when the head is EOF"),
            Just(c) => {
                self.offset += c.len_utf8();
                self.head = self.chars.next().into();
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
            '1'..='9' => {
                let from = self.offset;
                return self.lex_num(from, Dec, false);
            }
            // `'`
            '\'' => self.mov_and().lex_quote(),
            // Name, starting with '_' or an alphabetic character
            _ if head == '_' || head.is_alphabetic() => return self.lex_name(),
            // Starting with a whitespace character
            _ if head.is_whitespace() => return self.lex_whitespace(),
            // Invalid character
            _ => self.mov_just(Error(InvalidChar(head))),
        }
    }

    /// Lexes the next token, starting with `//`.
    fn lex_slash2(&mut self) -> OrEof<Token> {
        loop {
            match self.head {
                Eof | Just('\n') => break,
                _ => self.mov(),
            }
        }
        Just(LineComment)
    }

    /// Lexes the next token, starting with `/*`.
    fn lex_slash_ast(&mut self) -> OrEof<Token> {
        let mut open_cnt = 1usize;
        loop {
            match self.head {
                Eof => return Just(Error(UnclosedBlockComment { open_cnt })),
                Just(head) => {
                    self.mov();
                    match (head, self.head) {
                        // `/*`
                        ('/', Just('*')) => {
                            open_cnt += 1;
                            self.mov();
                        }
                        // `*/`
                        ('*', Just('/')) => {
                            open_cnt -= 1;
                            self.mov();
                            if open_cnt == 0 {
                                return Just(BlockComment);
                            }
                        }
                        _ => {}
                    }
                }
            }
        }
    }

    /// Lexes the next token, starting with `0`.
    fn lex_0(&mut self) -> OrEof<Token> {
        let from = self.offset - 1;
        match self.head {
            // Binary number, starting with `0b`
            Just('b') => self.mov_and().lex_num(from, Bin, false),
            // Hexadecimal number, starting with `0x`
            Just('x') => self.mov_and().lex_num(from, Hex, false),
            // Decimal number, starting with `0` not followed by `b`/`x`
            _ => self.lex_num(from, Dec, true),
        }
    }

    /// Lexes the next number token of the radix.
    ///
    /// - `head_0`: Whether the number has a leading `0`.
    fn lex_num(&mut self, from: usize, kind: NumKind, head_0: bool) -> OrEof<Token> {
        let mut has_digit = head_0;
        // Parse digits and calculate the value.
        let mut val = 0u128;
        let radix = kind.radix();
        loop {
            match self.head {
                Just('_') => self.mov(),
                Just(head) if head.is_digit(radix) => {
                    has_digit = true;
                    val = val * (radix as u128) + head.to_digit(radix).unwrap() as u128;
                    self.mov();
                }
                _ => break,
            }
        }

        // Parse suffix.
        let suffix_offset = self.offset - from;
        loop {
            match self.head {
                // Continues for `_`, `0`-`9`, or an alphabetic character.
                Just(head) if head == '_' || head.is_ascii_digit() || head.is_alphabetic() => {
                    self.mov()
                }
                _ => break,
            };
        }

        Just(if has_digit {
            Num {
                kind,
                suffix_offset,
                val,
            }
        } else {
            Error(EmptyNum {
                kind,
                suffix_offset,
            })
        })
    }

    /// Lexes the next token, starting with `'`.
    fn lex_quote(&mut self) -> OrEof<Token> {
        let mut some_char = false;
        loop {
            match self.head {
                // Continues for `_`, `0`-`9`, or an alphabetic character.
                Just(head) if head == '_' || head.is_ascii_digit() || head.is_alphabetic() => {
                    some_char = true;
                    self.mov();
                }
                _ => break,
            };
        }
        Just(if some_char {
            LftIdent
        } else {
            Error(StrayQuote)
        })
    }

    /// Lexes a name.
    fn lex_name(&mut self) -> OrEof<Token> {
        let start = self.offset;
        loop {
            match self.head {
                // Continues for `_`, `0`-`9`, or an alphabetic character.
                Just(head) if head == '_' || head.is_ascii_digit() || head.is_alphabetic() => {
                    self.mov()
                }
                _ => break,
            };
        }
        Just(match &self.s[start..self.offset] {
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
            _ => Ident,
        })
    }

    /// Lexes the next token, starting with a whitespace character.
    fn lex_whitespace(&mut self) -> OrEof<Token> {
        let mut newline_cnt = 0;
        loop {
            match self.head {
                // Continues for a whitespace character.
                Just(head) if head.is_whitespace() => {
                    self.mov();
                    if head == '\n' {
                        newline_cnt += 1;
                    }
                }
                _ => break,
            }
        }
        Just(Whitespace { newline_cnt })
    }
}

impl Iterator for Lexer<'_> {
    type Item = (Token, Range<usize>);
    fn next(&mut self) -> Option<Self::Item> {
        let start = self.offset;
        match self.lex() {
            Eof => None,
            Just(tok) => {
                let end = self.offset;
                Some((tok, start..end))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Tests lexing the string `s`.
    fn test_lex(s: &str, want: Vec<(&str, Token)>) {
        let res: Vec<_> = Lexer::new(s)
            .filter_map(|(tok, span)| match tok {
                Whitespace { .. } => None,
                _ => Some((&s[span], tok)),
            })
            .collect();
        for (ts, want_ts) in res.iter().zip(want.iter()) {
            assert_eq!(ts, want_ts);
        }
        assert!(res.len() == want.len());
    }

    /// Tests lexing all kinds of tokens.
    #[test]
    fn test_all() {
        test_lex(
            r"/* a /* b */ c */ // xxx
() [] {}
, ; : . .. ..= ? !
- -> ~ = == + * #
< <= <> > >=
& && | || / ^ @ % $
fn let
if else loop while break continue return
true false 123i32
abcde 'lft
0b__xxx ♡ ' /*a/*b",
            vec![
                ("/* a /* b */ c */", BlockComment),
                ("// xxx", LineComment),
                ("(", LParen),
                (")", RParen),
                ("[", LBrack),
                ("]", RBrack),
                ("{", LCurly),
                ("}", RCurly),
                (",", Comma),
                (";", Semi),
                (":", Colon),
                (".", Dot),
                ("..", Dot2),
                ("..=", Dot2Eq),
                ("?", Quest),
                ("!", Bang),
                ("-", Dash),
                ("->", DashGt),
                ("~", Tilde),
                ("=", Eq),
                ("==", Eq2),
                ("+", Plus),
                ("*", Star),
                ("#", Hash),
                ("<", Lt),
                ("<=", LtEq),
                ("<>", LtGt),
                (">", Gt),
                (">=", GtEq),
                ("&", Amp),
                ("&&", Amp2),
                ("|", Bar),
                ("||", Bar2),
                ("/", Slash),
                ("^", Hat),
                ("@", At),
                ("%", Percent),
                ("$", Dollar),
                ("fn", Fn),
                ("let", Let),
                ("if", If),
                ("else", Else),
                ("loop", Loop),
                ("while", While),
                ("break", Break),
                ("continue", Continue),
                ("return", Return),
                ("true", True),
                ("false", False),
                (
                    "123i32",
                    Num {
                        kind: Dec,
                        suffix_offset: 3,
                        val: 123,
                    },
                ),
                ("abcde", Ident),
                ("'lft", LftIdent),
                (
                    "0b__xxx",
                    Error(EmptyNum {
                        kind: Bin,
                        suffix_offset: 4,
                    }),
                ),
                ("♡", Error(InvalidChar('♡'))),
                ("'", Error(StrayQuote)),
                ("/*a/*b", Error(UnclosedBlockComment { open_cnt: 2 })),
            ],
        );
    }

    /// Tests lexing identifiers.
    #[test]
    fn test_next_ident() {
        test_lex("ab1_cde 漢字", vec![("ab1_cde", Ident), ("漢字", Ident)])
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
                    "0",
                    Num {
                        kind: Dec,
                        suffix_offset: 1,
                        val: 0,
                    },
                ),
                (
                    "0123i32",
                    Num {
                        kind: Dec,
                        suffix_offset: 4,
                        val: 123,
                    },
                ),
                (
                    "1_234_567_890",
                    Num {
                        kind: Dec,
                        suffix_offset: 13,
                        val: 1_234_567_890,
                    },
                ),
                (
                    "0b0101_1010usize",
                    Num {
                        kind: Bin,
                        suffix_offset: 11,
                        val: 0b0101_1010,
                    },
                ),
                (
                    "0xab_01_EFu64",
                    Num {
                        kind: Hex,
                        suffix_offset: 10,
                        val: 0xab_01_EF,
                    },
                ),
            ],
        )
    }
}
