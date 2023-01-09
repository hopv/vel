use crate::ctx::Ctx;
use crate::intern::Intern;
use crate::util::pos::{span, Pos, Span};
use std::fmt::Display;

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
    Ne,
    /// `*`
    Ast,
    /// `/\`
    And,
    /// `\/`
    Or,
    /// `~`
    Not,
    /// `<`
    Lt,
    /// `<=`
    Le,
    /// `>`
    Gt,
    /// `>=`
    Ge,
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
    Number(Intern<'arn, str>),
    /// Identifier
    Ident(Intern<'arn, str>),
    /// Whitespace
    Whitespace(Intern<'arn, str>),
    /// Line comment, `//...`
    LineComment(Intern<'arn, str>),

    /// Error by `\`
    ErrorBackslash,
    /// Error by an invalid character
    ErrorInvalidChar(char),
}
use Token::*;

impl Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LParen => write!(f, "("),
            RParen => write!(f, ")"),
            LBrack => write!(f, "["),
            RBrack => write!(f, "]"),
            LCurly => write!(f, "{{"),
            RCurly => write!(f, "}}"),
            Eq => write!(f, "="),
            Eq2 => write!(f, "=="),
            Ne => write!(f, "~="),
            Ast => write!(f, "*"),
            And => write!(f, "/\\"),
            Or => write!(f, "\\/"),
            Not => write!(f, "~"),
            Lt => write!(f, "<"),
            Le => write!(f, "<="),
            Gt => write!(f, ">"),
            Ge => write!(f, ">="),
            Plus => write!(f, "+"),
            Minus => write!(f, "-"),
            Div => write!(f, "/"),
            Fn => write!(f, "fn"),
            Let => write!(f, "let"),
            Number(s) => write!(f, "{}", s),
            Ident(s) => write!(f, "{}", s),
            Whitespace(s) => write!(f, "{}", s),
            LineComment(s) => write!(f, "//{}", s),

            ErrorBackslash => write!(f, "\\"),
            ErrorInvalidChar(c) => write!(f, "{}", c),
        }
    }
}

/// Lexes from a character stream to output Vel tokens.
pub fn lex<'arn>(
    from: Pos,
    mut ins: impl Iterator<Item = char>,
    out: impl FnMut(Token<'arn>, Span),
    ctx: &Ctx<'arn>,
) {
    if let Some(c) = ins.next() {
        lex_any(c, from, ins, out, ctx);
    }
}

/// Lexes, starting with any character.
fn lex_any<'arn>(
    c: char,
    from: Pos,
    ins: impl Iterator<Item = char>,
    mut out: impl FnMut(Token<'arn>, Span),
    ctx: &Ctx<'arn>,
) {
    {
        let otok = match c {
            '(' => Some(LParen),
            ')' => Some(RParen),
            '[' => Some(LBrack),
            ']' => Some(RBrack),
            '{' => Some(LCurly),
            '}' => Some(RCurly),
            '*' => Some(Ast),
            '+' => Some(Plus),
            '-' => Some(Minus),
            _ => None,
        };
        // Determined at this point
        if let Some(tok) = otok {
            let to = from.right();
            out(tok, span(from, to));
            return lex(to, ins, out, ctx);
        }
    }
    match c {
        // Starting with `=`
        '=' => {
            return lex_eq(from, ins, out, ctx);
        }
        // Starting with `~`
        '~' => {
            return lex_tilde(from, ins, out, ctx);
        }
        // Starting with `/`
        '/' => {
            return lex_slash(from, ins, out, ctx);
        }
        // Starting with `\`
        '\\' => {
            return lex_backslash(from, ins, out, ctx);
        }
        // Starting with `<`
        '<' => {
            return lex_lt(from, ins, out, ctx);
        }
        // Starting with `>`
        '>' => {
            return lex_gt(from, ins, out, ctx);
        }
        _ => {}
    }
    // Starting with a whitespace character
    if c.is_whitespace() {
        return lex_whitespace(c, from, ins, out, ctx);
    }
    // Starting with a digit, `0`-`9`
    if c.is_ascii_digit() {
        return lex_digit(c, from, ins, out, ctx);
    }
    // Starting with an alphabet character
    if c.is_alphabetic() {
        return lex_alphabet(c, from, ins, out, ctx);
    }
    // Invalid character
    let to = from.right();
    out(ErrorInvalidChar(c), span(from, to));
    return lex(to, ins, out, ctx);
}

/// Lexes, starting with `=`.
fn lex_eq<'arn>(
    from: Pos,
    mut ins: impl Iterator<Item = char>,
    mut out: impl FnMut(Token<'arn>, Span),
    ctx: &Ctx<'arn>,
) {
    match ins.next() {
        None => {
            // `=`
            let to = from.right();
            out(Eq, span(from, to));
            return;
        }
        // `==`
        Some('=') => {
            let to = from.right().right();
            out(Eq2, span(from, to));
            return lex(to, ins, out, ctx);
        }
        Some(c) => {
            // `=`
            let to = from.right();
            out(Eq, span(from, to));
            return lex_any(c, to, ins, out, ctx);
        }
    }
}

/// Lexes, starting with `~`.
fn lex_tilde<'arn>(
    from: Pos,
    mut ins: impl Iterator<Item = char>,
    mut out: impl FnMut(Token<'arn>, Span),
    ctx: &Ctx<'arn>,
) {
    match ins.next() {
        None => {
            // `~`
            let to = from.right();
            out(Not, span(from, to));
            return;
        }
        // `~=`
        Some('=') => {
            let to = from.right().right();
            out(Ne, span(from, to));
            return lex(to, ins, out, ctx);
        }
        Some(c) => {
            // `~`
            let to = from.right();
            out(Not, span(from, to));
            return lex_any(c, to, ins, out, ctx);
        }
    }
}

/// Lexes, starting with `/`.
fn lex_slash<'arn>(
    from: Pos,
    mut ins: impl Iterator<Item = char>,
    mut out: impl FnMut(Token<'arn>, Span),
    ctx: &Ctx<'arn>,
) {
    match ins.next() {
        None => {
            // `/`
            let to = from.right();
            out(Div, span(from, to));
            return;
        }
        // `/\`
        Some('\\') => {
            let to = from.right().right();
            out(And, span(from, to));
            return lex(to, ins, out, ctx);
        }
        // Starting with `//`
        Some('/') => {
            return lex_slash2(from, ins, out, ctx);
        }
        Some(c) => {
            // `/`
            let to = from.right();
            out(Div, span(from, to));
            return lex_any(c, to, ins, out, ctx);
        }
    }
}

/// Lexes, starting with `//`.
fn lex_slash2<'arn>(
    from: Pos,
    mut ins: impl Iterator<Item = char>,
    mut out: impl FnMut(Token<'arn>, Span),
    ctx: &Ctx<'arn>,
) {
    // Lexes a line comment
    let mut s = String::new();
    let mut to = from.right().right();
    loop {
        match ins.next() {
            None => {
                // End without a newline
                out(LineComment(ctx.intern_str(&s)), span(from, to));
                return;
            }
            Some(c) => {
                s.push(c);
                // End with a newline
                if c == '\n' {
                    to = to.newline();
                    out(LineComment(ctx.intern_str(&s)), span(from, to));
                    return lex(to, ins, out, ctx);
                }
                to = to.right();
            }
        }
    }
}

/// Lexes, starting with `\`.
fn lex_backslash<'arn>(
    from: Pos,
    mut ins: impl Iterator<Item = char>,
    mut out: impl FnMut(Token<'arn>, Span),
    ctx: &Ctx<'arn>,
) {
    match ins.next() {
        None => {
            // `\`
            let to = from.right();
            out(ErrorBackslash, span(from, to));
            return;
        }
        // `\/`
        Some('/') => {
            let to = from.right().right();
            out(Or, span(from, to));
            return lex(to, ins, out, ctx);
        }
        Some(c) => {
            // `\`
            let to = from.right();
            out(ErrorBackslash, span(from, to));
            return lex_any(c, to, ins, out, ctx);
        }
    }
}

/// Lexes, starting with `<`.
fn lex_lt<'arn>(
    from: Pos,
    mut ins: impl Iterator<Item = char>,
    mut out: impl FnMut(Token<'arn>, Span),
    ctx: &Ctx<'arn>,
) {
    match ins.next() {
        None => {
            // `<`
            let to = from.right();
            out(Lt, span(from, to));
            return;
        }
        // `<=`
        Some('=') => {
            let to = from.right().right();
            out(Le, span(from, to));
            return lex(to, ins, out, ctx);
        }
        Some(c) => {
            // `<`
            let to = from.right();
            out(Lt, span(from, to));
            return lex_any(c, to, ins, out, ctx);
        }
    }
}

/// Lexes, starting with `>`.
fn lex_gt<'arn>(
    from: Pos,
    mut ins: impl Iterator<Item = char>,
    mut out: impl FnMut(Token<'arn>, Span),
    ctx: &Ctx<'arn>,
) {
    match ins.next() {
        None => {
            // `>`
            let to = from.right();
            out(Gt, span(from, to));
            return;
        }
        // `>=`
        Some('=') => {
            let to = from.right().right();
            out(Ge, span(from, to));
            return lex(to, ins, out, ctx);
        }
        Some(c) => {
            // `>`
            let to = from.right();
            out(Gt, span(from, to));
            return lex_any(c, to, ins, out, ctx);
        }
    }
}

/// Lexes, starting with a whitespace character
fn lex_whitespace<'arn>(
    c: char,
    from: Pos,
    mut ins: impl Iterator<Item = char>,
    mut out: impl FnMut(Token<'arn>, Span),
    ctx: &Ctx<'arn>,
) {
    let mut s = c.to_string();
    let mut to = from.after(c);
    loop {
        match ins.next() {
            None => {
                out(Whitespace(ctx.intern_str(&s)), span(from, to));
                return;
            }
            Some(c) => {
                if !c.is_whitespace() {
                    out(Whitespace(ctx.intern_str(&s)), span(from, to));
                    return lex_any(c, to, ins, out, ctx);
                }
                s.push(c);
                to = to.after(c);
            }
        }
    }
}

/// Lexes, starting with a digit `0`-`9`.
fn lex_digit<'arn>(
    c: char,
    from: Pos,
    mut ins: impl Iterator<Item = char>,
    mut out: impl FnMut(Token<'arn>, Span),
    ctx: &Ctx<'arn>,
) {
    let mut s = c.to_string();
    let mut to = from.right();
    loop {
        match ins.next() {
            None => {
                out(Number(ctx.intern_str(&s)), span(from, to));
                return;
            }
            Some(c) => {
                if !(c.is_ascii_digit() || c == '_') {
                    out(Number(ctx.intern_str(&s)), span(from, to));
                    return lex_any(c, to, ins, out, ctx);
                }
                // Continues for a digit `0`-`9` or `_`.
                s.push(c);
                to = to.right();
            }
        }
    }
}

/// Lexes, starting with an alphabet character
fn lex_alphabet<'arn>(
    c: char,
    from: Pos,
    mut ins: impl Iterator<Item = char>,
    mut out: impl FnMut(Token<'arn>, Span),
    ctx: &Ctx<'arn>,
) {
    let mut s = c.to_string();
    let mut to = from.right();
    loop {
        match ins.next() {
            None => {
                out(name_token(s, ctx), span(from, to));
                return;
            }
            Some(c) => {
                if !(c.is_alphabetic() || c.is_ascii_digit() || c == '_') {
                    out(name_token(s, ctx), span(from, to));
                    return lex_any(c, to, ins, out, ctx);
                }
                // Continues for an alphabet character, `0`-`9`, or `_`.
                s.push(c);
                to = to.right();
            }
        }
    }
}

/// Gets the token for the name.
fn name_token<'arn>(s: String, ctx: &Ctx<'arn>) -> Token<'arn> {
    match s.as_str() {
        "fn" => Fn,
        "let" => Let,
        s => Ident(ctx.intern_str(s)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::util::pos::pos;
    use bumpalo::Bump;

    /// Template for testing
    fn test_lex(
        text: &str,
        get_want: impl for<'arn> FnOnce(&Ctx<'arn>) -> Vec<(Token<'arn>, Span)>,
    ) {
        let arena = Bump::new();
        let ctx = Ctx::new(&arena);
        let mut res = Vec::new();
        let out = |tok, span| match tok {
            Whitespace(_) => {}
            _ => res.push((tok, span)),
        };
        lex(pos(0, 0), text.chars(), out, &ctx);
        let want = get_want(&ctx);
        for (ts, want_ts) in res.iter().zip(want.iter()) {
            assert_eq!(ts, want_ts);
        }
        assert!(res.len() == want.len());
    }

    /// Tests lexing all kinds of tokens.
    #[test]
    fn test_lex_all() {
        test_lex(
            r"
() [] {} // xxx
= == ~= *
/\ \/ ~
< <= > >=
+ - /
fn let
123 abcde
\ ♡",
            |ctx| {
                vec![
                    (LParen, span(pos(1, 0), pos(1, 1))),
                    (RParen, span(pos(1, 1), pos(1, 2))),
                    (LBrack, span(pos(1, 3), pos(1, 4))),
                    (RBrack, span(pos(1, 4), pos(1, 5))),
                    (LCurly, span(pos(1, 6), pos(1, 7))),
                    (RCurly, span(pos(1, 7), pos(1, 8))),
                    (
                        LineComment(ctx.intern_str(" xxx\n")),
                        span(pos(1, 9), pos(2, 0)),
                    ),
                    (Eq, span(pos(2, 0), pos(2, 1))),
                    (Eq2, span(pos(2, 2), pos(2, 4))),
                    (Ne, span(pos(2, 5), pos(2, 7))),
                    (Ast, span(pos(2, 8), pos(2, 9))),
                    (And, span(pos(3, 0), pos(3, 2))),
                    (Or, span(pos(3, 3), pos(3, 5))),
                    (Not, span(pos(3, 6), pos(3, 7))),
                    (Lt, span(pos(4, 0), pos(4, 1))),
                    (Le, span(pos(4, 2), pos(4, 4))),
                    (Gt, span(pos(4, 5), pos(4, 6))),
                    (Ge, span(pos(4, 7), pos(4, 9))),
                    (Plus, span(pos(5, 0), pos(5, 1))),
                    (Minus, span(pos(5, 2), pos(5, 3))),
                    (Div, span(pos(5, 4), pos(5, 5))),
                    (Fn, span(pos(6, 0), pos(6, 2))),
                    (Let, span(pos(6, 3), pos(6, 6))),
                    (Number(ctx.intern_str("123")), span(pos(7, 0), pos(7, 3))),
                    (Ident(ctx.intern_str("abcde")), span(pos(7, 4), pos(7, 9))),
                    (ErrorBackslash, span(pos(8, 0), pos(8, 1))),
                    (ErrorInvalidChar('♡'), span(pos(8, 2), pos(8, 3))),
                ]
            },
        );
    }

    /// Tests lexing identifiers.
    #[test]
    fn test_lex_ident() {
        test_lex("ab1_cde 漢字", |ctx| {
            vec![
                (Ident(ctx.intern_str("ab1_cde")), span(pos(0, 0), pos(0, 7))),
                (Ident(ctx.intern_str("漢字")), span(pos(0, 8), pos(0, 10))),
            ]
        })
    }

    /// Tests lexing numbers.
    #[test]
    fn test_lex_number() {
        test_lex("0 123 1_234_567", |ctx| {
            vec![
                (Number(ctx.intern_str("0")), span(pos(0, 0), pos(0, 1))),
                (Number(ctx.intern_str("123")), span(pos(0, 2), pos(0, 5))),
                (
                    Number(ctx.intern_str("1_234_567")),
                    span(pos(0, 6), pos(0, 15)),
                ),
            ]
        })
    }
}
