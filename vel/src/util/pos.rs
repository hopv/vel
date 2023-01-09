use std::fmt::Display;

/// 2D position in a string.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct Pos {
    /// Line number, 0-origin.
    pub line: usize,
    /// Column number, 0-origin.
    pub col: usize,
}

/// Makes a position.
#[inline]
pub fn pos(line: usize, col: usize) -> Pos {
    Pos { line, col }
}

impl Display for Pos {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}

impl Pos {
    /// Gets the position after moving right.
    #[inline]
    pub fn right(self) -> Self {
        Pos {
            line: self.line,
            col: self.col + 1,
        }
    }

    /// Gets the position after newline.
    #[inline]
    pub fn newline(self) -> Self {
        Pos {
            line: self.line + 1,
            col: 0,
        }
    }

    /// Gets the position after the character.
    #[inline]
    pub fn after(self, c: char) -> Self {
        if c == '\n' {
            self.newline()
        } else {
            self.right()
        }
    }
}

/// 2D span in a string.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct Span {
    /// The starting position.
    pub from: Pos,
    /// The ending position.
    pub to: Pos,
}

impl Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}..{}", self.from, self.to)
    }
}

/// Makes a span.
#[inline]
pub fn span(from: Pos, to: Pos) -> Span {
    Span { from, to }
}

/// Utility for making a span.
#[macro_export]
macro_rules! span {
    (($from_line:expr , $from_col:expr) .. ($to_line:expr , $to_col:expr)) => {
        span(pos($from_line, $from_col), pos($to_line, $to_col))
    };
}
