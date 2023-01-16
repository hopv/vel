//! Position and span utilities.

use std::fmt::Display;
use std::ops::Range;

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

/// 2D continuous span in a string.
///
/// It is an alias of `Range<Pos>`,
/// so it can be constructed by `p..q` for `p, q: Pos`.
pub type Span = Range<Pos>;
