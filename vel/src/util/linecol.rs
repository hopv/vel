//! Position and span utilities.

use std::fmt::Display;
use std::ops::Range;

/// A position in a string represented in the line and column numbers.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct LineCol {
    /// Line number, 0-origin.
    pub line: usize,
    /// Column number, 0-origin.
    pub col: usize,
}

/// Makes a line-column position.
#[inline]
pub fn linecol(line: usize, col: usize) -> LineCol {
    LineCol { line, col }
}

impl Display for LineCol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}

impl LineCol {
    /// Gets the line-column position after the character `c` at `self`.
    #[inline]
    pub fn after(self, c: char) -> Self {
        let LineCol { line, col } = self;
        if c == '\n' {
            linecol(line + 1, 0)
        } else {
            linecol(line, col + 1)
        }
    }
}

/// A continuous span in a string.
///
/// It is an alias of `Range<LineCol>`,
/// consisting of `start, end: LineCol`.
/// It can be constructed by the operator `..`.
pub type Span = Range<LineCol>;
