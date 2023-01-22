//! Basic utilities.

use std::fmt::{Display, Formatter, Result};
use std::ops::Range;

/// Trait for an external copy method.
pub trait CopyExt {
    fn copy(&self) -> Self;
}

impl<T: Copy> CopyExt for Range<T> {
    /// Copy a range.
    fn copy(&self) -> Range<T> {
        self.start..self.end
    }
}

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
