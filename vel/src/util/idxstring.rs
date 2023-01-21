//! Indexed string.

use std::ops::{Deref, Index, Range, RangeFrom, RangeTo};

use super::linecol::{linecol, LineCol};

/// Indexed string.
pub struct IdxString {
    /// Body string.
    body: String,
    /// Caches the following info: The `line`-th (0-origin) line
    /// starts with the `line_idxs[line]`-th (0-origin) Unicode character (not byte).
    line_idxs: Vec<usize>,
    /// Caches the following info: The `idx`-th (0-origin) Unicode character
    /// is the `char_offsets[idx]`-th (0-origin) byte.
    ///
    /// `char_offsets.len()` - 1 equals the number of Unicode characters in the string.
    char_offsets: Vec<usize>,
    /// Caches the following info: The character at the `offset`-th (0-origin) byte
    /// has the line-column position `linecols[offset]`.
    linecols: Vec<LineCol>,
}

impl IdxString {
    /// Makes a new empty indexed string.
    #[inline]
    pub fn new() -> Self {
        IdxString {
            body: String::new(),
            line_idxs: vec![0],
            char_offsets: vec![0],
            linecols: vec![linecol(0, 0)],
        }
    }

    /// Pushes `str` to the end of the body string
    /// and updates the caches.
    #[inline]
    pub fn push_str(&mut self, s: &str) {
        let mut last_offset = *self.char_offsets.last().unwrap();
        let mut last_linecol = *self.linecols.last().unwrap();
        for c in s.chars() {
            self.body.push(c);
            last_offset += c.len_utf8();
            self.char_offsets.push(last_offset);
            last_linecol = last_linecol.after(c);
            self.linecols.push(last_linecol);
            if c == '\n' {
                self.line_idxs.push(self.char_offsets.len() - 1);
            }
        }
    }

    /// Gets the reference to the body string.
    #[inline]
    pub fn as_str(&self) -> &str {
        &self.body
    }

    /// Translates an offset into an line-column position.
    #[inline]
    pub fn linecol(&self, offset: usize) -> LineCol {
        self.linecols[offset]
    }

    /// Translates a line-column position into an offset.
    /// Works even if the position ignores line breaks of the last line.
    #[inline]
    pub fn offset(&self, lc: LineCol) -> usize {
        let LineCol { line, col } = lc;
        self.char_offsets[self.line_idxs[line] + col]
    }

    /// Gets the character at the line-column position.
    pub fn char_at(&self, lc: usize) -> char {
        self.body[lc..].chars().next().unwrap()
    }
}

impl From<String> for IdxString {
    /// Converts `String` into `IdxString`.
    #[inline]
    fn from(s: String) -> Self {
        let mut is = IdxString::new();
        is.push_str(&s);
        is
    }
}

impl From<&str> for IdxString {
    /// Converts `&str` into `IdxString`.
    #[inline]
    fn from(s: &str) -> Self {
        IdxString::from(String::from(s))
    }
}

impl From<IdxString> for String {
    /// Converts `IdxString` into `String`.
    #[inline]
    fn from(idx: IdxString) -> Self {
        idx.body
    }
}

impl Deref for IdxString {
    type Target = str;

    #[inline]
    fn deref(&self) -> &Self::Target {
        self.as_str()
    }
}

impl Index<Range<LineCol>> for IdxString {
    type Output = str;

    #[inline]
    fn index(&self, range: Range<LineCol>) -> &Self::Output {
        &self.body[self.offset(range.start)..self.offset(range.end)]
    }
}
impl Index<RangeFrom<LineCol>> for IdxString {
    type Output = str;

    #[inline]
    fn index(&self, range: RangeFrom<LineCol>) -> &Self::Output {
        &self.body[self.offset(range.start)..]
    }
}
impl Index<RangeTo<LineCol>> for IdxString {
    type Output = str;

    #[inline]
    fn index(&self, range: RangeTo<LineCol>) -> &Self::Output {
        &self.body[..self.offset(range.end)]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The test string.
    const STR: &str = r"abc
漢字";

    /// Tests `IdxString`.
    #[test]
    fn test() {
        let is = IdxString::from(STR);
        // Tests `offset` and `linecol`.
        assert_eq!(is.offset(linecol(0, 3)), 3);
        assert_eq!(is.linecol(3), linecol(0, 3));
        assert_eq!(is.offset(linecol(1, 1)), 7);
        assert_eq!(is.linecol(7), linecol(1, 1));
        // Tests index access by `LineCol`.
        assert_eq!(&is[linecol(0, 1)..linecol(1, 1)], "bc\n漢");
    }
}
