//! Indexed string.

use std::ops::{Deref, Index, Range, RangeFrom, RangeFull, RangeTo};

use super::linecol::{linecol, LineCol};

/// Indexed string.
///
/// A line-column offset `LineCol` and a byte offset `usize` can be
/// mutually converted in *O*(1) time.
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
            if c == '\n' {
                self.line_idxs.push(self.char_offsets.len());
            }
            // `l` is from 1 to 4 inclusive.
            let l = c.len_utf8();
            last_offset += l;
            self.char_offsets.push(last_offset);
            last_linecol = last_linecol.after(c);
            for _ in 0..l {
                self.linecols.push(last_linecol);
            }
        }
    }

    /// Gets the reference to the body string.
    #[inline]
    pub fn as_str(&self) -> &str {
        &self.body
    }

    /// Translates an offset into an line-column position.
    ///
    /// Works in *O*(1) time.
    #[inline]
    pub fn linecol(&self, offset: usize) -> LineCol {
        self.linecols[offset]
    }

    /// Translates a line-column position into an offset.
    ///
    /// Works in *O*(1) time.
    #[inline]
    pub fn offset(&self, lc: LineCol) -> usize {
        let LineCol { line, col } = lc;
        self.char_offsets[self.line_idxs[line] + col]
    }
}

impl From<&str> for IdxString {
    /// Converts `&str` into `IdxString`.
    #[inline]
    fn from(s: &str) -> Self {
        let mut is = IdxString::new();
        is.push_str(&s);
        is
    }
}

impl From<String> for IdxString {
    /// Converts `String` into `IdxString`.
    #[inline]
    fn from(s: String) -> Self {
        IdxString::from(s.as_str())
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

impl Index<Range<usize>> for IdxString {
    type Output = str;
    #[inline]
    fn index(&self, range: Range<usize>) -> &Self::Output {
        &self.body[range.start..range.end]
    }
}
impl Index<RangeFrom<usize>> for IdxString {
    type Output = str;
    #[inline]
    fn index(&self, range: RangeFrom<usize>) -> &Self::Output {
        &self.body[range.start..]
    }
}
impl Index<RangeTo<usize>> for IdxString {
    type Output = str;
    #[inline]
    fn index(&self, range: RangeTo<usize>) -> &Self::Output {
        &self.body[..range.end]
    }
}
impl Index<RangeFull> for IdxString {
    type Output = str;
    #[inline]
    fn index(&self, _: RangeFull) -> &Self::Output {
        &self.body[..]
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
    /// 漢 and 字 are 3-byte characters.
    const STR: &str = r"abc
漢字";

    /// Tests `IdxString::offset`.
    #[test]
    fn test_offset() {
        let is = IdxString::from(STR);
        assert_eq!(is.offset(linecol(0, 3)), 3);
        assert_eq!(is.offset(linecol(1, 1)), 7);
    }

    /// Tests `IdxString::linecol`.
    #[test]
    fn test_linecol() {
        let is = IdxString::from(STR);
        assert_eq!(is.linecol(3), linecol(0, 3));
        assert_eq!(is.linecol(7), linecol(1, 1));
    }

    /// Tests index access
    #[test]
    fn test_index() {
        let is = IdxString::from(STR);
        assert_eq!(&is[0..3], "abc");
        assert_eq!(&is[linecol(0, 1)..linecol(1, 1)], "bc\n漢");
    }
}
