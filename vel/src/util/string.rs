//! String utilities.

use std::fmt::Display;
use std::ops::{Add, AddAssign, Deref, Index, Range, RangeFrom, RangeFull, RangeTo};

////////////////////////////////////////////////////////////////////////////////

/// Byte offset in a string.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct Offset(usize);

impl Add<usize> for Offset {
    type Output = Self;
    #[inline]
    fn add(self, rhs: usize) -> Self::Output {
        Offset(self.0 + rhs)
    }
}

impl AddAssign<usize> for Offset {
    #[inline]
    fn add_assign(&mut self, rhs: usize) {
        self.0 += rhs;
    }
}

////////////////////////////////////////////////////////////////////////////////

/// Character index.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct CharIdx(usize);

impl Add<usize> for CharIdx {
    type Output = Self;
    #[inline]
    fn add(self, rhs: usize) -> Self::Output {
        CharIdx(self.0 + rhs)
    }
}

impl AddAssign<usize> for CharIdx {
    #[inline]
    fn add_assign(&mut self, rhs: usize) {
        self.0 += rhs;
    }
}

////////////////////////////////////////////////////////////////////////////////

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

////////////////////////////////////////////////////////////////////////////////

/// Indexed string.
///
/// A line-column offset `LineCol` and a byte offset `usize` can be
/// mutually converted in *O*(1) time.
pub struct IdxString {
    /// Body string.
    body: String,
    /// Caches the following info: The `line`-th (0-origin) line
    /// starts with the `line_idxs[line]`-th (0-origin) Unicode character (not byte).
    line_idxs: Vec<CharIdx>,
    /// Caches the following info: The `idx`-th (0-origin) Unicode character
    /// is the `char_offsets[idx]`-th (0-origin) byte.
    ///
    /// `char_offsets.len()` - 1 equals the number of Unicode characters in the string.
    char_offsets: Vec<Offset>,
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
            line_idxs: vec![CharIdx(0)],
            char_offsets: vec![Offset(0)],
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
                self.line_idxs.push(CharIdx(self.char_offsets.len()));
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
    pub fn linecol(&self, o: Offset) -> LineCol {
        self.linecols[o.0]
    }

    /// Translates a line-column position into an offset.
    ///
    /// Works in *O*(1) time.
    #[inline]
    pub fn offset(&self, lc: LineCol) -> Offset {
        let LineCol { line, col } = lc;
        self.char_offsets[(self.line_idxs[line] + col).0]
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

impl Index<Range<Offset>> for IdxString {
    type Output = str;
    #[inline]
    fn index(&self, span: Range<Offset>) -> &Self::Output {
        &self.body[span.start.0..span.end.0]
    }
}
impl Index<RangeFrom<Offset>> for IdxString {
    type Output = str;
    #[inline]
    fn index(&self, span: RangeFrom<Offset>) -> &Self::Output {
        &self.body[span.start.0..]
    }
}
impl Index<RangeTo<Offset>> for IdxString {
    type Output = str;
    #[inline]
    fn index(&self, span: RangeTo<Offset>) -> &Self::Output {
        &self.body[..span.end.0]
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
    fn index(&self, span: Range<LineCol>) -> &Self::Output {
        &self[self.offset(span.start)..self.offset(span.end)]
    }
}
impl Index<RangeFrom<LineCol>> for IdxString {
    type Output = str;
    #[inline]
    fn index(&self, span: RangeFrom<LineCol>) -> &Self::Output {
        &self[self.offset(span.start)..]
    }
}
impl Index<RangeTo<LineCol>> for IdxString {
    type Output = str;

    #[inline]
    fn index(&self, span: RangeTo<LineCol>) -> &Self::Output {
        &self[..self.offset(span.end)]
    }
}

////////////////////////////////////////////////////////////////////////////////

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
        assert_eq!(is.offset(linecol(0, 3)), Offset(3));
        assert_eq!(is.offset(linecol(1, 1)), Offset(7));
    }

    /// Tests `IdxString::linecol`.
    #[test]
    fn test_linecol() {
        let is = IdxString::from(STR);
        assert_eq!(is.linecol(Offset(3)), linecol(0, 3));
        assert_eq!(is.linecol(Offset(7)), linecol(1, 1));
    }

    /// Tests index access
    #[test]
    fn test_index() {
        let is = IdxString::from(STR);
        assert_eq!(&is[Offset(0)..Offset(3)], "abc");
        assert_eq!(&is[linecol(0, 1)..linecol(1, 1)], "bc\n漢");
    }
}
