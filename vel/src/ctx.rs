use crate::intern::{Intern, StrInterner};
use bumpalo::Bump;

/// Global context.
pub struct Ctx<'arn> {
    /// String interner.
    pub str_interner: StrInterner<'arn>,
}

impl<'arn> Ctx<'arn> {
    /// Creates a new context.
    pub fn new(arena: &'arn Bump) -> Self {
        Self {
            str_interner: StrInterner::new(arena),
        }
    }

    /// Interns a string.
    #[inline]
    pub fn intern_str(&self, s: &str) -> Intern<'arn, str> {
        self.str_interner.intern(s)
    }
}
