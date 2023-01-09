use crate::intern::{Intern, StrInterner};
use bumpalo::Bump;

/// Global context.
pub struct Ctx<'arn> {
    /// Arena, or allocator.
    arena: &'arn Bump,
    /// String interner.
    pub str_interner: StrInterner<'arn>,
}

impl<'arn> Ctx<'arn> {
    /// Creates a new context.
    pub fn new(arena: &'arn Bump) -> Self {
        Self {
            arena,
            str_interner: StrInterner::new(),
        }
    }

    /// Interns a string.
    #[inline]
    pub fn str(&self, s: &str) -> Intern<'arn, str> {
        self.str_interner.intern(s, |s| self.arena.alloc_str(s))
    }
}
