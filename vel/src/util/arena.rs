/// Arena for allocation.
pub type Arena = bumpalo::Bump;

// String in Arena.
pub type AString<'arn> = bumpalo::collections::String<'arn>;

/// Extends Arena.
pub trait ArenaExt {
    fn new_astring(&self) -> AString;
}
impl ArenaExt for Arena {
    /// Alias of `AString::new_in`.
    #[inline]
    fn new_astring<'arn>(&'arn self) -> AString<'arn> {
        AString::new_in(self)
    }
}

/// Extends AString.
pub trait AStringExt<'arn> {
    fn into_str(self) -> &'arn str;
}
impl<'arn> AStringExt<'arn> for AString<'arn> {
    /// Alias of `into_bump_str`.
    #[inline]
    fn into_str(self) -> &'arn str {
        self.into_bump_str()
    }
}
