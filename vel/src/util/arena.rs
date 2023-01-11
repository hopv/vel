/// Arena for allocation.
pub type Arena = bumpalo::Bump;

// String in Arena
pub type AString<'arn> = bumpalo::collections::String<'arn>;

pub trait ArenaExt {
    fn new_astring(&self) -> AString;
}
impl ArenaExt for Arena {
    /// Alias of `AString::new_in`
    fn new_astring<'arn>(&'arn self) -> AString<'arn> {
        AString::new_in(self)
    }
}

pub trait AStringExt<'arn> {
    /// Alias of `into_bump_str`
    fn into_str(self) -> &'arn str;
}
impl<'arn> AStringExt<'arn> for AString<'arn> {
    fn into_str(self) -> &'arn str {
        self.into_bump_str()
    }
}
