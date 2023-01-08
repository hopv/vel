use bumpalo::Bump;
use std::{collections::hash_map::HashMap, hash::Hash};

/// Intern, wrapping `&'arn T`
/// `T` can be `?Sized` (e.g., `str`)
#[derive(Debug)]
pub struct Intern<'arn, T: ?Sized>(pub &'arn T);

/// `#[derive(Copy, Clone)]` didn't work, probably because of `T: ?Sized`
impl<'arn, T: ?Sized> Copy for Intern<'arn, T> {}
impl<'arn, T: ?Sized> Clone for Intern<'arn, T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'arn, T: ?Sized> Intern<'arn, T> {
    /// Turn into a raw pointer
    pub fn raw(&self) -> *const T {
        self.0
    }
}

impl<'arn, T: ?Sized> PartialEq for Intern<'arn, T> {
    fn eq(&self, other: &Self) -> bool {
        self.raw() == other.raw()
    }
}
impl<'arn, T: ?Sized> Eq for Intern<'arn, T> {}

impl<'arn, T: ?Sized> Hash for Intern<'arn, T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.raw().hash(state)
    }
}

impl<'arn, T: ?Sized> PartialOrd for Intern<'arn, T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.raw().partial_cmp(&other.raw())
    }
}
impl<'arn, T: ?Sized> Ord for Intern<'arn, T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.raw().cmp(&other.raw())
    }
}

impl<'arn, T: ?Sized> std::ops::Deref for Intern<'arn, T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        self.0
    }
}

/// Performs interning for sized types
pub struct Interner<'arn, T> {
    /// Arena
    arena: &'arn Bump,
    /// Memoization table
    memo: HashMap<T, Intern<'arn, T>>,
}

impl<'arn, T: Eq + Hash + Clone> Interner<'arn, T> {
    /// Create a new interner
    pub fn new(arena: &'arn Bump) -> Self {
        Self {
            arena,
            memo: HashMap::new(),
        }
    }

    /// Intern a value
    pub fn intern(&mut self, val: T) -> Intern<'arn, T> {
        let entry = self.memo.entry(val.clone());
        *entry.or_insert_with(|| Intern(self.arena.alloc(val)))
    }
}

/// Performs interning for strings
pub struct StringInterner<'arn> {
    /// Arena
    arena: &'arn Bump,
    /// Memoization table
    memo: HashMap<String, Intern<'arn, str>>,
}

impl<'arn> StringInterner<'arn> {
    /// Create a new interner
    pub fn new(arena: &'arn Bump) -> Self {
        Self {
            arena,
            memo: HashMap::new(),
        }
    }

    /// Intern a string
    pub fn intern(&mut self, val: &str) -> Intern<'arn, str> {
        let entry = self.memo.entry(val.to_string());
        *entry.or_insert_with(|| Intern(self.arena.alloc_str(val)))
    }
}
