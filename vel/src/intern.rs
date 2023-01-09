use bumpalo::Bump;
use std::cell::UnsafeCell;
use std::fmt::{Debug, Display};
use std::{collections::hash_map::HashMap, hash::Hash};

/// Intern, wrapping `&'arn T`.
///
/// `T` can be `?Sized` (e.g., `str`).
pub struct Intern<'arn, T: ?Sized> {
    /// Private body.
    body: &'arn T,
}

/// Mysteriously, `#[derive(Copy, Clone)]` doesn't work.
impl<'arn, T: ?Sized> Copy for Intern<'arn, T> {}
impl<'arn, T: ?Sized> Clone for Intern<'arn, T> {
    #[inline]
    fn clone(&self) -> Self {
        *self
    }
}

impl<'arn, T: ?Sized> Intern<'arn, T> {
    /// Turns into a raw pointer.
    #[inline]
    pub fn raw(&self) -> *const T {
        self.body
    }
}

impl<T: ?Sized + Debug> Debug for Intern<'_, T> {
    /// Debug outputs the pointer's address.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Intern({:?}: {:?})", self.raw(), self.body)
    }
}

impl<T: ?Sized + Display> Display for Intern<'_, T> {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.body.fmt(f)
    }
}

impl<T: ?Sized> PartialEq for Intern<'_, T> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.raw() == other.raw()
    }
}
impl<T: ?Sized> Eq for Intern<'_, T> {}

impl<T: ?Sized> Hash for Intern<'_, T> {
    #[inline]
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.raw().hash(state)
    }
}

impl<T: ?Sized> PartialOrd for Intern<'_, T> {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.raw().partial_cmp(&other.raw())
    }
}
impl<T: ?Sized> Ord for Intern<'_, T> {
    #[inline]
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.raw().cmp(&other.raw())
    }
}

impl<T: ?Sized> std::ops::Deref for Intern<'_, T> {
    type Target = T;
    #[inline]
    fn deref(&self) -> &Self::Target {
        self.body
    }
}

/// Interns objects of a sized type `T`.
pub struct Interner<'arn, T> {
    /// Arena.
    arena: &'arn Bump,
    /// Memoization table.
    memo: UnsafeCell<HashMap<T, Intern<'arn, T>>>,
}

impl<'arn, T: Eq + Hash + Clone> Interner<'arn, T> {
    /// Creates a new interner.
    #[inline]
    pub fn new(arena: &'arn Bump) -> Self {
        Self {
            arena,
            memo: UnsafeCell::new(HashMap::new()),
        }
    }

    /// Interns an object.
    pub fn intern(&self, obj: T) -> Intern<'arn, T> {
        // This is safe because only a shared reference is finally returned.
        let memo = unsafe { &mut *self.memo.get() };
        *memo.entry(obj.clone()).or_insert_with(|| Intern {
            body: self.arena.alloc(obj),
        })
    }
}

/// Interns strings.
pub struct StrInterner<'arn> {
    /// Arena.
    arena: &'arn Bump,
    /// Memoization table.
    memo: UnsafeCell<HashMap<String, Intern<'arn, str>>>,
}

impl<'arn> StrInterner<'arn> {
    /// Creates a new interner.
    #[inline]
    pub fn new(arena: &'arn Bump) -> Self {
        Self {
            arena,
            memo: UnsafeCell::new(HashMap::new()),
        }
    }

    /// Interns a string.
    pub fn intern(&self, s: &str) -> Intern<'arn, str> {
        // This is safe because only a shared reference is finally returned.
        let memo = unsafe { &mut *self.memo.get() };
        *memo.entry(s.to_string()).or_insert_with(|| Intern {
            body: self.arena.alloc_str(s),
        })
    }
}
