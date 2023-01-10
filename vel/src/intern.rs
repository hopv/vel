use bumpalo::Bump;
use std::cell::UnsafeCell;
use std::fmt::{Debug, Display};
use std::{collections::hash_map::HashMap, hash::Hash};

/// Intern, wrapping `&'arn T`.
///
/// Created by `Interner::intern`.
///
/// Allows *O*(1) equality check, ensured to agree with the object equality,
/// as long as both operand interns come from the same interner.
///
/// `T` can be `?Sized` (e.g., `str`).
#[derive(Debug)]
pub struct Intern<'arn, T: ?Sized> {
    /// Id, given deterministically in the order of registration.
    id: usize,
    /// Reference to the object.
    obj: &'arn T,
}

impl<'arn, T: ?Sized> Copy for Intern<'arn, T> {}
impl<'arn, T: ?Sized> Clone for Intern<'arn, T> {
    #[inline]
    fn clone(&self) -> Self {
        *self
    }
}

impl<T: ?Sized + Display> Display for Intern<'_, T> {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.obj.fmt(f)
    }
}

/// O(1) equality check, ensured to agree with the object equality,
/// as long as both operand interns come from the same interner.
impl<T: ?Sized> PartialEq for Intern<'_, T> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}
impl<T: ?Sized> Eq for Intern<'_, T> {}

impl<T: ?Sized> Hash for Intern<'_, T> {
    #[inline]
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state)
    }
}

impl<T: ?Sized> PartialOrd for Intern<'_, T> {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.id.partial_cmp(&other.id)
    }
}
impl<T: ?Sized> Ord for Intern<'_, T> {
    #[inline]
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.id.cmp(&other.id)
    }
}

impl<T: ?Sized> std::ops::Deref for Intern<'_, T> {
    type Target = T;
    #[inline]
    fn deref(&self) -> &Self::Target {
        self.obj
    }
}

/// Interns objects of a sized type `T`.
pub struct Interner<'arn, T> {
    /// Memoization table.
    memo: UnsafeCell<HashMap<&'arn T, usize>>,
}

impl<'arn, T: Clone + Eq + Hash> Interner<'arn, T> {
    /// Creates a new interner.
    #[inline]
    pub fn new() -> Self {
        Self {
            memo: UnsafeCell::new(HashMap::new()),
        }
    }

    /// Interns an object.
    ///
    /// This is safe as long as `T::clone`, `T::eq` and `T::hash`
    /// don't perform `self.intern`, which is almost always the case.
    pub unsafe fn intern(&self, o: &T, arena: &'arn Bump) -> Intern<'arn, T> {
        let memo = &mut *self.memo.get();
        match memo.get_key_value(o) {
            Some((obj, &id)) => Intern { id, obj },
            None => {
                let id = memo.len();
                let obj = arena.alloc_with(|| o.clone());
                memo.insert(obj, id);
                Intern { id, obj }
            }
        }
    }
}

/// Interns strings.
pub struct StrInterner<'arn> {
    /// Memoization table.
    memo: UnsafeCell<HashMap<&'arn str, usize>>,
}

impl<'arn> StrInterner<'arn> {
    /// Creates a new interner.
    #[inline]
    pub fn new() -> Self {
        Self {
            memo: UnsafeCell::new(HashMap::new()),
        }
    }

    /// Interns a string.
    pub fn intern(&self, s: &str, arena: &'arn Bump) -> Intern<'arn, str> {
        let memo = unsafe { &mut *self.memo.get() };
        // This mutation of memo is safe because only &str is used.
        match memo.get_key_value(s) {
            Some((obj, id)) => Intern { id: *id, obj },
            None => {
                let id = memo.len();
                let obj = arena.alloc_str(s);
                memo.insert(obj, id);
                Intern { id, obj }
            }
        }
    }
}
