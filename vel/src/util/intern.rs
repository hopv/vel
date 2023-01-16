//! Interning (a.k.a. hash consing).

use crate::util::arena::Arena;
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
#[derive(Debug, Copy, Clone)]
pub struct Intern<'arn, T: ?Sized> {
    /// Id, given deterministically in the order of registration.
    pub id: usize,
    /// Reference to the object.
    pub obj: &'arn T,
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
    memo: HashMap<&'arn T, usize>,
}

impl<'arn, T: Clone + Eq + Hash> Interner<'arn, T> {
    /// Creates a new interner.
    #[inline]
    pub fn new() -> Self {
        Self {
            memo: HashMap::new(),
        }
    }

    /// Interns an object.
    pub fn intern(&mut self, o: &T, arena: &'arn Arena) -> Intern<'arn, T> {
        let memo = &mut self.memo;
        match memo.get_key_value(o) {
            Some((&obj, &id)) => Intern { id, obj },
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
    memo: HashMap<&'arn str, usize>,
}

impl<'arn> StrInterner<'arn> {
    /// Creates a new interner.
    #[inline]
    pub fn new() -> Self {
        Self {
            memo: HashMap::new(),
        }
    }

    /// Interns a string.
    pub fn intern(&mut self, s: &str, arena: &'arn Arena) -> Intern<'arn, str> {
        let memo = &mut self.memo;
        match memo.get_key_value(s) {
            Some((&obj, &id)) => Intern { id, obj },
            None => {
                let id = memo.len();
                let obj = arena.alloc_str(s);
                memo.insert(obj, id);
                Intern { id, obj }
            }
        }
    }
}
