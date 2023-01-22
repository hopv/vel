//! Interning (a.k.a. hash consing).

use crate::util::arena::Arena;
use std::fmt::{Debug, Display};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::{collections::hash_map::HashMap, hash::Hash};

////////////////////////////////////////////////////////////////////////////////

/// Intern, wrapping `&'arn T`.
///
/// Created only by `Interner`/`StrInterner`'s `intern`.
///
/// Allows *O*(1) equality check (for two interns from the same interner),
/// which is ensured to agree with the content equality.
///
/// Also allows *O*(1) comparison (for two interns from the same interner),
/// which is ensured to agree with the ordering of registration.
///
/// `T` can be `?Sized` (e.g., `str`).
#[derive(Debug, Copy, Clone)]
pub struct Intern<'arn, T: ?Sized> {
    /// Id of the interner that generated this intern.
    from: usize,
    /// This intern's id, given in the ordering of registration.
    id: usize,
    /// Reference to the object.
    pub obj: &'arn T,
}

impl<T: ?Sized + Display> Display for Intern<'_, T> {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.obj.fmt(f)
    }
}

/// Does *O*(1) equality check, ensured to agree with the object equality.
///
/// Panics if the two interns come from the same interner.
impl<T: ?Sized> PartialEq for Intern<'_, T> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        assert!(
            self.from == other.from,
            "Intern::eq called for two interns from different interners: #{} and #{}",
            self.from,
            other.from
        );
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

/// Does *O*(1) comparison, just comparing the intern ids,
/// which agrees with the ordering of registration (not the ordering of contents).
///
/// Panics if the two interns come from different interners.
impl<T: ?Sized> PartialOrd for Intern<'_, T> {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        assert!(
            self.from == other.from,
            "Intern::partial_cmp called for two interns from different interners: #{} and #{}",
            self.from,
            other.from
        );
        self.id.partial_cmp(&other.id)
    }
}

/// *O*(1) comparison, just comparing the intern ids,
/// which agrees with the ordering of registration (not the ordering of contents).
///
/// Panics if the two interns come from different interners.
impl<T: ?Sized> Ord for Intern<'_, T> {
    #[inline]
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        assert!(
            self.from == other.from,
            "Intern::cmp called for two interns from different interners: #{} and #{}",
            self.from,
            other.from
        );
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

////////////////////////////////////////////////////////////////////////////////

/// Interns objects of a sized type `T`.
/// Every instance of `Interner<T>` has a unique `interner_id`.
#[derive(Debug)]
pub struct Interner<'arn, T> {
    /// Id of the interner.
    id: usize,
    /// Memoization table.
    memo: HashMap<&'arn T, usize>,
    /// Arena
    arena: &'arn Arena,
}

impl<'arn, T: Clone + Eq + Hash> Interner<'arn, T> {
    /// Creates a new interner.
    #[inline]
    pub fn new(arena: &'arn Arena) -> Self {
        /// Counts the number of the existing interners.
        static CNT: AtomicUsize = AtomicUsize::new(0);
        Self {
            id: CNT.fetch_add(1, Ordering::Relaxed),
            memo: HashMap::new(),
            arena,
        }
    }

    /// Interns an object.
    pub fn intern(&mut self, o: &T) -> Intern<'arn, T> {
        let memo = &mut self.memo;
        match memo.get_key_value(o) {
            Some((&obj, &id)) => Intern {
                from: self.id,
                id,
                obj,
            },
            None => {
                let id = memo.len();
                let obj = self.arena.alloc_with(|| o.clone());
                memo.insert(obj, id);
                Intern {
                    from: self.id,
                    id,
                    obj,
                }
            }
        }
    }
}

////////////////////////////////////////////////////////////////////////////////

/// Interns strings.
/// Every instance of `StrInterner<T>` has a unique `interner_id`.
pub struct StrInterner<'arn> {
    /// Id of the interner.
    id: usize,
    /// Memoization table.
    memo: HashMap<&'arn str, usize>,
    /// Arena.
    arena: &'arn Arena,
}

impl<'arn> StrInterner<'arn> {
    /// Creates a new interner.
    #[inline]
    pub fn new(arena: &'arn Arena) -> Self {
        /// Counts the number of the existing interners.
        static CNT: AtomicUsize = AtomicUsize::new(0);
        Self {
            id: CNT.fetch_add(1, Ordering::Relaxed),
            memo: HashMap::new(),
            arena,
        }
    }

    /// Interns a string.
    pub fn intern(&mut self, s: &str) -> Intern<'arn, str> {
        let memo = &mut self.memo;
        match memo.get_key_value(s) {
            Some((&obj, &id)) => Intern {
                from: self.id,
                id,
                obj,
            },
            None => {
                let id = memo.len();
                let obj = self.arena.alloc_str(s);
                memo.insert(obj, id);
                Intern {
                    from: self.id,
                    id,
                    obj,
                }
            }
        }
    }
}

////////////////////////////////////////////////////////////////////////////////

#[cfg(test)]
mod tests {
    use super::*;

    /// Tests that equality and comparison of interns
    /// from the same interner works correctly.
    #[test]
    fn test_interner_cmp() {
        let arn = Arena::new();
        let mut interner = Interner::new(&arn);
        let i_123 = interner.intern(&123);
        let i_89 = interner.intern(&89);
        let i_123_ = interner.intern(&123);
        let i_456 = interner.intern(&456);
        assert!(i_123 == i_123_);
        assert!(i_123 != i_89);
        assert!(i_89 != i_456);
        // Comparison is based on the ordering of registration, not of contents.
        assert!(i_123 < i_89);
        assert!(i_89 < i_456);
    }

    /// Tests that equality and comparison of interns
    /// from the same string interner works correctly.
    #[test]
    fn test_str_interner_cmp() {
        let arn = Arena::new();
        let mut interner = StrInterner::new(&arn);
        let i_yeah = interner.intern(&"yeah");
        let i_hey = interner.intern(&"hey");
        let i_yeah_ = interner.intern(&"yeah");
        let i_wow = interner.intern(&"wow");
        assert!(i_yeah == i_yeah_);
        assert!(i_yeah != i_hey);
        assert!(i_hey != i_wow);
        // Comparison is based on the ordering of registration, not of contents.
        assert!(i_yeah < i_hey);
        assert!(i_hey < i_wow);
    }

    /// Tests that `eq` on interns from different interners panics.
    #[test]
    #[should_panic = "Intern::eq called for two interns from different interners"]
    fn test_eq_diff_interners() {
        let arn = Arena::new();
        let i0 = Interner::new(&arn).intern(&0);
        let i1 = Interner::new(&arn).intern(&1);
        let _ = i0 == i1;
    }

    /// Tests that `eq` on interns from different string interners panics.
    #[test]
    #[should_panic = "Intern::eq called for two interns from different interners"]
    fn test_eq_diff_str_interners() {
        let arn = Arena::new();
        let i0 = StrInterner::new(&arn).intern(&"0");
        let i1 = StrInterner::new(&arn).intern(&"1");
        let _ = i0 == i1;
    }

    /// Tests that `partial_cmp` on interns from different interners panics.
    #[test]
    #[should_panic = "Intern::partial_cmp called for two interns from different interners"]
    fn test_partial_cmp_diff_interners() {
        let arn = Arena::new();
        let i0 = Interner::new(&arn).intern(&0);
        let i1 = Interner::new(&arn).intern(&1);
        let _ = i0 < i1;
    }

    /// Tests that `cmp` on interns from different interners panics.
    #[test]
    #[should_panic = "Intern::cmp called for two interns from different interners"]
    fn test_cmp_diff_interners() {
        let arn = Arena::new();
        let i0 = Interner::new(&arn).intern(&0);
        let i1 = Interner::new(&arn).intern(&1);
        let _ = i0.cmp(&i1);
    }
}
