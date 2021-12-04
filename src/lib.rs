#![cfg_attr(test, deny(missing_docs))]
#![cfg_attr(test, deny(warnings))]
#![doc(html_root_url = "https://docs.rs/unicase/2.6.0")]
#![cfg_attr(feature = "nightly", feature(test))]
#![no_std]
#![feature(const_fn_trait_bound)]

//! # UniCase
//!
//! UniCase provides a way of specifying strings that are case-insensitive.
//!
//! UniCase supports full [Unicode case folding](https://www.w3.org/International/wiki/Case_folding).
//!
//! ## Example
//!
//! ```rust
//! use unicase::UniCase;
//!
//! let a = UniCase::new("Maße");
//! let b = UniCase::new("MASSE");
//! let c = UniCase::new("mase");
//!
//! assert_eq!(a, b);
//! assert!(b != c);
//! ```

#[cfg(feature = "nightly")]
extern crate test;

#[cfg(any(feature = "std", test))]
extern crate std;

#[cfg(any(feature = "std", test))]
use std::borrow::Cow;
#[cfg(any(feature = "std", test))]
use std::string::String;

#[cfg(__unicase__iter_cmp)]
use core::cmp::Ordering;
use core::fmt;
use core::hash::{Hash, Hasher};
use core::ops::{Deref, DerefMut};
use core::str::FromStr;

use self::unicode::Unicode;

mod unicode;

/// Case Insensitive wrapper of strings.
#[derive(Clone, Copy)]
pub struct UniCase<S>(Unicode<S>);

/// Compare two string-like types for case-less equality, using unicode folding.
///
/// Equivalent to `UniCase::new(left) == UniCase::new(right)`.
///
/// Note: This will perform a scan for ASCII characters before doing the
/// the comparison. See `UniCase` for more information.
#[inline]
pub fn eq<S: AsRef<str> + ?Sized>(left: &S, right: &S) -> bool {
    UniCase::new(left) == UniCase::new(right)
}

macro_rules! inner {
    (mut $e:expr) => ({ &mut $e.0 });
    ($e:expr) => ({ &$e.0 });
}

impl<S: AsRef<str> + Default> Default for UniCase<S> {
    fn default() -> Self { Self::new(Default::default()) }
}

impl<S: AsRef<str>> UniCase<S> {
    /// Create a new `UniCase`.
    pub const fn new(s: S) -> UniCase<S> { UniCase(Unicode(s)) }
}

impl<S> UniCase<S> {
    /// Unwraps the inner value held by this `UniCase`.
    #[inline]
    pub fn into_inner(self) -> S { self.0.0 }
}

impl<S> Deref for UniCase<S> {
    type Target = S;
    #[inline]
    fn deref<'a>(&'a self) -> &'a S { inner!(self.0) }
}

impl<S> DerefMut for UniCase<S> {
    #[inline]
    fn deref_mut<'a>(&'a mut self) -> &'a mut S { inner!(mut self.0) }
}

impl<S: AsRef<str>> AsRef<str> for UniCase<S> {
    #[inline]
    fn as_ref(&self) -> &str { inner!(self.0).as_ref() }

}

impl<S: fmt::Debug> fmt::Debug for UniCase<S> {
    #[inline]
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(inner!(self.0), fmt)
    }
}

impl<S: fmt::Display> fmt::Display for UniCase<S> {
    #[inline]
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(inner!(self.0), fmt)
    }
}


impl<S1: AsRef<str>, S2: AsRef<str>> PartialEq<UniCase<S2>> for UniCase<S1> {
    #[inline]
    fn eq(&self, other: &UniCase<S2>) -> bool { self.0 == other.0 }
}

impl<S: AsRef<str>> Eq for UniCase<S> {}

impl<S: AsRef<str>> Hash for UniCase<S> {
    #[inline]
    fn hash<H: Hasher>(&self, hasher: &mut H) { self.0.hash(hasher) }
}

#[cfg(any(feature = "std", test))]
macro_rules! from_impl {
    ($from:ty => $to:ty; $by:ident) => (
        impl<'a> From<$from> for UniCase<$to> {
            fn from(s: $from) -> Self { UniCase::new(s.$by()) }
        }
    );
    ($from:ty => $to:ty) => ( from_impl!($from => $to; into); )
}

macro_rules! into_impl {
    ($to:ty) => (
        impl<'a> Into<$to> for UniCase<$to> {
            fn into(self) -> $to { self.into_inner() }
        }
    );
}

impl<S: AsRef<str>> From<S> for UniCase<S> {
    fn from(s: S) -> Self { UniCase::new(s) }
}

#[cfg(any(feature = "std", test))]
from_impl!(&'a str => Cow<'a, str>);
#[cfg(any(feature = "std", test))]
from_impl!(String => Cow<'a, str>);
#[cfg(any(feature = "std", test))]
from_impl!(&'a str => String);
#[cfg(any(feature = "std", test))]
from_impl!(Cow<'a, str> => String; into_owned);
#[cfg(any(feature = "std", test))]
from_impl!(&'a String => &'a str; as_ref);

into_impl!(&'a str);
#[cfg(any(feature = "std", test))]
into_impl!(String);
#[cfg(any(feature = "std", test))]
into_impl!(Cow<'a, str>);

#[cfg(__unicase__iter_cmp)]
impl<T: AsRef<str>> PartialOrd for UniCase<T> {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> { Some(Ord::cmp(self, other)) }
}

#[cfg(__unicase__iter_cmp)]
impl<T: AsRef<str>> Ord for UniCase<T> {
    #[inline]
    fn cmp(&self, other: &Self) -> Ordering { Ord::cmp(&self.0, &other.0) }
}

impl<S: FromStr + AsRef<str>> FromStr for UniCase<S> {
    type Err = <S as FromStr>::Err;
    fn from_str(s: &str) -> Result<UniCase<S>, Self::Err> { s.parse().map(UniCase::new) }
}

#[cfg(test)]
mod tests {
    use super::UniCase;
    use std::borrow::ToOwned;
    use std::hash::{Hash, Hasher};
    #[cfg(not(__unicase__default_hasher))]
    use std::hash::SipHasher as DefaultHasher;
    #[cfg(__unicase__default_hasher)]
    use std::collections::hash_map::DefaultHasher;
    use std::string::String;

    fn hash<T: Hash>(t: &T) -> u64 {
        let mut s = DefaultHasher::new();
        t.hash(&mut s);
        s.finish()
    }

    #[test]
    fn test_copy_for_refs() {
        fn foo<T>(_: UniCase<T>) {}

        let a = UniCase::new("foobar");
        foo(a);
        foo(a);
    }

    #[test]
    fn test_eq_ascii() {
        let a = UniCase::new("foobar");
        let b = UniCase::new("FOOBAR");
        let c = UniCase::new("FoObAr");

        assert_eq!(a, b);
        assert_eq!(b, a);
        assert_eq!(a, c);
        assert_eq!(c, a);
        assert_eq!(hash(&a), hash(&b));
        assert_eq!(hash(&a), hash(&c));
        assert!(a.is_ascii());
        assert!(b.is_ascii());
        assert!(c.is_ascii());
    }

    #[test]
    fn test_eq_unicode() {
        let a = UniCase::new("στιγμας");
        let b = UniCase::new("στιγμασ");
        assert_eq!(a, b);
        assert_eq!(b, a);
        assert_eq!(hash(&a), hash(&b));
    }

    #[test]
    fn test_eq_unicode_left_is_substring() {
        // https://github.com/seanmonstar/unicase/issues/38
        let a = UniCase::new("foo");
        let b = UniCase::new("foobar");

        assert!(a != b);
        assert!(b != a);
    }

    #[cfg(feature = "nightly")]
    #[bench]
    fn bench_unicase_ascii(b: &mut ::test::Bencher) {
        b.bytes = b"foobar".len() as u64;
        let x = UniCase::new("foobar");
        let y = UniCase::new("FOOBAR");
        b.iter(|| assert_eq!(x, y));
    }

    #[cfg(feature = "nightly")]
    static SUBJECT: &'static [u8] = b"ffoo bar baz foo bar baz foo bar baz foo bar baz foo bar baz foo bar baz foo bar baz foo bar baz foo bar baz foo bar baz foo bar baz foo bar baz foo bar baz foo bar baz foo bar baz foo bar baz foo bar baz foo bar baz foo bar baz foo bar baz foo bar baz foo bar baz foo bar baz foo bar baz foo bar baz foo bar baz foo bar baz foo bar baz foo bar baz foo bar baz foo bar baz foo bar baz foo bar baz foo bar baz foo bar baz foo bar baz foo bar baz foo bar baz foo bar baz foo bar baz foo bar baz foo bar baz foo bar baz foo bar baz foo bar baz foo bar baz foo bar baz foo bar baz foo bar baz foo bar baz foo bar baz foo bar baz foo bar baz foo bar baz foo bar baz foo bar baz foo bar baz oo bar baz quux herp derp";

    #[cfg(feature = "nightly")]
    #[inline(never)]
    fn is_ascii(bytes: &[u8]) -> bool {
        #[allow(unused, deprecated)]
        use std::ascii::AsciiExt;
        bytes.is_ascii()
    }

    #[cfg(feature = "nightly")]
    #[bench]
    fn bench_is_ascii(b: &mut ::test::Bencher) {
        b.iter(|| assert!(is_ascii(SUBJECT)));
    }

    #[cfg(feature = "nightly")]
    #[bench]
    fn bench_is_utf8(b: &mut ::test::Bencher) {
        b.iter(|| assert!(::std::str::from_utf8(SUBJECT).is_ok()));
    }

    #[cfg(__unicase__iter_cmp)]
    #[test]
    fn test_case_cmp() {
        assert!(UniCase::new("a") < UniCase::new("B"));

        assert!(UniCase::new("A") < UniCase::new("b"));
        assert!(UniCase::new("aa") > UniCase::new("a"));

        assert!(UniCase::new("a") < UniCase::new("aa"));
        assert!(UniCase::new("a") < UniCase::new("AA"));
    }

    #[test]
    fn test_from_impls() {
        let view: &'static str = "foobar";
        let _: UniCase<&'static str> = view.into();
        let _: UniCase<&str> = view.into();
        let _: UniCase<String> = view.into();

        let owned: String = view.to_owned();
        let _: UniCase<&str> = (&owned).into();
        let _: UniCase<String> = owned.into();
    }

    #[test]
    fn test_into_impls() {
        let view: UniCase<&'static str> = UniCase::new("foobar");
        let _: &'static str = view.into();
        let _: &str = view.into();

        let owned: UniCase<String> = "foobar".into();
        let _: String = owned.clone().into();
        let _: &str = owned.as_ref();
    }

    #[cfg(__unicase__const_fns)]
    #[test]
    fn test_unicase_unicode_const() {
        const _UNICASE: UniCase<&'static str> = UniCase::new("");
    }
}
