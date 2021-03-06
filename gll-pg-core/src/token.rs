//! Small wrapper for Logos token
//!

use logos;

/// A wrapper around Logos token type, providing its slice and kind.
#[derive(Clone, Eq, PartialEq)]
pub struct LogosToken<'a, T: logos::Logos<'a>> {
    /// Token kind
    pub kind: T,
    /// Token slice
    pub slice: &'a str,
}
