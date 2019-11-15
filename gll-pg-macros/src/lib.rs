//! A parser generator for GLL grammar, macros part
//!
//! This library includes the proc macro to generate code.
//!
#![feature(proc_macro_diagnostic)]

extern crate proc_macro;

mod gen;

use syn::{parse_macro_input, ItemImpl};

/// The macro to generate a GLL parser.
///
/// It should be used on a `impl` block with a lexer:
///
/// ```ignore
/// use gll_pg_core::*;
/// use gll_pg_macros::gll;
///
/// struct Parser {}
/// #[gll(S, Token)]
/// impl Parser {
///     // ...
///     #[rule(S -> Ta)]
///     fn rule1(a: LogosToken<Token>) -> () { }
/// }
/// ```
///
/// Here `S` is the start symbol and `Token` is a lexer deriving `Logos`.
///
/// Each rule is of form `lhs -> rhs1 rhs2 ...`.
/// If you want to express `lhs -> Eps` rule, use `lhs -> `.
///
/// In each method, you can use `&self`, `&mut self` or simply omit it.
///
/// A type is associated with each `rhs` and return type is associated with `lhs`.
/// Every terminal uses `LogosToken<Token>` type.
/// Each nonterminal uses one custom type which derives `Clone` through out the grammar.
///
/// The generated `Parser::parse` function has the same return type as start symbol.
#[proc_macro_attribute]
pub fn gll(
    attr: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as ItemImpl);
    gen::generate(attr, input)
}
