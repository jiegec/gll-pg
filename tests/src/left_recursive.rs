use gll_pg_core::*;
use gll_pg_macros::gll;
use logos::Logos;

#[derive(Logos, Debug, Eq, PartialEq, Clone)]
pub enum Token {
    #[end]
    End,
    #[error]
    Error,
    #[token = " "]
    _Eps,
    #[token = "a"]
    Ta,
}

#[derive(Default)]
struct Parser {}

#[gll(S, Token)]
impl Parser {
    #[rule(S -> S Ta)]
    fn s1(s: &usize, a: &LogosToken<Token>) -> usize {
        *s + 1
    }
    #[rule(S ->)]
    fn s2() -> usize {
        0
    }
}

check_output! {eps, "", [0]}
check_output! {one, "a", [1]}
check_output! {two, "aa", [2]}
check_output! {three, "aaa", [3]}
