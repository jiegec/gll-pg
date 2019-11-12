use gll_pg_core::LogosToken;
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
    #[rule(S -> Ta)]
    fn s1(a: LogosToken<Token>) -> usize {
        1
    }
    #[rule(S -> S S)]
    fn s2(s1: usize, s2: usize) -> usize {
        s1 + s2
    }
    #[rule(S -> S S S)]
    fn s3(s1: usize, s2: usize, s3: usize) -> usize {
        s1 + s2 + s3
    }
}

check_output! {eps, "", []}
check_output! {one, "a", [1]}
check_output! {two, "aa", [2]}
// 2,2: 2
// 3: 1
check_output! {three, "aaa", [3, 3, 3]}
// 2,3: 2
// 3,2: 3
// 2,2,2: 4
check_output! {four, "aaaa", [4, 4, 4, 4, 4, 4, 4, 4, 4]}
