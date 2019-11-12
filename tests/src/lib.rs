#![feature(proc_macro_hygiene)]

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
    #[token = "b"]
    Tb,
    #[token = "c"]
    Tc,
    #[token = "d"]
    Td,
}

#[gll(S)]
#[verbose]
impl Parser {
    #[rule(S -> A S Td)]
    fn s1(a: i32, s: i32, d: Token) -> i32 {
        a + s + 1
    }
    #[rule(S -> B S)]
    fn s2(b: i32, s: i32) -> i32 {
        b + s
    }
    #[rule(S -> )]
    fn s3() -> i32 {
        0
    }
    #[rule(A -> Ta)]
    fn a1(a: Token) -> i32 {
        1
    }
    #[rule(A -> Tc)]
    fn a2(c: Token) -> i32 {
        1
    }
    #[rule(B -> Ta)]
    fn b1(a: Token) -> i32 {
        1
    }
    #[rule(B -> Tb)]
    fn b2(b: Token) -> i32 {
        1
    }
}

#[test]
fn gll() {
    let mut lexer = Token::lexer("aabd");
    let res = Parser::parse(&mut lexer);
    // two ways to parse
    assert_eq!(res, [4, 4]);
}
