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
    fn s1(l: i32, _op: Token, r: i32) -> i32 {
        l + r
    }
    #[rule(S -> B S)]
    fn s2(l: i32, _op: Token, r: i32) -> i32 {
        l - r
    }
    #[rule(S -> )]
    fn s3(l: i32, _op: Token, r: i32) -> i32 {
        l * r
    }
    #[rule(A -> Ta)]
    fn a1(l: i32, _op: Token, r: i32) -> i32 {
        l / r
    }
    #[rule(A -> Tc)]
    fn a2(l: i32, _op: Token, r: i32) -> i32 {
        l / r
    }
    #[rule(B -> Ta)]
    fn b1(l: i32, _op: Token, r: i32) -> i32 {
        l % r
    }
    #[rule(B -> Tb)]
    fn b2(_op: Token, r: i32) -> i32 {
        -r
    }
}

#[test]
fn gll() {
    let mut lexer = Token::lexer("aabd");
    Parser::parse(&mut lexer);
}
