#![feature(proc_macro_hygiene)]

use logos::Logos;
use gll_pg_macros::gll;

#[derive(Logos, Debug, Eq, PartialEq, Clone)]
pub enum Token {
    #[end]
    End,
    #[error]
    Error,
    #[token = " "]
    _Eps,
    #[token = "+"]
    Add,
    #[token = "-"]
    Sub,
    #[token = "*"]
    Mul,
    #[token = "/"]
    Div,
    #[token = "%"]
    Mod,
    #[token = "("]
    LPar,
    #[token = ")"]
    RPar,
    #[regex = "[0-9]+"]
    IntLit,
}

#[gll(S)]
#[verbose]
impl Parser {
    #[rule(S -> A S d)]
    fn s1(l: i32, _op: Token, r: i32) -> i32 { l + r }
    #[rule(S -> B S)]
    fn s2(l: i32, _op: Token, r: i32) -> i32 { l - r }
    #[rule(S -> )]
    fn s3(l: i32, _op: Token, r: i32) -> i32 { l * r }
    #[rule(A -> a)]
    fn a1(l: i32, _op: Token, r: i32) -> i32 { l / r }
    #[rule(A -> c)]
    fn a2(l: i32, _op: Token, r: i32) -> i32 { l / r }
    #[rule(B -> a)]
    fn b1(l: i32, _op: Token, r: i32) -> i32 { l % r }
    #[rule(B -> b)]
    fn b2(_op: Token, r: i32) -> i32 { -r }
}

#[test]
fn gll() {
    let mut lexer = Token::lexer("1 - 2 * (3 + 4 * 5 / 6) + -7 * -9 % 10");
    assert_eq!(Parser::parse(&mut lexer), Some(-8));
}
