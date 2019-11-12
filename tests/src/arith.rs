//! This example is taken from MashPlant/lalr1

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

#[gll(Expr)]
impl Parser {
    #[rule(Expr -> Expr Add Expr)]
    fn expr_add(l: i32, _op: LogosToken<Token>, r: i32) -> i32 {
        l + r
    }
    #[rule(Expr -> Expr Sub Expr)]
    fn expr_sub(l: i32, _op: LogosToken<Token>, r: i32) -> i32 {
        l - r
    }
    #[rule(Expr -> Expr Mul Expr)]
    fn expr_mul(l: i32, _op: LogosToken<Token>, r: i32) -> i32 {
        l * r
    }
    #[rule(Expr -> Expr Div Expr)]
    fn expr_div(l: i32, _op: LogosToken<Token>, r: i32) -> i32 {
        l / r
    }
    #[rule(Expr -> Expr Mod Expr)]
    fn expr_mod(l: i32, _op: LogosToken<Token>, r: i32) -> i32 {
        l % r
    }
    #[rule(Expr -> Sub Expr)]
    fn expr_neg(_op: LogosToken<Token>, r: i32) -> i32 {
        -r
    }
    #[rule(Expr -> LPar Expr RPar)]
    fn expr_paren(_l: LogosToken<Token>, i: i32, _r: LogosToken<Token>) -> i32 {
        i
    }
    #[rule(Expr -> IntLit)]
    fn expr_int(i: LogosToken<Token>) -> i32 {
        i.slice.parse().unwrap()
    }
}

#[test]
fn gll() {
    let mut lexer = Token::lexer("1 + 2 * 3");
    let res = Parser::parse(&mut lexer);
    // two ways to parse
    assert_eq!(res, [7, 9]);
}
