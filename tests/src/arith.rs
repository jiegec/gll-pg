//! This example is taken from MashPlant/lalr1

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

#[derive(Default)]
struct Parser {
    literals: Vec<i32>,
}

#[gll(Expr, Token)]
#[verbose]
impl Parser {
    // you can omit self
    #[rule(Expr -> Expr Add Expr)]
    fn expr_add(l: &i32, _op: &LogosToken<Token>, r: &i32) -> i32 {
        *l + *r
    }
    // you can use &self
    #[rule(Expr -> Expr Sub Expr)]
    fn expr_sub(&self, l: &i32, _op: &LogosToken<Token>, r: &i32) -> i32 {
        *l - *r
    }
    // you can use &mut self as well
    // but all of these have &mut self in fact
    #[rule(Expr -> Expr Mul Expr)]
    fn expr_mul(&mut self, l: &i32, _op: &LogosToken<Token>, r: &i32) -> i32 {
        *l * *r
    }
    #[rule(Expr -> Expr Div Expr)]
    fn expr_div(l: &i32, _op: &LogosToken<Token>, r: &i32) -> i32 {
        *l / *r
    }
    #[rule(Expr -> Expr Mod Expr)]
    fn expr_mod(l: &i32, _op: &LogosToken<Token>, r: &i32) -> i32 {
        *l % *r
    }
    #[rule(Expr -> Sub Expr)]
    fn expr_neg(_op: &LogosToken<Token>, r: &i32) -> i32 {
        -*r
    }
    #[rule(Expr -> LPar Expr RPar)]
    fn expr_paren(_l: &LogosToken<Token>, i: &i32, _r: &LogosToken<Token>) -> i32 {
        *i
    }
    // so you can make your IDE happy with &mut self here
    #[rule(Expr -> IntLit)]
    fn expr_int(&mut self, i: &LogosToken<Token>) -> i32 {
        let lit = i.slice.parse().unwrap();
        self.literals.push(lit);
        lit
    }
}

#[test]
fn ambiguous() {
    let mut lexer = Token::lexer("1 + 2 + 3");
    let mut parser = Parser { literals: vec![] };
    let res = parser.parse(&mut lexer).unwrap();
    // two ways to parse
    let res: Vec<_> = res.cloned().collect();
    assert_eq!(res, vec![6, 6]);
}

check_output! {unambiguous, "1 + (2 * -3)", [-5i32]}
