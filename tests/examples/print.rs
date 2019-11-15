//! Print the GSS and SPPF graph

use gll_pg_core::*;
use gll_pg_macros::gll;
use logos::Logos;
use std::fs::File;
use std::io::Write;

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

#[derive(Default)]
struct Parser {}

#[gll(S, Token)]
impl Parser {
    #[rule(S -> A S Td)]
    fn s1(_a: &(), _s: &(), _d: &LogosToken<Token>) {}
    #[rule(S -> B S)]
    fn s2(_b: &(), _s: &()) {}
    #[rule(S -> )]
    fn s3() {}
    #[rule(A -> Ta)]
    fn a1(_a: &LogosToken<Token>) {}
    #[rule(A -> Tc)]
    fn a2(_c: &LogosToken<Token>) {}
    #[rule(B -> Ta)]
    fn b1(_a: &LogosToken<Token>) {}
    #[rule(B -> Tb)]
    fn b2(_b: &LogosToken<Token>) {}
}

fn main() {
    let mut lexer = Token::lexer("aabd");
    let mut parser = Parser {};
    let res = parser.parse(&mut lexer).unwrap();
    write!(File::create("gss.dot").unwrap(), "{}", res.print_gss_dot()).unwrap();
    write!(
        File::create("sppf.dot").unwrap(),
        "{}",
        res.print_sppf_dot()
    )
    .unwrap();
}
