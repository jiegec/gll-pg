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
    #[token = "if"]
    If,
    #[token = "else"]
    Else,
}

#[derive(Default)]
struct Parser {}

#[derive(Clone, Debug, Eq, PartialEq)]
enum S {
    Eps,
    If(Box<S>),
    IfElse(Box<S>, Box<S>),
}

#[gll(S, Token)]
impl Parser {
    #[rule(S ->)]
    fn s1() -> S {
        S::Eps
    }
    #[rule(S -> If S Else S)]
    fn s2(_: &LogosToken<Token>, s1: &S, _: &LogosToken<Token>, s2: &S) -> S {
        S::IfElse(Box::new(s1.clone()), Box::new(s2.clone()))
    }
    #[rule(S -> If S)]
    fn s3(_: &LogosToken<Token>, s1: &S) -> S {
        S::If(Box::new(s1.clone()))
    }
}

check_output! {eps, "", [S::Eps]}
check_output! {one, "if else", [S::IfElse(Box::new(S::Eps), Box::new(S::Eps))]}
check_output! {two, "if if else", [S::If(Box::new(S::IfElse(Box::new(S::Eps), Box::new(S::Eps)))), S::IfElse(Box::new(S::If(Box::new(S::Eps))), Box::new(S::Eps))]}
