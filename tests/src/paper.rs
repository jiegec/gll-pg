//! This example is taken from the original paper

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
    #[token = "b"]
    Tb,
    #[token = "c"]
    Tc,
    #[token = "d"]
    Td,
}

#[derive(Clone, Debug, PartialEq)]
pub enum A {
    A,
    C,
}
#[derive(Clone, Debug, PartialEq)]
pub enum B {
    A,
    B,
}
#[derive(Clone, Debug, PartialEq)]
pub enum S {
    ASd(A, Box<S>),
    BS(B, Box<S>),
    Eps,
}

#[gll(S)]
impl Parser {
    #[rule(S -> A S Td)]
    fn s1(a: A, s: S, d: LogosToken<Token>) -> S {
        S::ASd(a, Box::new(s))
    }
    #[rule(S -> B S)]
    fn s2(b: B, s: S) -> S {
        S::BS(b, Box::new(s))
    }
    #[rule(S -> )]
    fn s3() -> S {
        S::Eps
    }
    #[rule(A -> Ta)]
    fn a1(a: LogosToken<Token>) -> A {
        A::A
    }
    #[rule(A -> Tc)]
    fn a2(c: LogosToken<Token>) -> A {
        A::C
    }
    #[rule(B -> Ta)]
    fn b1(a: LogosToken<Token>) -> B {
        B::A
    }
    #[rule(B -> Tb)]
    fn b2(b: LogosToken<Token>) -> B {
        B::B
    }
}

#[test]
fn gll() {
    let mut lexer = Token::lexer("aabd");
    let res = Parser::parse(&mut lexer);
    // two ways to parse
    assert_eq!(
        res,
        [
            S::BS(
                B::A,
                Box::new(S::ASd(A::A, Box::new(S::BS(B::B, Box::new(S::Eps)))))
            ),
            S::ASd(
                A::A,
                Box::new(S::BS(B::A, Box::new(S::BS(B::B, Box::new(S::Eps)))))
            )
        ]
    );
}
