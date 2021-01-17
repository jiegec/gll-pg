//! This example is taken from the original paper

use gll_pg_core::*;
use gll_pg_macros::gll;
use logos::Logos;

#[derive(Logos, Debug, Eq, PartialEq, Clone)]
pub enum Token {
    End,
    #[error]
    Error,
    #[token(" ")]
    _Eps,
    #[token("a")]
    Ta,
    #[token("b")]
    Tb,
    #[token("c")]
    Tc,
    #[token("d")]
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

#[derive(Default)]
struct Parser {
    history: Vec<A>,
}

#[gll(S, Token)]
impl Parser {
    #[rule(S -> A S Td)]
    fn s1(a: &A, s: &S, _d: &LogosToken<Token>) -> S {
        S::ASd(a.clone(), Box::new(s.clone()))
    }
    #[rule(S -> B S)]
    fn s2(b: &B, s: &S) -> S {
        S::BS(b.clone(), Box::new(s.clone()))
    }
    #[rule(S -> )]
    fn s3() -> S {
        S::Eps
    }
    #[rule(A -> Ta)]
    fn a1(&mut self, _a: &LogosToken<Token>) -> A {
        self.history.push(A::A);
        A::A
    }
    #[rule(A -> Tc)]
    fn a2(&mut self, _c: &LogosToken<Token>) -> A {
        self.history.push(A::C);
        A::C
    }
    #[rule(B -> Ta)]
    fn b1(_a: &LogosToken<Token>) -> B {
        B::A
    }
    #[rule(B -> Tb)]
    fn b2(_b: &LogosToken<Token>) -> B {
        B::B
    }
}

#[test]
fn paper() {
    let mut lexer = Token::lexer("aabd");
    let mut parser = Parser {
        history: Vec::new(),
    };
    let res = parser.parse(&mut lexer).unwrap();
    let vec: Vec<S> = res.cloned().collect();
    // two ways to parse
    assert_eq!(
        vec.as_slice(),
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
    assert_eq!(parser.history.len(), 2);
}

check_output! {eps, "", [S::Eps]}
check_output! {single_b, "b", [S::BS(B::B, Box::new(S::Eps))]}
check_output! {single_a, "a", [S::BS(B::A, Box::new(S::Eps))]}
check_error! {single_d, "d"}
check_output! {ad, "ad", [S::ASd(A::A, Box::new(S::Eps))]}
