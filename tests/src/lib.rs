#![feature(proc_macro_hygiene)]
#![allow(duplicate_macro_exports)]
#![allow(dead_code)]

macro_rules! check_output {
    ($name:ident,$s:tt,$r:expr) => {
        #[test]
        fn $name() {
            let mut lexer = Token::lexer($s);
            let mut parser = Parser::default();
            let res = parser.parse(&mut lexer).unwrap();
            let res: Vec<_> = res.cloned().collect();
            let mut expected = Vec::new();
            expected.extend($r.iter().cloned());
            assert_eq!(res, expected);
        }
    };
}

macro_rules! check_error {
    ($name:ident,$s:tt) => {
        #[test]
        fn $name() {
            let mut lexer = Token::lexer($s);
            let mut parser = Parser::default();
            assert!(parser.parse(&mut lexer).is_none());
        }
    };
}

pub mod arith;
pub mod crazy;
pub mod if_else;
pub mod left_recursive;
pub mod paper;
