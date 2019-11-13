#![feature(proc_macro_hygiene)]
#![allow(duplicate_macro_exports)]
#![allow(dead_code)]

macro_rules! check_output {
    ($name:ident,$s:tt,$r:expr) => {
        #[test]
        fn $name() {
            let mut lexer = Token::lexer($s);
            let mut parser = Parser::default();
            let res = parser.parse(&mut lexer);
            assert_eq!(res, $r);
        }
    };
}

mod arith;
mod crazy;
mod if_else;
mod left_recursive;
mod paper;
