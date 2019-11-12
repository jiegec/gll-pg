use logos;

#[derive(Clone, Eq, PartialEq)]
pub struct LogosToken<'a, T: logos::Logos> {
    pub kind: T,
    pub slice: &'a str,
}
