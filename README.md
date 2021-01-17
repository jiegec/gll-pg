# gll-pg

A Parser Generator for GLL parser. It uses Logos as lexer.

## Usage

Add to Cargo.toml:

```toml
[dependencies]
gll-pg-macros = "0.2"
gll-pg-core = "0.2"
logos = "0.9"
```

Write a lexer using Logos:

```rust
use logos::Logos;

#[derive(Logos, Debug, Eq, PartialEq, Clone)]
pub enum Token {
    End, // must not change this
    #[error]
    Error,
    #[token(" ")]
    _Eps, // must not change this, will be skipped
    #[token("a")]
    Ta,
}
```

Use gll-pg to generate parser:

```rust
struct Parser {}

#[gll(S, Token)]
impl Parser {
    #[rule(S -> S Ta)]
    fn s1(s: &usize, _a: &LogosToken<Token>) -> usize {
        *s + 1
    }
    #[rule(S ->)]
    fn s2() -> usize {
        0
    }
}
```

Run it:

```rust
let mut lexer = Token::lexer("aaa");
let mut parser = Parser {};
let res = parser.parse(&mut lexer).unwrap();
// res is a StreamingIterator
let vec: Vec<usize> = res.cloned().collect();
assert_eq!(res, vec![3]);
```

## Example

### Ambiguous Calculator

For the grammar below:

```
E -> E + E
E -> E * E
E -> - E
E -> ( E )
E -> int
```

For input "1 + 2 \* 3", it gives [7, 9].
For input "1 + (2 \* -3)", it gives [-5].

See `tests/src/arith.rs` for detail.

### Recursive

It can handle recursive grammars:

```
S -> a
S -> S S
S -> S S S
```

Code snippet:

```rust
#[gll(S, Token)]
impl Parser {
    #[rule(S -> Ta)]
    fn s1(_a: &LogosToken<Token>) -> usize {
        1
    }
    #[rule(S -> S S)]
    fn s2(s1: &usize, s2: &usize) -> usize {
        s1 + s2
    }
    #[rule(S -> S S S)]
    fn s3(s1: &usize, s2: &usize, s3: &usize) -> usize {
        s1 + s2 + s3
    }
}

// "" gives []
// "a" gives [1]
// "aa" gives [2]
// "aaa" gives [3,3,3]
// "aaaa" gives [4, 4, 4, 4, 4, 4, 4, 4, 4]
```

See `tests/src/crazy.rs` for detail.

## Changelog

### Release 0.3.0 2019-11-15

1. Return a struct that impls StreamingIterator to give derivations lazily.
2. Implement memoization for shared structure of derivations.
3. Turn all clone calls to reference to arena objects.
4. Implement diagnostics to catch type mismatch errors.
5. Make errors in user code reported with correct line numbers.

### Release 0.2.1 2019-11-13

1. Improve documentation.

### Release 0.2.0 2019-11-13

1. Allow access to struct fields.

### Release 0.1.0 2019-11-12

1. Initial working version.
2. The parser recursively clone & collect all possible derivations.

## References

1. Code generation and template is learned from `MashPlant/lalr1`.
2. Scott, E., & Johnstone, A. (2010). GLL parsing. Electronic Notes in Theoretical Computer Science, 253(7), 177-189.
3. Scott, E., & Johnstone, A. (2013). GLL parse-tree generation. Science of Computer Programming, 78(10), 1828-1844.
