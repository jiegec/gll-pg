# gll-pg

A Parser Generator for GLL parser. It uses Logos as lexer.

## Usage

Add to Cargo.toml:

```
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
    #[end]
    End, // must not change this
    #[error]
    Error,
    #[token = " "]
    _Eps, // must not change this, will be skipped
    #[token = "a"]
    Ta,
}
```

Use gll-pg to generate parser:

```rust
struct Parser {}

#[gll(S, Token)]
impl Parser {
    #[rule(S -> S Ta)]
    fn s1(s: usize, a: LogosToken<Token>) -> usize {
        s + 1
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
let res = parser.parse(&mut lexer);
assert_eq!(res, [3]);
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

For input "1 + 2 * 3", it gives [7, 9].
For input "1 + (2 * -3)", it gives [-5].

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
    fn s1(a: LogosToken<Token>) -> usize {
        1
    }
    #[rule(S -> S S)]
    fn s2(s1: usize, s2: usize) -> usize {
        s1 + s2
    }
    #[rule(S -> S S S)]
    fn s3(s1: usize, s2: usize, s3: usize) -> usize {
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


## References

1. Code generation and template is learned from `MashPlant/lalr1`.
2. Scott, E., & Johnstone, A. (2010). GLL parsing. Electronic Notes in Theoretical Computer Science, 253(7), 177-189.
3. Scott, E., & Johnstone, A. (2013). GLL parse-tree generation. Science of Computer Programming, 78(10), 1828-1844.
