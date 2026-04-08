# Sysl Ergonomic Issues

Found via parser combinator exercise (`parser.sysl`).

## 1. No Generic Type Aliases

Can't write:
```sysl
type Parser[T] = (string, int) -> Result[T]
```

Every combinator repeats the full function type:
```sysl
map(p: (string, int) -> Result, f: (int) -> int) -> (string, int) -> Result
```

With a type alias:
```sysl
map(p: Parser, f: (int) -> int) -> Parser
```

## 2. No Generic Combinators

`map`/`seq`/`alt` must be specialized per result type. Can't write:
```sysl
map[A, B](p: Parser[A], f: (A) -> B) -> Parser[B]
```
because `Result[A]` and `Result[B]` are different monomorphized types. Currently must duplicate every combinator for each result type.

## 3. `?` Operator on Result

`parse_factor` could be much cleaner:
```sysl
val (v, p2) = parse_expr(input, pos + 1)?
```
Instead of:
```sysl
parse_expr(input, pos + 1) match
    Ok(v, p2) -> ...
    Fail(fp) -> Fail(fp)
```
The `Fail` propagation is pure boilerplate. `?` should work here since the enclosing function returns the same `Result` type. Needs testing.

## 4. Mutual Recursion Prevents Closure Composition

`expr` ↔ `factor` recursion means these must be named functions, not closures stored in variables:
```sysl
// Can't do this — factor references expr which isn't defined yet
val expr = seq(term, many(seq(char('+'), term)))
val factor = alt(nat(), between(char('('), expr, char(')')))
```

`val` initializers run top-to-bottom. Named functions work but lose composability.

Fix: `lazy val`, or allow forward references to functions in closures.

## 5. Expression-Body Ambiguity with `->` in Return Type

This doesn't parse as a block body:
```sysl
char(expected: byte) -> (string, int) -> Result[byte]
    char_if(ch -> ch == expected)
```
Must use `=` for expression body. The `->` in the return type confuses the parser about where the return type ends and the body begins.

## 6. Match Arms Verbose for Two-Variant Enums

Every parse result requires:
```sysl
p(input, pos) match
    Ok(v, p2) -> ...
    Fail(fp) -> Fail(fp)
```
The `Fail` arm is pure boilerplate. The `?` operator (issue #3) would eliminate this.

## 7. No Method Syntax on Function Types

Would love:
```sysl
parser.map(f).or(other).many()
```
Instead of:
```sysl
many(alt(map(parser, f), other))
```
Needs extension methods or a wrapper struct with methods.

## 8. No Pattern Matching in `if`

Would love:
```sysl
if p(input, pos) is Ok(v, p2) then ...
```
Instead of a full `match` block for simple Ok/Fail dispatch.

## Priority

| # | Issue | Impact |
|---|-------|--------|
| 1 | Generic type aliases | High — eliminates signature noise |
| 3 | `?` on Result | High — eliminates match boilerplate |
| 2 | Generic combinators | High — enables reusable abstractions |
| 4 | Mutual recursion / lazy val | Medium — enables combinator composition |
| 7 | Method syntax on functions | Medium — fluent API style |
| 8 | Pattern match in `if` | Low — convenience sugar |
| 5 | Expression-body ambiguity | Low — workaround exists (`=`) |
| 6 | Verbose match arms | Low — solved by #3 |
