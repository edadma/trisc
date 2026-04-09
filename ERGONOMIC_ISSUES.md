# Sysl Ergonomic Issues

Found via parser combinator exercise (`parser.sysl`).

## ~~1. No Generic Type Aliases~~ ✅ DONE

Works:
```sysl
type Parser[T] = (string, int) -> Result[T]

apply(f: Transform[int], x: int) -> int = f(x)
```
Generic type aliases resolve through `typeEnv` substitution. Tested in `parser.sysl` and `SyslTypeAliasTests`.

## ~~2. No Generic Combinators~~ ✅ DONE

Works:
```sysl
map[A, B](p: Parser[A], f: (A) -> B) -> Parser[B]
    (input: string, pos: int) ->
        p(input, pos) match
            Ok(v, p2) -> Ok(f(v), p2)
            Fail(p2) -> Fail(p2)
```
Required fix: `unifyTypes` now expands generic type aliases before structural unification, so type parameters can be inferred through aliases. Tested in `parser.sysl` and `SyslTypeAliasTests`.

## ~~3. `?` Operator on Result~~ ✅ DONE

Works:
```sysl
parse_factor(input: string, pos: int) -> Result[int]
    val v, p2 = parse_expr(input, pos + 1)?
    Ok(v, p2 + 1)
```
`?` propagates the `Fail` variant automatically. Used throughout `parser.sysl`.

## ~~4. Mutual Recursion Prevents Closure Composition~~ ✅ DONE

Solved with `def` — zero-argument auto-call functions (like Scala's `def` without parens):
```sysl
def expr = seq(term, many(seq(char('+'), term)))
def factor = alt(nat(), between(char('('), expr, char(')')))
```
`def` functions are forward-declared like regular functions, so mutual recursion works. Bare `name` auto-calls; `&name` gives the function pointer. Return type is inferred from the expression body. `def` is also allowed (documentary) on functions with parameters.

## ~~5. Expression-Body Ambiguity with `->` in Return Type~~ ✅ Not an issue

Works correctly:
```sysl
char_exact(expected: int) -> (string, int) -> Result[int]
    (input: string, pos: int) ->
        if pos < len(input) && input[pos] == expected then Ok(expected, pos + 1)
        else Fail(pos)
```
Function types require parens around params (`(string, int) -> Result[T]`), so `funcTypeRef` unambiguously consumes the return type. Generic type aliases (`-> Parser[int]`) make this a non-issue in practice.

## ~~6. Match Arms Verbose for Two-Variant Enums~~ ✅ Solved by #3

The `?` operator eliminates the boilerplate `Fail` propagation arm.

## ~~7. No Method Syntax on Function Types~~ ✅ DONE

Works via wrapper struct with methods:
```sysl
struct P
    run: Parser[int]

P.map_to(f: (int) -> int) -> P = P(map(self.run, f))
P.or(other: P) -> P = P(alt(self.run, other.run))
P.parse(input: string, pos: int) -> Result[int] = self.run(input, pos)

// Fluent chaining:
var n = p_nat
var neg = n.map_to(negate)
var q = p_char(int('?'))
var p = neg.or(q)
p.parse("7", 0)  // Ok(-7, 1)
```
Required fixing an interpreter bug: `TFieldAccess` on pointer-typed objects
now auto-dereferences. See `parser.sysl` for full fluent API with tests.

## ~~8. No Pattern Matching in `if`~~ ✅ DONE

Works:
```sysl
if p(input, pos) is Ok(v, p2) then v else -1
```
Desugars to a `match` expression at parse time. Supports inline and block
bodies, optional `else`, and all match pattern types (destructure, wildcard,
value). `is` is a new keyword.

## Priority

| # | Issue | Status |
|---|-------|--------|
| 1 | Generic type aliases | ✅ Done |
| 2 | Generic combinators | ✅ Done |
| 3 | `?` on Result | ✅ Done |
| 6 | Verbose match arms | ✅ Solved by #3 |
| 4 | Mutual recursion / def | ✅ Done |
| 7 | Method syntax on functions | ✅ Done — wrapper struct with methods |
| 8 | Pattern match in `if` | ✅ Done — `if expr is Pattern then ...` |
| 5 | Expression-body ambiguity | ✅ Not an issue — parens make it unambiguous |
