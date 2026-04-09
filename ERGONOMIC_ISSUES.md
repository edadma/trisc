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

| # | Issue | Status |
|---|-------|--------|
| 1 | Generic type aliases | ✅ Done |
| 2 | Generic combinators | ✅ Done |
| 3 | `?` on Result | ✅ Done |
| 6 | Verbose match arms | ✅ Solved by #3 |
| 4 | Mutual recursion / def | ✅ Done |
| 7 | Method syntax on functions | Open — medium priority |
| 8 | Pattern match in `if` | Open — low priority |
| 5 | Expression-body ambiguity | ✅ Not an issue — parens make it unambiguous |
