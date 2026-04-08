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
| 4 | Mutual recursion / lazy val | Open — medium priority |
| 7 | Method syntax on functions | Open — medium priority |
| 8 | Pattern match in `if` | Open — low priority |
| 5 | Expression-body ambiguity | Open — low priority, workaround exists (`=`) |
