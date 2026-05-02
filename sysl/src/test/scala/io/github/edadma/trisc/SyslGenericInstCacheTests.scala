package io.github.edadma.trisc

class SyslGenericInstCacheTests extends SyslTestHelpers {

  // ===== Generic-instantiation cache cannot leak refinements across instances =====
  //
  // Background: `instantiateGenericNominalAlias` / `instantiateGenericStruct` /
  // `instantiateGenericEnum` cache by `(name, typeArgs)` where `typeArgs` is a
  // `List[SyslType]`. Two distinct entries can share the same MANGLED name
  // because `typeToMangled` deliberately strips effect annotations on `FuncType`
  // — both `Parser[(int)->int]` and `Parser[(int)->int #pure]` mangle to
  // `Parser_fni32Reti32`. The cache distinguishes them, but their per-mangled
  // bookkeeping (`genericAliasToTemplate`, `enumToTemplate`, `structToTemplate`)
  // is keyed only by the mangled name — so the second instantiation overwrites
  // the first's `concreteArgs` entry, and any later unifier lookup gets the
  // wrong binding.
  //
  // Symptom: the spec ran a Parser combinator chain
  //
  //   add_p() | sub_p()    where each builds (int,int)->int via ^^^
  //
  // and the `|` dispatch failed — the unifier bound `[A]` to the
  // `#pure`-tagged variant from a stale cache entry, which then didn't satisfy
  // the operand's actual `#unknown` underlying.
  //
  // Fix: keep the cache lookup as a template-identity gate and as the binding
  // source for phantom type parameters that don't appear in the underlying;
  // additionally, walk the actual NamedType's `underlying` (alias case) or the
  // actual struct/enum's fields/variants (struct/enum case) and unify each
  // template field/variant TypeAST against it, refining bindings via the
  // existing lattice-aware `mergeBindings`. Either source alone has a corner
  // it can't cover; together they're complete.

  // ===== Concrete reproducer from the spec — Parser chain through `^^^` and `|` =====

  "Parser[fn] | Parser[fn] dispatches when the chain went through ^^^-via-map" in {
    eval(
      """type Parser[A] = new (int) -> A
        |
        |trait Or[T]
        |    #operator("|")
        |    or_op(a: T, b: T) -> T
        |
        |impl[A] Or[Parser[A]]
        |    or_op(a: Parser[A], b: Parser[A]) -> Parser[A] = a
        |
        |trait MapTo[A, V, R]
        |    #operator("^^^")
        |    pmapto(a: A, v: V) -> R
        |
        |impl[A, B] MapTo[Parser[A], B, Parser[B]]
        |    pmapto(a: Parser[A], v: B) -> Parser[B] =
        |        map_p(a, (_: A) -> v)
        |
        |map_p[A, B](p: Parser[A], f: (A) -> B) -> Parser[B] =
        |    Parser[B]((_x: int) -> f(_x))
        |
        |p_int() -> Parser[int] = Parser[int]((_x: int) -> 0)
        |
        |add_p() -> Parser[(int, int) -> int] =
        |    p_int() ^^^ ((a: int, b: int) -> a + b)
        |
        |sub_p() -> Parser[(int, int) -> int] =
        |    p_int() ^^^ ((a: int, b: int) -> a - b)
        |
        |main() -> int
        |    val _ = add_p() | sub_p()
        |    0
        |""".stripMargin) shouldBe 0
  }

  // ===== Same shape with a generic enum in the underlying ===============

  "Parser[fn] over a generic enum payload survives the cache collision" in {
    // The exact parsyl shape: `Parser[A] = new (Inp) -> PR[A]`. PR's `Ok`
    // variant carries an `A`-typed field, so two PR instantiations with
    // different `A` (e.g. `(int,int)->int #pure` vs `(int,int)->int`) collide
    // in `enumToTemplate`. Pre-fix, the unifier picked up the stale entry's
    // `A` and the `|` dispatch failed.
    eval(
      """struct Inp
        |    offset: int
        |
        |enum PR[A]
        |    Ok(value: A, next: Inp)
        |    Err(at: Inp)
        |
        |type Parser[A] = new (Inp) -> PR[A]
        |
        |map[A, B](p: Parser[A], f: (A) -> B) -> Parser[B] =
        |    Parser[B]((i: Inp) ->
        |        p(i) match
        |            Ok(v, n) -> Ok(f(v), n)
        |            Err(n)   -> Err(n))
        |
        |trait Or[T]
        |    #operator("|")
        |    or_op(a: T, b: T) -> T
        |
        |impl[A] Or[Parser[A]]
        |    or_op(a: Parser[A], b: Parser[A]) -> Parser[A] = a
        |
        |trait MapTo[A, V, R]
        |    #operator("^^^")
        |    pmapto(a: A, v: V) -> R
        |
        |impl[A, B] MapTo[Parser[A], B, Parser[B]]
        |    pmapto(a: Parser[A], v: B) -> Parser[B] = map(a, (_: A) -> v)
        |
        |p_str() -> Parser[string] = Parser[string]((i: Inp) -> Err(i))
        |
        |add_p() -> Parser[(int, int) -> int] =
        |    p_str() ^^^ ((a: int, b: int) -> a + b)
        |
        |sub_p() -> Parser[(int, int) -> int] =
        |    p_str() ^^^ ((a: int, b: int) -> a - b)
        |
        |main() -> int
        |    val _ = add_p() | sub_p()
        |    0
        |""".stripMargin) shouldBe 0
  }

  // ===== Same shape — generic struct in the underlying =====

  "trait-impl over a generic struct survives the cache collision" in {
    // `Box[A]` instantiated with two effect-distinct `A`s collides in
    // `structToTemplate`; `Inj.inj((x: int) -> x)` then has to bind `[A]`
    // against the struct's actual fields, not the stale cache entry.
    eval(
      """struct Box[A]
        |    fn: (int) -> A
        |
        |trait Inj[A, R]
        |    inj(v: A) -> R
        |
        |impl[A] Inj[A, Box[A]]
        |    inj(v: A) -> Box[A] = Box[A]((_x: int) -> v)
        |
        |make_fn() -> Box[(int) -> int] =
        |    Inj.inj((x: int) -> x)
        |
        |main() -> int
        |    val _ = make_fn()
        |    0
        |""".stripMargin) shouldBe 0
  }

  // ===== Targeted regression — generic struct/enum dispatch with concrete A =====

  "generic struct: two distinct A's dispatch independently (no cache cross-talk)" in {
    // Box[int] and Box[string] mangle to different names; the refinement
    // walk picks per-instance A from the actual struct's `value` field.
    eval(
      """struct Box[A]
        |    value: A
        |
        |trait Show[T]
        |    showInt(x: T) -> int
        |
        |impl Show[Box[int]]
        |    showInt(x: Box[int]) -> int = x.value
        |
        |impl Show[Box[string]]
        |    showInt(x: Box[string]) -> int = 999
        |
        |main() -> int
        |    val a = Box(42)
        |    val b = Box("hi")
        |    Show.showInt(a) + Show.showInt(b)
        |""".stripMargin) shouldBe 1041
  }

  "non-generic struct dispatch unchanged" in {
    eval(
      """struct Vec2
        |    x: int
        |    y: int
        |
        |trait Norm[T]
        |    norm(v: T) -> int
        |
        |impl Norm[Vec2]
        |    norm(v: Vec2) -> int = v.x * v.x + v.y * v.y
        |
        |main() -> int
        |    val p = Vec2(3, 4)
        |    Norm.norm(p)
        |""".stripMargin) shouldBe 25
  }
}
