package io.github.edadma.trisc

class SyslExtensionTests extends SyslTestHelpers {

  "extension dispatch" - {

    "extension on user struct: receiver in scope, method callable via dot" in {
      eval(
        """struct Box
          |    v: int
          |
          |extension (b: Box)
          |    def doubled(extra: int) -> int = b.v * 2 + extra
          |
          |main() -> int
          |    bx = Box(5)
          |    bx.doubled(3)
          |""".stripMargin) shouldBe 13
      // 5 * 2 + 3 = 13
    }

    "extension on i32 (non-struct receiver, the headline ergonomics win)" in {
      eval(
        """extension (x: i32)
          |    def plus_one_then_double -> i32 = (x + 1) * 2
          |
          |main() -> int
          |    n = 7
          |    n.plus_one_then_double
          |""".stripMargin) shouldBe 16
      // (7 + 1) * 2 = 16
    }

    "extension method missing — error names receiver and method (no silent dispatch)" in {
      val ex = intercept[Exception] {
        eval(
          """main() -> int
            |    n = 7
            |    n.no_such_method
            |""".stripMargin)
      }
      val msg = ex.getMessage.toLowerCase
      assert(msg.contains("no_such_method") || msg.contains("cannot call method"),
        s"expected dispatch error, got: ${ex.getMessage}")
    }

    "two extensions of the same method on different types coexist" in {
      eval(
        """extension (x: i32)
          |    def kind -> i32 = 1
          |
          |extension (s: string)
          |    def kind -> i32 = 2
          |
          |main() -> int
          |    a = (5).kind
          |    b = "hi".kind
          |    a * 10 + b
          |""".stripMargin) shouldBe 12
    }

    "real struct method takes precedence over extension with same name" in {
      eval(
        """struct Box
          |    v: int
          |
          |Box_get(self: *Box) -> int = self.v
          |
          |extension (b: Box)
          |    def get -> int = 999
          |
          |main() -> int
          |    bx = Box(7)
          |    bx.get
          |""".stripMargin) shouldBe 7
      // Real method `Box_get` wins; extension never fires.
    }
  }

  "cross-module extension visibility (Phase 2b)" - {

    "wildcard import brings extension into scope" in {
      val libs = Map(
        "mylib/strings" ->
          """module mylib
            |
            |extension (x: i32)
            |    def doubled -> i32 = x * 2
            |""".stripMargin)
      evalWithLibs(libs,
        """import mylib.*
          |
          |main() -> int
          |    n = 21
          |    n.doubled
          |""".stripMargin) shouldBe 42
    }

    "named import also brings extensions (Scala-3 style)" in {
      val libs = Map(
        "mylib/x" ->
          """module mylib
            |
            |add_one(n: i32) -> i32 = n + 1
            |
            |extension (x: i32)
            |    def tripled -> i32 = x * 3
            |""".stripMargin)
      evalWithLibs(libs,
        """import mylib.{add_one}
          |
          |main() -> int
          |    n = 5
          |    n.tripled + add_one(0)
          |""".stripMargin) shouldBe 16
      // 5 * 3 + 1 = 16
    }

    "Predef auto-import covers multiple primitive owners simultaneously" in {
      // Both std.string and std.int are Predef owners. With both present in
      // the source set and neither explicitly imported, extensions on string
      // and i32 should both dispatch.
      val libs = Map(
        "std/string/string" ->
          """module std.string
            |
            |extension (s: string)
            |    def tag -> i32 = 7
            |""".stripMargin,
        "std/int/int" ->
          """module std.int
            |
            |extension (x: i32)
            |    def tag -> i32 = 11
            |""".stripMargin)
      evalWithLibs(libs,
        """main() -> int
          |    a = "hi".tag
          |    b = (5).tag
          |    a * 100 + b
          |""".stripMargin) shouldBe 711
    }

    "Predef auto-import is silent when std modules are absent (no error)" in {
      // No std/string or std/int modules in the source set — auto-Predef must
      // skip silently. Extensions defined locally still dispatch.
      eval(
        """extension (s: string)
          |    def hi_len -> i32 = 99
          |
          |main() -> int
          |    "x".hi_len
          |""".stripMargin) shouldBe 99
    }

    "extension on string receiver dispatches across modules without explicit import (Predef auto-import)" in {
      // The driver auto-injects a wildcard import for every Predef module
      // (std.string, std.int, std.float, std.bool) that exists in the source
      // set. So `"hi".double_it` works with no `import` statement at all,
      // mirroring Scala 3's `Predef`.
      val libs = Map(
        "std/string/string" ->
          """module std.string
            |
            |extension (s: string)
            |    def double_it -> i32 = 4
            |""".stripMargin)
      evalWithLibs(libs,
        """main() -> int
          |    "hi".double_it
          |""".stripMargin) shouldBe 4
    }

    "extension defined in unimported module is not dispatched" in {
      val libs = Map(
        "otherlib/x" ->
          """module otherlib
            |
            |extension (x: i32)
            |    def hidden -> i32 = 999
            |""".stripMargin)
      val ex = intercept[Exception] {
        evalWithLibs(libs,
          """main() -> int
            |    n = 7
            |    n.hidden
            |""".stripMargin)
      }
      val msg = ex.getMessage.toLowerCase
      assert(msg.contains("hidden") || msg.contains("cannot call method"),
        s"expected dispatch error, got: ${ex.getMessage}")
    }
  }

  "operator extensions (Phase 2c)" - {

    "extension cannot override a built-in operator (use impl Add[T] instead)" in {
      val ex = intercept[Exception] {
        eval(
          """struct Vec
            |    x: int
            |    y: int
            |
            |extension (a: Vec)
            |    #operator("+")
            |    def add(b: Vec) -> Vec = Vec(a.x + b.x, a.y + b.y)
            |
            |main() -> int = 0
            |""".stripMargin)
      }
      ex.getMessage.toLowerCase should include("reserved for built-in")
    }

    "non-built-in binary #operator on extension" in {
      eval(
        """struct Bag
          |    n: int
          |
          |extension (a: Bag)
          |    #operator("<>")
          |    def merge(b: Bag) -> Bag = Bag(a.n + b.n)
          |
          |main() -> int
          |    x = Bag(7)
          |    y = Bag(35)
          |    z = x <> y
          |    z.n
          |""".stripMargin) shouldBe 42
    }

    "prefix #operator on extension dispatches" in {
      eval(
        """struct Bag
          |    n: int
          |
          |extension (a: Bag)
          |    #operator("~~")
          |    def flip -> Bag = Bag(0 - a.n)
          |
          |main() -> int
          |    x = Bag(7)
          |    y = ~~x
          |    y.n
          |""".stripMargin) shouldBe -7
    }

    "cross-module operator extension" in {
      val libs = Map(
        "veclib/vec" ->
          """module veclib
            |
            |struct V
            |    n: int
            |
            |extension (a: V)
            |    #operator("<>")
            |    def merge(b: V) -> V = V(a.n + b.n)
            |""".stripMargin)
      evalWithLibs(libs,
        """import veclib.*
          |
          |main() -> int
          |    a = V(11)
          |    b = V(31)
          |    c = a <> b
          |    c.n
          |""".stripMargin) shouldBe 42
    }
  }

  "generic extensions (Phase 2d)" - {

    "extension on []T dispatches with T inferred from receiver" in {
      eval(
        """extension [T](xs: []T)
          |    def head_at(i: int) -> T = xs[i]
          |
          |main() -> int
          |    arr: [3]int
          |    arr[0] = 11
          |    arr[1] = 22
          |    arr[2] = 33
          |    s = arr[:]
          |    s.head_at(1)
          |""".stripMargin) shouldBe 22
    }

    "generic extension on a parameterized struct" in {
      eval(
        """struct Box[T]
          |    v: T
          |
          |extension [T](b: Box[T])
          |    def get -> T = b.v
          |
          |main() -> int
          |    bx = Box(42)
          |    bx.get
          |""".stripMargin) shouldBe 42
    }

    "two generic extensions with different receiver shapes coexist" in {
      eval(
        """struct Box[T]
          |    v: T
          |
          |extension [T](b: Box[T])
          |    def kind -> int = 1
          |
          |extension [T](xs: []T)
          |    def kind -> int = 2
          |
          |main() -> int
          |    arr: [2]int
          |    arr[0] = 7
          |    arr[1] = 9
          |    s = arr[:]
          |    bx = Box(99)
          |    a = bx.kind
          |    b = s.kind
          |    a * 10 + b
          |""".stripMargin) shouldBe 12
    }

    "cross-module generic extension" in {
      val libs = Map(
        "vlib/v" ->
          """module vlib
            |
            |extension [T](xs: []T)
            |    def at(i: int) -> T = xs[i]
            |""".stripMargin)
      evalWithLibs(libs,
        """import vlib.*
          |
          |main() -> int
          |    arr: [3]int
          |    arr[0] = 17
          |    arr[1] = 99
          |    arr[2] = 5
          |    s = arr[:]
          |    s.at(2) * 100 + s.at(0)
          |""".stripMargin) shouldBe 517
    }
  }

  "generic operator extensions (Phase 2d)" - {

    "binary #operator on a generic-receiver extension on []T" in {
      eval(
        """extension [T](xs: []T)
          |    #operator("<>")
          |    def merge(ys: []T) -> []T = xs
          |
          |main() -> int
          |    arr: [3]int
          |    arr[0] = 7
          |    arr[1] = 14
          |    arr[2] = 21
          |    a = arr[:]
          |    b = arr[:]
          |    c = a <> b
          |    c[1]
          |""".stripMargin) shouldBe 14
    }

    "prefix #operator on a generic-receiver extension on parameterized struct" in {
      eval(
        """struct Box[T]
          |    v: T
          |
          |extension [T](b: Box[T])
          |    #operator("~~")
          |    def flip -> Box[T] = b
          |
          |main() -> int
          |    bx = Box(99)
          |    cx = ~~bx
          |    cx.v
          |""".stripMargin) shouldBe 99
    }

    "cross-module generic operator extension" in {
      val libs = Map(
        "veclib/vec" ->
          """module veclib
            |
            |extension [T](xs: []T)
            |    #operator("<>")
            |    def merge(ys: []T) -> []T = xs
            |""".stripMargin)
      evalWithLibs(libs,
        """import veclib.*
          |
          |main() -> int
          |    arr: [3]int
          |    arr[0] = 11
          |    arr[1] = 22
          |    arr[2] = 33
          |    a = arr[:]
          |    b = arr[:]
          |    c = a <> b
          |    c[2]
          |""".stripMargin) shouldBe 33
    }
  }

  "bare-form extension methods (impure)" - {

    "bare form on string receiver can call impure helper" in {
      // `shout` is bare (no `def`), so it's impure-by-default and can call
      // panic. The `def` form would reject the call to panic via validatePureFn.
      eval(
        """extension (s: string)
          |    shout -> int =
          |        if len(s) == 0 then panic("empty")
          |        len(s)
          |
          |main() -> int
          |    "hello".shout
          |""".stripMargin) shouldBe 5
    }

    "def form still rejects impure callee (regression)" in {
      val ex = intercept[Exception] {
        eval(
          """extension (s: string)
            |    def shout -> int =
            |        if len(s) == 0 then panic("empty")
            |        len(s)
            |
            |main() -> int
            |    "hello".shout
            |""".stripMargin)
      }
      ex.getMessage.toLowerCase should (include("pure") or include("impure"))
    }

    "mixed pure + impure methods in one block both dispatch" in {
      eval(
        """extension (s: string)
          |    def quiet -> int = len(s)
          |    shout -> int =
          |        if len(s) == 0 then panic("empty")
          |        len(s) * 10
          |
          |main() -> int
          |    "hi".quiet * 100 + "hello".shout
          |""".stripMargin) shouldBe 250
      // 2 * 100 + 5 * 10 = 250
    }

    "bare form with #operator" in {
      eval(
        """struct Bag
          |    n: int
          |
          |extension (a: Bag)
          |    #operator("<>")
          |    merge(b: Bag) -> Bag =
          |        if a.n < 0 then panic("negative")
          |        Bag(a.n + b.n)
          |
          |main() -> int
          |    x = Bag(7)
          |    y = Bag(35)
          |    z = x <> y
          |    z.n
          |""".stripMargin) shouldBe 42
    }

    "bare form generic-receiver extension" in {
      eval(
        """extension [T](xs: []T)
          |    at(i: int) -> T =
          |        if i < 0 then panic("negative index")
          |        xs[i]
          |
          |main() -> int
          |    arr: [3]int
          |    arr[0] = 11
          |    arr[1] = 22
          |    arr[2] = 33
          |    s = arr[:]
          |    s.at(1)
          |""".stripMargin) shouldBe 22
    }

    "cross-module bare-form extension" in {
      val libs = Map(
        "shoutlib/shout" ->
          """module shoutlib
            |
            |extension (s: string)
            |    shout -> int =
            |        if len(s) == 0 then panic("empty")
            |        len(s)
            |""".stripMargin)
      evalWithLibs(libs,
        """import shoutlib.*
          |
          |main() -> int
          |    "hello".shout
          |""".stripMargin) shouldBe 5
    }
  }

  "same-module sibling extension visibility" - {

    // Helper: compile + run a multi-file source set with no top-level "test" file.
    // evalWithLibs forces a no-module-decl "test" key, which collides with the
    // same-module-sibling case where every file declares the same module.
    def runMultiFile(sources: Map[String, String]): Long =
      val driver = new SyslDriver
      val result = driver.compile(sources)
      val merged = TProgram(result.units.flatMap(_.typed.decls))
      val interp = new SyslInterpreter()
      interp.run(merged)

    "non-generic extension declared in sibling file dispatches without import" in {
      runMultiFile(Map(
        "sib/lib" ->
          """module sib
            |
            |extension (s: string)
            |    tag -> int = 7
            |""".stripMargin,
        "sib/main" ->
          """module sib
            |
            |main() -> int
            |    "hi".tag
            |""".stripMargin,
      )) shouldBe 7
    }

    "generic-receiver extension declared in sibling file dispatches without import" in {
      runMultiFile(Map(
        "sib/lib" ->
          """module sib
            |
            |extension [T](xs: []T)
            |    head_or_zero -> T = xs[0]
            |""".stripMargin,
        "sib/main" ->
          """module sib
            |
            |main() -> int
            |    val xs: [3]int = [10, 20, 30]
            |    xs[:].head_or_zero
            |""".stripMargin,
      )) shouldBe 10
    }

    "operator extension declared in sibling file dispatches via operator" in {
      runMultiFile(Map(
        "sib/lib" ->
          """module sib
            |
            |struct Bag
            |    n: int
            |
            |extension (a: Bag)
            |    #operator("<>")
            |    merge(b: Bag) -> Bag = Bag(a.n + b.n)
            |""".stripMargin,
        "sib/main" ->
          """module sib
            |
            |main() -> int
            |    val a = Bag(3)
            |    val b = Bag(4)
            |    val c = a <> b
            |    c.n
            |""".stripMargin,
      )) shouldBe 7
    }

    "sibling extension dispatch with both files importing the same external module" in {
      runMultiFile(Map(
        "shoutlib/shout" ->
          """module shoutlib
            |
            |loud(s: string) -> int = len(s) * 2
            |""".stripMargin,
        "sib/lib" ->
          """module sib
            |
            |import shoutlib.*
            |
            |extension (s: string)
            |    boom -> int = loud(s)
            |""".stripMargin,
        "sib/main" ->
          """module sib
            |
            |import shoutlib.*
            |
            |main() -> int
            |    "abc".boom
            |""".stripMargin,
      )) shouldBe 6
    }
  }

  "same-module sibling trait impl visibility" - {

    // Same helper as above — multi-file no-test-key.
    def runMultiFile(sources: Map[String, String]): Long =
      val driver = new SyslDriver
      val result = driver.compile(sources)
      val merged = TProgram(result.units.flatMap(_.typed.decls))
      val interp = new SyslInterpreter()
      interp.run(merged)

    "non-generic multi-target operator impl declared in sibling file dispatches" in {
      runMultiFile(Map(
        "sib/lib" ->
          """module sib
            |
            |struct Box
            |    v: int
            |
            |trait Add[A, B, R]
            |    #operator("|+|")
            |    add(a: A, b: B) -> R
            |
            |impl Add[Box, Box, Box]
            |    add(a: Box, b: Box) -> Box = Box(a.v + b.v)
            |""".stripMargin,
        "sib/main" ->
          """module sib
            |
            |main() -> int
            |    val a = Box(1)
            |    val b = Box(2)
            |    val c = a |+| b
            |    c.v
            |""".stripMargin,
      )) shouldBe 3
    }

    "generic operator impl declared in sibling file dispatches" in {
      runMultiFile(Map(
        "sib/lib" ->
          """module sib
            |
            |struct Wrap[T]
            |    v: T
            |
            |trait Map[A, F, R]
            |    #operator("^^")
            |    pmap(a: A, f: F) -> R
            |
            |impl[A, B] Map[Wrap[A], (A) -> B, Wrap[B]]
            |    pmap(a: Wrap[A], f: (A) -> B) -> Wrap[B] = Wrap[B](f(a.v))
            |""".stripMargin,
        "sib/main" ->
          """module sib
            |
            |main() -> int
            |    val w = Wrap[int](7)
            |    val r = w ^^ ((n: int) -> n + 1)
            |    r.v
            |""".stripMargin,
      )) shouldBe 8
    }

    "non-operator trait impl declared in sibling file dispatches via direct call" in {
      // Trait method dispatch (no operator) from a sibling-declared concrete impl.
      // Calls the impl method through its mangled name route via the trait
      // dispatch path — exercises the same machinery as operator dispatch but
      // through the explicit method-name surface.
      runMultiFile(Map(
        "sib/lib" ->
          """module sib
            |
            |struct Box
            |    v: int
            |
            |trait Show[T]
            |    show(t: T) -> int
            |
            |impl Show[Box]
            |    show(t: Box) -> int = t.v + 100
            |""".stripMargin,
        "sib/main" ->
          """module sib
            |
            |main() -> int
            |    val b = Box(7)
            |    Show.show(b)
            |""".stripMargin,
      )) shouldBe 107
    }

    "cross-module precedence still works after sibling-impl visibility lands" in {
      // Regression: same-module sibling visibility must not accidentally
      // unify cross-module impls. Importing module A's wildcard should still
      // pull in A's impl normally; the sibling-merge path is keyed on the
      // current module's other source files, not on every imported module.
      runMultiFile(Map(
        "alib/types" ->
          """module alib
            |
            |struct Box
            |    v: int
            |
            |trait Add[A, B, R]
            |    #operator("|+|")
            |    add(a: A, b: B) -> R
            |
            |impl Add[Box, Box, Box]
            |    add(a: Box, b: Box) -> Box = Box(a.v + b.v + 1000)
            |""".stripMargin,
        "user/main" ->
          """import alib.*
            |
            |main() -> int
            |    val a = Box(5)
            |    val b = Box(10)
            |    val c = a |+| b
            |    c.v
            |""".stripMargin,
      )) shouldBe 1015
    }
  }
}
