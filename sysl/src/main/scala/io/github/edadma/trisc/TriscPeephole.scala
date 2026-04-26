package io.github.edadma.trisc

import scala.collection.mutable

/** Peephole optimizer for TRISC assembly output.
 *
 *  Operates on a structured `Line` ADT that the codegen pushes directly into an
 *  array (no string round-trip). Rules see contiguous runs of `Instr` lines bounded
 *  by `Label`/`Directive`/`Comment`/`Blank` barriers — labels in particular are
 *  barriers because anything could branch to them, so we can't assume preceding state.
 *
 *  ## Adding a rule
 *
 *  Most rules fit `Rule.window(N)`:
 *
 *  ```scala
 *  val myRule: Rule = Rule.window(2, "my-rule") {
 *    case List(Instr("foo", List(Reg(a), ...)), Instr("bar", ...)) if guard =>
 *      List(Instr("baz", List(reg(a), ...)))
 *  }
 *  ```
 *
 *  Add to `defaultRules` to enable. Variable-window rules can implement `Rule`
 *  directly and consume any number of lookahead instructions.
 *
 *  ## Tuning
 *
 *  - `optimize(lines, rules, maxPasses)` runs to fixed point or `maxPasses`.
 *  - Within a run, scanning resumes at the start of the replacement so chained
 *    rewrites compound in one pass.
 *  - Rules are tried in list order at each position; first match wins.
 */
object TriscPeephole:

  // ===== Line ADT =====

  sealed trait Line:
    def render: String

  /** An assembly instruction. `mnemonic` is the opcode token; `operands` are the
   *  comma-separated arguments in source order, each trimmed of whitespace. */
  case class Instr(mnemonic: String, operands: List[String]) extends Line:
    def render: String =
      if operands.isEmpty then s"  $mnemonic"
      else s"  $mnemonic ${operands.mkString(", ")}"

  /** A label: either `name:` (column-0 with colon) or `.name` (column-0, dot-prefixed,
   *  no colon — the TRISC assembler accepts both). Acts as an optimization barrier. */
  case class Label(text: String) extends Line:
    def render: String = text

  /** A non-instruction column-0 line (`global ...`, `segment ...`, `extern ...`,
   *  `entry ...`). Pass-through; acts as a barrier. */
  case class Directive(text: String) extends Line:
    def render: String = text

  /** A `# ...` line. Pass-through; acts as a barrier. */
  case class Comment(text: String) extends Line:
    def render: String = text

  /** Blank line. Pass-through; acts as a barrier (by virtue of not being an Instr). */
  case object Blank extends Line:
    def render: String = ""

  // ===== Operand extractors =====
  //
  // Use these in rule pattern matches to destructure operand strings into typed values.

  /** Match an `r<N>` register operand. */
  object Reg:
    def unapply(s: String): Option[Int] =
      if s.length >= 2 && s.charAt(0) == 'r' then s.substring(1).toIntOption
      else None

  /** Match a numeric immediate operand (decimal; supports negative). */
  object Imm:
    def unapply(s: String): Option[Long] = s.toLongOption

  /** Format a register operand (inverse of `Reg.unapply`). */
  def reg(n: Int): String = s"r$n"

  // ===== Rule API =====

  /** A peephole rule. Given the remaining instruction tail, decides whether (and how
   *  much of it) to rewrite. */
  trait Rule:
    def name: String
    /** Try to match starting at the head of `tail`. On a match, return
     *  `Some((numConsumed, replacement))` — `numConsumed` is how many input Instrs to
     *  remove from the head and `replacement` is what to put in their place (may be
     *  empty to delete entirely; may be longer than consumed). Return `None` to skip. */
    def apply(tail: List[Instr]): Option[(Int, List[Instr])]

  object Rule:
    /** Build a fixed-N-instruction-window rule from a partial function over the window.
     *  The PF returns the replacement for the matched window. */
    def window(n: Int, ruleName: String)(pf: PartialFunction[List[Instr], List[Instr]]): Rule =
      new Rule:
        val name = ruleName
        def apply(tail: List[Instr]): Option[(Int, List[Instr])] =
          val w = tail.take(n)
          if w.length == n && pf.isDefinedAt(w) then Some((n, pf(w)))
          else None

  // ===== Rule library =====
  //
  // Each rule is a small, locally-verifiable rewrite — none of these need cross-block
  // reasoning. Rules are declared first so `defaultRules` (below) can refer to them
  // without forward-reference NPEs at object-init time.

  /** `mov rD, rD` → ε (no-op self-move). */
  val movSelfDrop: Rule = Rule.window(1, "mov-self-drop") {
    case List(Instr("mov", List(Reg(d), Reg(s)))) if d == s => Nil
  }

  /** `addi rD, rD, 0` → ε (no-op self-add). */
  val addiSelfZeroDrop: Rule = Rule.window(1, "addi-self-zero-drop") {
    case List(Instr("addi", List(Reg(d), Reg(a), Imm(0)))) if d == a => Nil
  }

  /** `addi rD, rA, 0` (D ≠ A) → `mov rD, rA` (the addi is just a register copy). */
  val addiZeroToMov: Rule = Rule.window(1, "addi-zero-to-mov") {
    case List(Instr("addi", List(Reg(d), Reg(a), Imm(0)))) if d != a =>
      List(Instr("mov", List(reg(d), reg(a))))
  }

  /** `pshd rN; popd rN` → ε (push then immediately pop the same register: pure
   *  stack churn unless the intervening code reads the slot, which can't happen
   *  in a 2-instruction window). */
  val pshdPopdSameReg: Rule = Rule.window(2, "pshd-popd-same-reg") {
    case List(Instr("pshd", List(Reg(s))), Instr("popd", List(Reg(d)))) if s == d => Nil
  }

  /** `pshd rS; popd rD` (S ≠ D) → `mov rD, rS` (round-trip through stack just to
   *  copy a register — the codegen emits this pattern pervasively). */
  val pshdPopdDiffReg: Rule = Rule.window(2, "pshd-popd-diff-reg") {
    case List(Instr("pshd", List(Reg(s))), Instr("popd", List(Reg(d)))) if s != d =>
      List(Instr("mov", List(reg(d), reg(s))))
  }

  /** `addi rD, rA, X; addi rD, rD, Y` → `addi rD, rA, X+Y` (fold consecutive
   *  add-immediate to the same register). Skipped if the folded constant doesn't
   *  fit the assembler's signed-16-bit immediate range. */
  val addiFold: Rule = Rule.window(2, "addi-fold") {
    case List(
      Instr("addi", List(Reg(d1), Reg(a), Imm(x))),
      Instr("addi", List(Reg(d2), Reg(s), Imm(y))),
    ) if d1 == d2 && d2 == s && fitsAddiImm(x + y) =>
      List(Instr("addi", List(reg(d1), reg(a), (x + y).toString)))
  }

  /** TRISC `addi` accepts a signed-16-bit immediate. Conservative — verify against
   *  the actual assembler if you want to widen. */
  private def fitsAddiImm(v: Long): Boolean = v >= -32768L && v <= 32767L

  /** TRISC load mnemonics that take the form `ldX rD, rA, rB` with addr = rA + rB. */
  private val loadMnemonics: Set[String] = Set("ldb", "lds", "ldw", "ldd")

  /** TRISC store mnemonics that take the form `stX rS, rA, rB` with addr = rA + rB. */
  private val storeMnemonics: Set[String] = Set("stb", "sts", "stw", "std")

  // ===== Register-write model =====
  //
  // Returns true if `instr` may write to register `target`. Conservative — when
  // unsure, returns true so rules that depend on "this register is preserved"
  // bail out safely. Used by the variable-window `dead-pshd-popd` rule.

  /** Mnemonics that write their first register operand (and only that). */
  private val writesFirstOpReg: Set[String] = Set(
    "mov", "addi", "add", "sub", "div", "and", "or", "xor", "not", "lsl", "lsr", "asr",
    "ldi", "movi", "ldb", "lds", "ldw", "ldd",
    "zeb", "zes", "zew", "seb", "ses", "sew",
    "f32tof64", "i2f", "f2i", "fadd", "fsub", "fmul", "fdiv",
  )

  def writesReg(instr: Instr, target: Int): Boolean = instr match
    // r0 is hardwired zero — no instruction can actually write to it.
    case _ if target == 0 => false
    // Common form: rD is the first operand and is the destination.
    case Instr(m, Reg(d) :: _) if writesFirstOpReg.contains(m) => d == target
    // mul clobbers both rD and r((d+1) & 7) — see CLAUDE.md TRISC notes.
    case Instr("mul", Reg(d) :: _) => d == target || ((d + 1) & 7) == target
    // pshd modifies SP (r7) but no other register.
    case Instr("pshd", _) => target == 7
    // popd writes both the named destination AND modifies r7.
    case Instr("popd", List(Reg(d))) => d == target || target == 7
    // jalr rDest, rTarget: writes rDest (link register) AND clobbers all
    // caller-saved regs per ABI: r1 (return value), r2/r3/r4 (scratch), and r6
    // (link). r5 (fp) and r7 (sp) are callee-preserved.
    case Instr("jalr", List(Reg(d), _)) =>
      d == target || (target match
        case 1 | 2 | 3 | 4 | 6 => true
        case _ => false)
    // Stores (stb/sts/stw/std), branches, and asm/labels never write registers.
    case _ => false

  /** True if `instr` is a control transfer that could leave this basic-block run.
   *  Used to bail on `dead-pshd-popd` when the middle contains a branch — the
   *  matching popd may not be reachable on every path. (Function calls via
   *  `jalr rLink, rTarget` with a non-zero link return here, so they're fine.) */
  def exitsBlock(instr: Instr): Boolean = instr match
    case Instr("bra" | "beq" | "bne" | "blt" | "ble" | "bgt" | "bge" | "bltu" | "bleu" | "bgtu" | "bgeu", _) => true
    case Instr("jalr", List(Reg(0), _)) => true  // tail-jump: r0 link discarded
    case _ => false

  /** `mov rD, rS; ldX rD, rD, r0` → `ldX rD, rS, r0` — the mov's destination is
   *  immediately overwritten by the load, so the mov is just a register-rename
   *  that we can fold into the load's base operand. Always safe (no liveness check
   *  needed because rD's pre-load value is unused). */
  val movLoadFold: Rule = Rule.window(2, "mov-load-fold") {
    case List(
      Instr("mov", List(Reg(d1), Reg(s))),
      Instr(ld, List(Reg(d2), Reg(a), Reg(0))),
    ) if loadMnemonics.contains(ld) && d1 == d2 && d2 == a =>
      List(Instr(ld, List(reg(d1), reg(s), reg(0))))
  }

  /** `mov rD, rS; addi rD, rD, K` → `addi rD, rS, K` — same logic: the mov's
   *  destination is immediately overwritten, so the mov is folded into addi's
   *  source. Pairs naturally with `addi-zero-to-mov` running first. */
  val movAddiFold: Rule = Rule.window(2, "mov-addi-fold") {
    case List(
      Instr("mov", List(Reg(d1), Reg(s))),
      Instr("addi", List(Reg(d2), Reg(d3), Imm(k))),
    ) if d1 == d2 && d2 == d3 && fitsAddiImm(k) =>
      List(Instr("addi", List(reg(d1), reg(s), k.toString)))
  }

  /** `std rA, rB, r0; ldd rA, rB, r0` → `std rA, rB, r0` — drop the load: we
   *  just stored the full 8-byte register value, so reading it back gives the
   *  same value. Restricted to `std`/`ldd` because sub-word stw/stb followed by
   *  ldw/ldb would truncate-then-sign-extend any garbage in the register's high
   *  bits, changing the register's observed value if the load were removed. */
  val stdLddSameReg: Rule = Rule.window(2, "std-ldd-same-reg") {
    case List(
      stI @ Instr("std", List(Reg(s1), Reg(b1), Reg(c1))),
      Instr("ldd", List(Reg(d), Reg(b2), Reg(c2))),
    ) if s1 == d && b1 == b2 && c1 == c2 =>
      List(stI)
  }

  // ===== Forward copy propagation =====
  //
  // Subsumes most ad-hoc mov-fold rules. After `mov rD, rS`, scan forward and:
  //   - if an instruction reads rD as a SOURCE, rewrite that source to rS
  //   - if rS is written, stop (rS no longer holds the original value)
  //   - if rD is written without being read first, drop the mov (rD is dead)
  //   - if we hit a call, branch, or block exit, stop (conservative)
  //
  // The rewritten instructions replace the mov + original instructions; if rD is
  // never killed within the lookahead, the mov stays.

  /** Maximum instructions to scan during forward copy propagation. */
  private val copyPropLookahead = 16

  /** Substitute every source-position occurrence of `oldReg` with `newReg` in
   *  `instr`. The destination operand (first operand, except for stores/branches/
   *  pshd which read all operands) is left untouched, mirroring `readsReg`. */
  def rewriteSourceUses(instr: Instr, oldReg: Int, newReg: Int): Instr =
    val from = reg(oldReg)
    val to = reg(newReg)
    def sub(s: String): String = if s == from then to else s
    instr match
      // Stores read all operands; rewrite all.
      case Instr(m, ops) if storeMnemonics.contains(m) =>
        Instr(m, ops.map(sub))
      // pshd reads its single operand.
      case Instr("pshd", ops) => Instr("pshd", ops.map(sub))
      // popd writes its operand; don't rewrite.
      case Instr("popd", _) => instr
      // Conditional branches read both compared registers.
      case Instr(m @ ("beq" | "bne" | "blt" | "ble" | "bgt" | "bge" | "bltu" | "bleu" | "bgtu" | "bgeu"), ops) =>
        Instr(m, ops.map(sub))
      // jalr reads only its second operand (the target).
      case Instr("jalr", List(d, t)) => Instr("jalr", List(d, sub(t)))
      // For everything else, the first operand is the destination — leave it,
      // rewrite the rest.
      case Instr(m, Nil) => instr
      case Instr(m, head :: srcs) => Instr(m, head :: srcs.map(sub))

  /** Forward copy-propagation rule. Replaces `mov rD, rS; <stuff using rD>; <kill of rD>`
   *  with `<stuff using rS>; <kill of rD>`, dropping the mov entirely when proven dead.
   *  If no kill is found within `copyPropLookahead`, the mov stays.
   *
   *  Bails on:
   *  - `exitsBlock` instructions (branches/tail jumps where the kill may be on a
   *    path we can't prove reachable),
   *  - `jalr` (function calls have implicit register-reading semantics that
   *    `readsReg` can't see — the called function may consume r1 as an argument
   *    or read other registers per ABI; we can't safely drop a mov whose value
   *    feeds into the call),
   *  - any explicit write to rS (after which propagation can't continue because
   *    rS no longer holds the captured value). */
  val copyPropagate: Rule = new Rule:
    val name = "copy-propagate"
    def apply(tail: List[Instr]): Option[(Int, List[Instr])] = tail match
      case Instr("mov", List(Reg(d), Reg(s))) :: rest if d != s =>
        val rewritten = mutable.ListBuffer.empty[Instr]
        var i = 0
        var killed = false
        var bailed = false
        val it = rest.iterator
        while it.hasNext && i < copyPropLookahead && !killed && !bailed do
          val instr = it.next()
          if exitsBlock(instr) || instr.mnemonic == "jalr" then bailed = true
          else if writesReg(instr, s) then bailed = true
          else
            val reads = readsReg(instr, d)
            val writes = writesReg(instr, d)
            // Rewrite source uses of rD to rS first.
            val rewrittenInstr = if reads then rewriteSourceUses(instr, d, s) else instr
            rewritten += rewrittenInstr
            // After this instr, rD either keeps its mov-derived value (if not
            // written), or holds a fresh value (if written). Either case past a
            // write means we stop propagating.
            if writes then killed = true
          i += 1
        if killed then
          // Drop the mov, return the (possibly-rewritten) tail in its place.
          Some((1 + rewritten.length, rewritten.toList))
        else None
      case _ => None

  /** Returns true if any operand of `instr` references register `target`. Used by
   *  `deadPshdPopd` to detect r7 reads in the middle (pshd modifies r7, so any
   *  r7-relative load/store/addi in the middle sees a different value if pshd is
   *  dropped). */
  def usesReg(instr: Instr, target: Int): Boolean =
    val name = reg(target)
    instr.operands.exists(_ == name)

  /** Returns true if `instr` reads register `target` as a source (excludes the
   *  destination operand). Conservatively reports true for unfamiliar mnemonics.
   *  Used by 3-window copy-propagation rules to confirm a register is dead. */
  def readsReg(instr: Instr, target: Int): Boolean =
    val name = reg(target)
    instr match
      // Stores read all operands (including the value register).
      case Instr(m, ops) if storeMnemonics.contains(m) => ops.exists(_ == name)
      // pshd reads its single register operand.
      case Instr("pshd", ops) => ops.exists(_ == name)
      // popd reads from the stack (memory), writes its register operand.
      case Instr("popd", _) => false
      // Conditional branches read both compared registers.
      case Instr("beq" | "bne" | "blt" | "ble" | "bgt" | "bge" | "bltu" | "bleu" | "bgtu" | "bgeu", ops) =>
        ops.exists(_ == name)
      // Unconditional bra reads no register.
      case Instr("bra", _) => false
      // jalr rDest, rTarget: writes rDest, reads rTarget. After call, all caller-
      // saved registers (r1–r4, r6) are clobbered — but for "reads target" we
      // only count the explicit read.
      case Instr("jalr", List(_, src)) => src == name
      case Instr("jalr", _) => true
      // For everything else (arithmetic/load/etc.), the first operand is the
      // destination and the rest are read sources.
      case Instr(_, Nil) => false
      case Instr(_, _ :: srcs) => srcs.exists(_ == name)

  /** Variable-window rule: eliminate `pshd rN; <middle>; popd rN` when the middle
   *  is provably stack-neutral and doesn't depend on the saved register or the
   *  modified SP.
   *
   *  Bails when middle contains any of:
   *  - a branch (matching popd may be unreachable on some path),
   *  - a tail jump (`jalr r0, rX`),
   *  - a write to rN (the save is necessary),
   *  - a function call (`jalr` with non-zero link) — function ABI and stack frame
   *    semantics make r7-relative reasoning brittle,
   *  - any reference to r7 (pshd modifies r7; r7-relative loads/stores/addi
   *    would observe the deeper SP if pshd weren't there).
   *
   *  The middle's own balanced `pshd`/`popd` pairs are tracked via a depth
   *  counter so nested save/restores don't confuse the matching. */
  val deadPshdPopd: Rule = new Rule:
    val name = "dead-pshd-popd"
    def apply(tail: List[Instr]): Option[(Int, List[Instr])] = tail match
      case Instr("pshd", List(Reg(savedReg))) :: rest =>
        var depth = 1
        var idx = 0
        var matched = -1
        val it = rest.iterator
        while it.hasNext && matched < 0 do
          val instr = it.next()
          if exitsBlock(instr) then return None
          // Function calls have ABI/stack semantics we can't reason about locally.
          if instr.mnemonic == "jalr" then return None
          // Any r7 reference disqualifies the elimination — pshd shifted r7 by 8.
          if usesReg(instr, 7) then return None
          instr match
            case Instr("pshd", _) => depth += 1
            case Instr("popd", _) =>
              if depth == 1 then matched = idx
              else depth -= 1
            case _ =>
          idx += 1
        if matched < 0 then return None
        rest(matched) match
          case Instr("popd", List(Reg(r))) if r == savedReg =>
            val middle = rest.take(matched)
            if middle.exists(writesReg(_, savedReg)) then return None
            Some((1 + matched + 1, middle))
          case _ => None
      case _ => None

  /** Default rule set — order matters when windows overlap (earlier wins).
   *
   *  Disabled rules (correct in isolation per unit tests, but cause
   *  `SyslTriscStringRefcountTests` failures in the larger posix-linked codepath
   *  for reasons not yet diagnosed):
   *
   *  - `pshdPopdSameReg` / `pshdPopdDiffReg` — eliminate adjacent `pshd; popd`
   *    pairs. The codegen emits these in arg-passing setup and somewhere in
   *    posix.string the side effect of the stack write seems to matter.
   *  - `copyPropagate` — full forward copy propagation. Same test fails when it
   *    rewrites uses of rD to rS through the posix.string code, even though my
   *    `readsReg`/`writesReg` model thinks it's safe. Suggests some implicit
   *    register-use convention I'm not modeling (e.g. an instruction that reads
   *    a register I treat as write-only).
   *  - `movLoadSrcFoldDead` / `movAddiSrcFoldDead` / `movStoreFoldDead` — same
   *    family as copyPropagate; same test failure suspected.
   *
   *  TODO: investigate by dumping the assembled bytes for a passing run vs an
   *  optimized run and bisecting at the byte level. Likely there's a subtle ABI
   *  detail (e.g. an implicit clobber by a particular mnemonic, or a stack-slot
   *  access we can't see without dataflow). Re-enable rules incrementally as the
   *  model improves.
   */
  val defaultRules: List[Rule] = List(
    movSelfDrop,
    addiSelfZeroDrop,
    addiZeroToMov,
    addiFold,
    movLoadFold,
    movAddiFold,
    stdLddSameReg,
  )

  // ===== Optimizer driver =====

  case class Stats(perRule: Map[String, Int], passes: Int):
    def totalRewrites: Int = perRule.values.sum
    def isEmpty: Boolean = totalRewrites == 0
    def report: String =
      if isEmpty then s"peephole: no rewrites in $passes pass(es)"
      else
        val lines = perRule.toList.sortBy(-_._2).map((n, c) => s"  $n: $c")
        s"peephole: $totalRewrites rewrites in $passes pass(es)\n${lines.mkString("\n")}"

  /** Optimize `lines` by applying rules to each contiguous run of `Instr` lines.
   *  Repeats passes until no rule fires (fixed point) or `maxPasses` is reached.
   *  Returns the optimized lines and per-rule application stats. */
  def optimize(
      lines: collection.Seq[Line],
      rules: List[Rule] = defaultRules,
      maxPasses: Int = 16,
  ): (List[Line], Stats) =
    val perRule = mutable.LinkedHashMap.empty[String, Int]
    var current: List[Line] = lines.toList
    var pass = 0
    var changed = true
    while changed && pass < maxPasses do
      changed = false
      val next = mutable.ListBuffer.empty[Line]
      val run = mutable.ListBuffer.empty[Instr]
      def flush(): Unit =
        if run.nonEmpty then
          val (optimized, hits) = applyRulesToRun(run.toList, rules)
          for (n, c) <- hits if c > 0 do
            perRule(n) = perRule.getOrElse(n, 0) + c
            changed = true
          next ++= optimized
          run.clear()
      for line <- current do
        line match
          case i: Instr => run += i
          case other =>
            flush()
            next += other
      flush()
      current = next.toList
      pass += 1
    (current, Stats(perRule.toMap, pass))

  /** Apply rules to a single contiguous run of `Instr` lines. First-applicable rule per
   *  position wins; after a match, scanning resumes at the start of the replacement so
   *  chained rewrites compound within one pass. */
  private def applyRulesToRun(run: List[Instr], rules: List[Rule]): (List[Instr], Map[String, Int]) =
    val out = mutable.ListBuffer.empty[Instr]
    val hits = mutable.LinkedHashMap.empty[String, Int]
    var rest = run
    while rest.nonEmpty do
      var matched = false
      val it = rules.iterator
      while it.hasNext && !matched do
        val rule = it.next()
        rule(rest) match
          case Some((n, replacement)) =>
            hits(rule.name) = hits.getOrElse(rule.name, 0) + 1
            // Prepend replacement onto rest so chained rules can fire on the new head.
            rest = replacement ++ rest.drop(n)
            matched = true
          case None =>
      if !matched then
        out += rest.head
        rest = rest.tail
    (out.toList, hits.toMap)

  // ===== I/O =====

  /** Render lines back to asm source. */
  def render(lines: collection.Seq[Line]): String =
    val sb = new StringBuilder
    for line <- lines do
      sb ++= line.render
      sb += '\n'
    sb.toString

  /** Parse one line of TRISC asm into a `Line`. Used by `SyslTriscCodegen.emit` to
   *  push structured lines directly into the codegen's output array — no string
   *  round-trip. Also useful for tests and debugging via [[parse]]. */
  def parseLine(raw: String): Line =
    if raw.isEmpty || raw.forall(_.isWhitespace) then Blank
    else if raw.startsWith("#") then Comment(raw)
    else if !raw.charAt(0).isWhitespace then
      // Column 0: label or directive. Labels are either `.name` (dot-prefixed,
      // no colon) or `name:` (colon-terminated). Everything else (`global ...`,
      // `entry ...`, etc.) is a directive.
      if raw.startsWith(".") || raw.endsWith(":") then Label(raw)
      else Directive(raw)
    else
      // Indented: instruction. Split on first whitespace into mnemonic + operands;
      // operands are comma-separated, each trimmed.
      val trimmed = raw.trim
      val sp = trimmed.indexWhere(_.isWhitespace)
      if sp < 0 then Instr(trimmed, Nil)
      else
        val mnem = trimmed.substring(0, sp)
        val rest = trimmed.substring(sp + 1).trim
        val operands =
          if rest.isEmpty then Nil
          else rest.split(',').iterator.map(_.trim).toList
        Instr(mnem, operands)

  /** Parse a multi-line asm string. Test/debug entry — production codepaths push
   *  structured `Line` values via `parseLine` directly from `emit`. */
  def parse(asm: String): List[Line] = asm.linesIterator.map(parseLine).toList

  /** End-to-end string → string convenience for tests and debugging. */
  def apply(asm: String, rules: List[Rule] = defaultRules, maxPasses: Int = 16): (String, Stats) =
    val (optimized, stats) = optimize(parse(asm), rules, maxPasses)
    (render(optimized), stats)
