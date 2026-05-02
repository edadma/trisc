package io.github.edadma.trisc

/** Pretty-prints AST nodes back to parseable Sysl source. Used by .smeta serialization to store generic template
  * declarations (data enums, structs, functions with type parameters) so they can be re-parsed on import.
  */
object SyslPrettyPrinter:
  private val IND = "    " // 4 spaces

  def attrLiteralToSource(l: AttrLiteral): String = l match
    case AttrLitString(s) => escapeString(s)
    case AttrLitInt(v)    => v.toString
    case AttrLitBool(b)   => if b then "true" else "false"
    case AttrLitIdent(n)  => n

  def attrArgToSource(a: AttrArg): String = a match
    case AttrPositional(v)     => attrLiteralToSource(v)
    case AttrNamed(k, v)       => s"$k: ${attrLiteralToSource(v)}"

  def attributesBlockToSource(attrs: List[Attribute], indent: String): String =
    attrs.map { at =>
      val inner = at.args match
        case Nil    => ""
        case args => s"(${args.map(attrArgToSource).mkString(", ")})"
      s"$indent#${at.name}$inner\n"
    }.mkString

  // --- Types ---

  def typeToSource(t: TypeAST): String = t match
    case NamedTypeAST(name, Nil)    => name
    case NamedTypeAST(name, args)   => s"$name[${args.map(typeToSource).mkString(", ")}]"
    case PtrTypeAST(inner)          => s"*${typeToSource(inner)}"
    case PtrNonNullTypeAST(inner)   => s"*${typeToSource(inner)} not null"
    case ArrayTypeAST(size, elem)   => s"[$size]${typeToSource(elem)}"
    case SliceTypeAST(elem)         => s"[]${typeToSource(elem)}"
    case FuncTypeAST(params, ret, esc, eff) =>
      val esca = if esc then "@escaping " else ""
      val effS = if eff.isPure then " #pure"
        else (eff.reads, eff.writes) match
          case (Some(r), Some(w)) => s" #reads(${r.toList.sorted.mkString(", ")}) #writes(${w.toList.sorted.mkString(", ")})"
          case (Some(r), None)    => s" #reads(${r.toList.sorted.mkString(", ")})"
          case (None, Some(w))    => s" #writes(${w.toList.sorted.mkString(", ")})"
          case _                  => ""
      s"$esca(${params.map(typeToSource).mkString(", ")}) -> ${typeToSource(ret)}$effS"
    case TupleTypeAST(elems)        => s"(${elems.map(typeToSource).mkString(", ")})"
    case RefTypeAST(inner)          => s"&${typeToSource(inner)}"
    case ByNameTypeAST(inner)       => s"=> ${typeToSource(inner)}"

  // --- Declarations ---

  def declToSource(d: DeclAST): String = d match
    case DataEnumDeclAST(name, variants, tps, _) =>
      val tpStr = if tps.nonEmpty then s"[${tps.mkString(", ")}]" else ""
      val body = variants.map { v =>
        if v.fields.isEmpty then s"${IND}${v.name}"
        else s"${IND}${v.name}(${v.fields.map((n, t) => s"$n: ${typeToSource(t)}").mkString(", ")})"
      }.mkString("\n")
      s"enum $name$tpStr\n$body"

    case StructDeclAST(name, fields, tps, _, _) =>
      val tpStr = if tps.nonEmpty then s"[${tps.mkString(", ")}]" else ""
      val body = fields.map((n, t, _) => s"${IND}$n: ${typeToSource(t)}").mkString("\n")
      s"struct $name$tpStr\n$body"

    case FunDeclAST(name, params, returnType, body, isPrivate, typeParams, typeBounds, _, isDef, _) =>
      val priv = if isPrivate then "private " else ""
      val defKw = if isDef then "def " else ""
      val tpStr =
        if typeParams.nonEmpty then
          val tpParts = typeParams.map { tp =>
            typeBounds.get(tp) match
              case Some(bounds) if bounds.nonEmpty => s"$tp: ${bounds.mkString(" + ")}"
              case _                              => tp
          }
          s"[${tpParts.mkString(", ")}]"
        else ""
      if isDef && params.isEmpty then
        val retStr = returnType.map(t => s" -> ${typeToSource(t)}").getOrElse("")
        val bodyStr = body match
          case ExprBodyAST(expr) => s" = ${exprToSource(expr)}"
          case BlockBodyAST(stmts, _) =>
            val b = stmts.map(s => s"${IND}${stmtToSource(s, 1)}").mkString("\n")
            s"\n$b"
        s"$priv${defKw}$name$retStr$bodyStr"
      else
        val paramStr = params.map(p => s"${p.name}: ${typeToSource(p.typ)}").mkString(", ")
        val retStr = returnType.map(t => s" -> ${typeToSource(t)}").getOrElse("")
        val bodyStr = bodyToSource(body, 1)
        s"$priv$defKw$name$tpStr($paramStr)$retStr$bodyStr"

    case TraitDeclAST(name, typeParams, methods, _) =>
      val body = methods.map { m =>
        val attrStr = attributesBlockToSource(m.attributes, IND)
        val paramStr = m.params.map(p => s"${p.name}: ${typeToSource(p.typ)}").mkString(", ")
        val retStr = s" -> ${typeToSource(m.returnType)}"
        val sig = m.body match
          case None    => s"${m.name}($paramStr)$retStr"
          case Some(b) => s"${m.name}($paramStr)$retStr${bodyToSource(b, 2)}"
        s"$attrStr${IND}$sig"
      }.mkString("\n")
      s"trait $name[${typeParams.mkString(", ")}]\n$body"

    case ImplDeclAST(traitName, typeParams, targetTypes, methods, _) =>
      val tpStr = if typeParams.nonEmpty then s"[${typeParams.mkString(", ")}]" else ""
      val targetStr = targetTypes.map(typeToSource).mkString(", ")
      val body = methods.map(m => s"${IND}${declToSource(m).replace("\n", s"\n")}").mkString("\n")
      s"impl$tpStr $traitName[$targetStr]\n$body"

    case _ => s"// unsupported declaration: ${d.getClass.getSimpleName}"

  // --- Function body ---

  private def bodyToSource(body: FunBodyAST, depth: Int): String = body match
    case ExprBodyAST(expr) => s" = ${exprToSource(expr, depth)}"
    case BlockBodyAST(stmts, _) =>
      val body = stmts.map(s => s"${IND * depth}${stmtToSource(s, depth)}").mkString("\n")
      s"\n$body"

  // --- Statements ---

  private def stmtToSource(s: StmtAST, depth: Int): String = s match
    case VarStmtAST(name, typ, init, isMutable, _, isConst, _) =>
      val kw = if isConst then "const" else if isMutable then "var" else "val"
      val typStr = typ.map(t => s": ${typeToSource(t)}").getOrElse("")
      init match
        case UninitDeclAST(t) => s"$kw $name: ${typeToSource(t)}"
        case _                => s"$kw $name$typStr = ${exprToSource(init, depth)}"

    case DestructureStmtAST(names, init, isMutable) =>
      val kw = if isMutable then "var" else "val"
      s"$kw (${names.mkString(", ")}) = ${exprToSource(init, depth)}"

    case AssignStmtAST(target, value) =>
      s"$target = ${exprToSource(value, depth)}"

    case CompoundAssignStmtAST(target, op, value) =>
      s"$target $op= ${exprToSource(value, depth)}"

    case DerefAssignStmtAST(pointer, value) =>
      s"*${exprToSource(pointer, depth)} = ${exprToSource(value, depth)}"

    case IndexAssignStmtAST(array, index, value) =>
      s"${exprToSource(array, depth)}[${exprToSource(index, depth)}] = ${exprToSource(value, depth)}"

    case FieldAssignStmtAST(obj, field, value) =>
      s"${exprToSource(obj, depth)}.$field = ${exprToSource(value, depth)}"

    case FieldCompoundAssignStmtAST(obj, field, op, value) =>
      s"${exprToSource(obj, depth)}.$field $op= ${exprToSource(value, depth)}"

    case ReturnStmtAST(value) =>
      value match
        case Some(v) => s"return ${exprToSource(v, depth)}"
        case None    => "return"

    case WhileStmtAST(cond, body, label) =>
      val bodyStr = body.map(s => s"${IND * (depth + 1)}${stmtToSource(s, depth + 1)}").mkString("\n")
      val prefix = label.map(l => s"$l: ").getOrElse("")
      s"${prefix}while ${exprToSource(cond, depth)}\n$bodyStr"

    case ForStmtAST(init, cond, update, body, label) =>
      val bodyStr = body.map(s => s"${IND * (depth + 1)}${stmtToSource(s, depth + 1)}").mkString("\n")
      val prefix = label.map(l => s"$l: ").getOrElse("")
      s"${prefix}for ${stmtToSource(init, depth)}; ${exprToSource(cond, depth)}; ${stmtToSource(update, depth)} do\n$bodyStr"

    case DoWhileStmtAST(cond, body, label) =>
      val bodyStr = body.map(s => s"${IND * (depth + 1)}${stmtToSource(s, depth + 1)}").mkString("\n")
      val prefix = label.map(l => s"$l: ").getOrElse("")
      s"${prefix}do\n$bodyStr\n${IND * depth}while ${exprToSource(cond, depth)}"

    case LoopStmtAST(body, label) =>
      val bodyStr = body.map(s => s"${IND * (depth + 1)}${stmtToSource(s, depth + 1)}").mkString("\n")
      val prefix = label.map(l => s"$l: ").getOrElse("")
      s"${prefix}loop\n$bodyStr"

    case BreakStmtAST(label)    => label.map(l => s"break $l").getOrElse("break")
    case ContinueStmtAST(label) => label.map(l => s"continue $l").getOrElse("continue")

    case DeferStmtAST(body) =>
      s"defer ${stmtToSource(body, depth)}"

    case AsmStmtAST(code) =>
      s"asm(${escapeString(code)})"

    case ExprStmtAST(expr) =>
      exprToSource(expr, depth)

  // --- Expressions ---

  def exprToSource(e: ExpressionAST, depth: Int = 0): String = e match
    case IntLitAST(value)            => value.toString
    case TypedIntLitAST(value, name) => s"$value$name"
    case FloatLitAST(value)          => value.toString
    case CharLitAST(value)           => s"'${escapeChar(value)}'"
    case StringLitAST(value)         => escapeString(value)
    case StringLitExprAST(value)     => escapeString(value)
    case BoolLitAST(true)            => "true"
    case BoolLitAST(false)           => "false"
    case VarRefAST(name)             => name

    case BinaryAST(left, op, right) =>
      s"(${exprToSource(left, depth)} $op ${exprToSource(right, depth)})"

    case UnaryAST(op, operand) =>
      s"$op${exprToSource(operand, depth)}"

    case PreIncAST(name)  => s"++$name"
    case PreDecAST(name)  => s"--$name"
    case PostIncAST(name) => s"$name++"
    case PostDecAST(name) => s"$name--"

    case CallAST(name, args) =>
      s"$name(${args.map(exprToSource(_, depth)).mkString(", ")})"

    case IndirectCallAST(callee, args) =>
      s"${exprToSource(callee, depth)}(${args.map(exprToSource(_, depth)).mkString(", ")})"

    case MethodCallAST(obj, method, args) =>
      s"${exprToSource(obj, depth)}.$method(${args.map(exprToSource(_, depth)).mkString(", ")})"

    case CastAST(PtrTypeAST(NamedTypeAST(name, Nil)), expr) =>
      s"*$name(${exprToSource(expr, depth)})"

    case CastAST(NamedTypeAST(name, Nil), expr) =>
      s"$name(${exprToSource(expr, depth)})"

    case CastAST(targetType, expr) =>
      s"${typeToSource(targetType)}(${exprToSource(expr, depth)})"

    case IfExprAST(cond, thenBody, elseBody) =>
      val thenStr = thenBody match
        case List(ExprStmtAST(e)) => s" ${exprToSource(e, depth)}"
        case stmts                => stmts.map(s => s"${IND * (depth + 1)}${stmtToSource(s, depth + 1)}").mkString("\n", "\n", "")
      val elseStr = elseBody match
        case None                          => ""
        case Some(List(ExprStmtAST(ie: IfExprAST))) => s"\n${IND * depth}elif ${exprToSource(ie, depth).stripPrefix("if ")}"
        case Some(List(ExprStmtAST(e)))    => s" else ${exprToSource(e, depth)}"
        case Some(stmts) =>
          val body = stmts.map(s => s"${IND * (depth + 1)}${stmtToSource(s, depth + 1)}").mkString("\n")
          s"\n${IND * depth}else\n$body"
      s"if ${exprToSource(cond, depth)} then$thenStr$elseStr"

    case TryAST(expr) =>
      s"${exprToSource(expr, depth)}?"

    case MatchExprAST(expr, arms, default) =>
      val armStr = arms.map(matchArmToSource(_, depth + 1)).mkString("\n")
      val defStr = default match
        case None => ""
        case Some(List(ExprStmtAST(e))) => s"\n${IND * (depth + 1)}else -> ${exprToSource(e, depth + 1)}"
        case Some(stmts) =>
          val body = stmts.map(s => s"${IND * (depth + 2)}${stmtToSource(s, depth + 2)}").mkString("\n")
          s"\n${IND * (depth + 1)}else ->\n$body"
      s"${exprToSource(expr, depth)} match\n$armStr$defStr"

    case AddrOfAST(name) => s"&$name"
    case AddrOfIndexAST(arr, idx) => s"&${exprToSource(arr, depth)}[${exprToSource(idx, depth)}]"
    case AddrOfFieldAST(obj, field) => s"&${exprToSource(obj, depth)}.$field"
    case DerefAST(expr) => s"*${exprToSource(expr, depth)}"

    case IndexAST(expr, index) =>
      s"${exprToSource(expr, depth)}[${exprToSource(index, depth)}]"

    case SliceExprAST(array, low, high) =>
      val lo = low.map(exprToSource(_, depth)).getOrElse("")
      val hi = high.map(exprToSource(_, depth)).getOrElse("")
      s"${exprToSource(array, depth)}[$lo:$hi]"

    case FieldAccessAST(obj, field) =>
      s"${exprToSource(obj, depth)}.$field"

    case FieldPreIncAST(obj, field)  => s"++${exprToSource(obj, depth)}.$field"
    case FieldPreDecAST(obj, field)  => s"--${exprToSource(obj, depth)}.$field"
    case FieldPostIncAST(obj, field) => s"${exprToSource(obj, depth)}.$field++"
    case FieldPostDecAST(obj, field) => s"${exprToSource(obj, depth)}.$field--"

    case ArrayDeclAST(size, elemType) =>
      s"[${size}]${typeToSource(elemType)}"

    case ArrayLitAST(elements) =>
      s"[${elements.map(exprToSource(_, depth)).mkString(", ")}]"

    case TupleLitAST(elements) =>
      s"(${elements.map(exprToSource(_, depth)).mkString(", ")})"

    case StructInitAST(typeName) =>
      s"$typeName()"

    case UninitDeclAST(typeName) =>
      s"uninit ${typeToSource(typeName)}"

    case SizeofTypeAST(typeName) =>
      s"sizeof(${typeToSource(typeName)})"

    case SizeofExprAST(expr) =>
      s"sizeof(${exprToSource(expr, depth)})"

    case NewExprAST(typeName, args) =>
      s"new $typeName(${args.map(exprToSource(_, depth)).mkString(", ")})"

    case NewArrayAST(size, elemType) =>
      s"new [${exprToSource(size, depth)}]${typeToSource(elemType)}"

    case AsmExprAST(code) =>
      s"asm(${escapeString(code)})"

    case ClosureAST(params, body) =>
      val paramStr = params match
        case Nil                                                  => "()"
        case List(ClosureParamAST(name, None))                    => name
        case ps => s"(${ps.map(p => p.typ.map(t => s"${p.name}: ${typeToSource(t)}").getOrElse(p.name)).mkString(", ")})"
      val bodyStr = bodyToSource(body, depth + 1)
      s"$paramStr ->$bodyStr"

    case _ => s"/* unsupported: ${e.getClass.getSimpleName} */"

  // --- Match arms ---

  private def matchArmToSource(arm: MatchArmAST, depth: Int): String =
    val pats = arm.patterns.map(patternToSource).mkString(", ")
    val guard = arm.guard.map(g => s" if ${exprToSource(g, depth)}").getOrElse("")
    arm.body match
      case List(ExprStmtAST(e)) =>
        s"${IND * depth}$pats$guard -> ${exprToSource(e, depth)}"
      case stmts =>
        val body = stmts.map(s => s"${IND * (depth + 1)}${stmtToSource(s, depth + 1)}").mkString("\n")
        s"${IND * depth}$pats$guard ->\n$body"

  // --- Patterns ---

  private def patternToSource(p: MatchPatternAST): String = p match
    case WildcardPatternAST            => "_"
    case ValuePatternAST(expr)         => exprToSource(expr)
    case RangePatternAST(low, high)    => s"${exprToSource(low)}..${exprToSource(high)}"
    case DestructurePatternAST(name, fields) =>
      if fields.isEmpty then name
      else s"$name(${fields.map(patternToSource).mkString(", ")})"

  // --- String escaping ---

  private def escapeString(s: String): String =
    val sb = new StringBuilder("\"")
    for c <- s do
      c match
        case '\n' => sb ++= "\\n"
        case '\t' => sb ++= "\\t"
        case '\r' => sb ++= "\\r"
        case '\\' => sb ++= "\\\\"
        case '"'  => sb ++= "\\\""
        case '\u0000' => sb ++= "\\0"
        case c if c < ' ' => sb ++= f"\\x${c.toInt}%02x"
        case c    => sb += c
    sb += '"'
    sb.toString

  private def escapeChar(c: Char): String = c match
    case '\n' => "\\n"
    case '\t' => "\\t"
    case '\r' => "\\r"
    case '\\' => "\\\\"
    case '\'' => "\\'"
    case '\u0000' => "\\0"
    case c if c < ' ' => f"\\x${c.toInt}%02x"
    case c    => c.toString
