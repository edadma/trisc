package io.github.edadma.trisc

object Grammars:

  val grammars: Map[String, String] = Map(
    "sysl" -> """{
    "scopeName": "source.sysl",
    "patterns": [
      { "match": "//.*$", "name": "comment.line.sysl" },
      { "begin": "/\\*", "end": "\\*/", "name": "comment.block.sysl" },
      { "begin": "\"", "end": "\"", "name": "string.quoted.double.sysl",
        "patterns": [{ "match": "\\\\.", "name": "constant.character.escape.sysl" }] },
      { "begin": "'", "end": "'", "name": "string.quoted.single.sysl" },
      { "match": "\\b(if|then|elif|else|while|do|for|break|continue|return|defer|import|private|var|val|struct|enum|type|sizeof|asm|extern|func|end)\\b", "name": "keyword.control.sysl" },
      { "match": "\\b(true|false)\\b", "name": "constant.language.sysl" },
      { "match": "\\b(int|char|byte|bool|void|string|i8|i16|i32|i64|u8|u16|u32|u64|double|f64)\\b", "name": "entity.name.type.sysl" },
      { "match": "\\b0[xX][0-9a-fA-F_]+\\b", "name": "constant.numeric.hex.sysl" },
      { "match": "\\b\\d+\\.\\d+([eE][+-]?\\d+)?\\b", "name": "constant.numeric.float.sysl" },
      { "match": "\\b\\d[\\d_]*\\b", "name": "constant.numeric.integer.sysl" },
      { "match": "->|=>|\\+=|-=|\\*=|/=|%=|&=|\\|=|\\^=|<<=|>>=|==|!=|<=|>=|&&|\\|\\||<<|>>", "name": "keyword.operator.sysl" }
    ]
  }""",

    "scala" -> """{
    "scopeName": "source.scala",
    "patterns": [
      { "match": "//.*$", "name": "comment.line.scala" },
      { "begin": "/\\*", "end": "\\*/", "name": "comment.block.scala" },
      { "begin": "\"\"\"", "end": "\"\"\"", "name": "string.quoted.triple.scala" },
      { "begin": "\"", "end": "\"", "name": "string.quoted.double.scala",
        "patterns": [{ "match": "\\\\.", "name": "constant.character.escape.scala" }] },
      { "match": "\\b(abstract|case|catch|class|def|do|else|extends|final|finally|for|forSome|given|if|implicit|import|lazy|match|new|object|override|package|private|protected|return|sealed|super|this|throw|trait|try|type|using|val|var|while|with|yield|enum|export|then|end|extension|transparent|inline|opaque|open|derives)\\b", "name": "keyword.control.scala" },
      { "match": "\\b(true|false|null|Nil|None)\\b", "name": "constant.language.scala" },
      { "match": "\\b(Boolean|Byte|Char|Double|Float|Int|Long|Short|Unit|String|Any|AnyRef|AnyVal|Nothing|Null|Option|Some|List|Map|Set|Seq|Vector|Array|Either|Left|Right|Future|Try|Success|Failure)\\b", "name": "entity.name.type.scala" },
      { "match": "\\b0[xX][0-9a-fA-F_]+[lL]?\\b", "name": "constant.numeric.hex.scala" },
      { "match": "\\b\\d[\\d_]*\\.\\d[\\d_]*([eE][+-]?\\d+)?[fFdD]?\\b", "name": "constant.numeric.float.scala" },
      { "match": "\\b\\d[\\d_]*[lL]?\\b", "name": "constant.numeric.integer.scala" },
      { "match": "@\\w+", "name": "entity.name.function.scala" },
      { "match": "=>|<-|->|=|\\+|-|\\*|/|%|!|&|\\||\\^|~|<|>", "name": "keyword.operator.scala" }
    ]
  }""",

    "javascript" -> """{
    "scopeName": "source.js",
    "patterns": [
      { "match": "//.*$", "name": "comment.line.js" },
      { "begin": "/\\*", "end": "\\*/", "name": "comment.block.js" },
      { "begin": "`", "end": "`", "name": "string.quoted.template.js",
        "patterns": [{ "match": "\\\\.", "name": "constant.character.escape.js" }] },
      { "begin": "\"", "end": "\"", "name": "string.quoted.double.js",
        "patterns": [{ "match": "\\\\.", "name": "constant.character.escape.js" }] },
      { "begin": "'", "end": "'", "name": "string.quoted.single.js",
        "patterns": [{ "match": "\\\\.", "name": "constant.character.escape.js" }] },
      { "match": "\\b(async|await|break|case|catch|class|const|continue|debugger|default|delete|do|else|export|extends|finally|for|from|function|if|import|in|instanceof|let|new|of|return|static|super|switch|this|throw|try|typeof|var|void|while|with|yield)\\b", "name": "keyword.control.js" },
      { "match": "\\b(true|false|null|undefined|NaN|Infinity)\\b", "name": "constant.language.js" },
      { "match": "\\b(Array|Boolean|Date|Error|Function|JSON|Map|Math|Number|Object|Promise|Proxy|RegExp|Set|String|Symbol|WeakMap|WeakSet|console|document|window)\\b", "name": "entity.name.type.js" },
      { "match": "\\b0[xX][0-9a-fA-F_]+n?\\b", "name": "constant.numeric.hex.js" },
      { "match": "\\b\\d+\\.\\d+([eE][+-]?\\d+)?\\b", "name": "constant.numeric.float.js" },
      { "match": "\\b\\d+n?\\b", "name": "constant.numeric.integer.js" },
      { "match": "=>|===|!==|==|!=|<=|>=|&&|\\|\\||\\?\\?|\\.\\.\\.", "name": "keyword.operator.js" }
    ]
  }""",

    "typescript" -> """{
    "scopeName": "source.ts",
    "patterns": [
      { "match": "//.*$", "name": "comment.line.ts" },
      { "begin": "/\\*", "end": "\\*/", "name": "comment.block.ts" },
      { "begin": "`", "end": "`", "name": "string.quoted.template.ts",
        "patterns": [{ "match": "\\\\.", "name": "constant.character.escape.ts" }] },
      { "begin": "\"", "end": "\"", "name": "string.quoted.double.ts",
        "patterns": [{ "match": "\\\\.", "name": "constant.character.escape.ts" }] },
      { "begin": "'", "end": "'", "name": "string.quoted.single.ts",
        "patterns": [{ "match": "\\\\.", "name": "constant.character.escape.ts" }] },
      { "match": "\\b(abstract|as|async|await|break|case|catch|class|const|continue|debugger|declare|default|delete|do|else|enum|export|extends|finally|for|from|function|if|implements|import|in|instanceof|interface|is|keyof|let|module|namespace|new|of|override|private|protected|public|readonly|return|satisfies|static|super|switch|this|throw|try|type|typeof|var|void|while|with|yield)\\b", "name": "keyword.control.ts" },
      { "match": "\\b(true|false|null|undefined|NaN|Infinity)\\b", "name": "constant.language.ts" },
      { "match": "\\b(any|boolean|bigint|never|number|object|string|symbol|unknown|void)\\b", "name": "entity.name.type.ts" },
      { "match": "\\b0[xX][0-9a-fA-F_]+n?\\b", "name": "constant.numeric.hex.ts" },
      { "match": "\\b\\d+\\.\\d+([eE][+-]?\\d+)?\\b", "name": "constant.numeric.float.ts" },
      { "match": "\\b\\d+n?\\b", "name": "constant.numeric.integer.ts" },
      { "match": "=>|===|!==|==|!=|<=|>=|&&|\\|\\||\\?\\?|\\.\\.\\.", "name": "keyword.operator.ts" }
    ]
  }""",

    "python" -> """{
    "scopeName": "source.python",
    "patterns": [
      { "match": "#.*$", "name": "comment.line.python" },
      { "begin": "\"\"\"", "end": "\"\"\"", "name": "string.quoted.triple.double.python" },
      { "begin": "'''", "end": "'''", "name": "string.quoted.triple.single.python" },
      { "begin": "\"", "end": "\"", "name": "string.quoted.double.python",
        "patterns": [{ "match": "\\\\.", "name": "constant.character.escape.python" }] },
      { "begin": "'", "end": "'", "name": "string.quoted.single.python",
        "patterns": [{ "match": "\\\\.", "name": "constant.character.escape.python" }] },
      { "match": "\\b(and|as|assert|async|await|break|class|continue|def|del|elif|else|except|finally|for|from|global|if|import|in|is|lambda|nonlocal|not|or|pass|raise|return|try|while|with|yield)\\b", "name": "keyword.control.python" },
      { "match": "\\b(True|False|None)\\b", "name": "constant.language.python" },
      { "match": "\\b(int|float|str|bool|list|dict|set|tuple|bytes|type|object|range|complex|frozenset|bytearray|memoryview)\\b", "name": "entity.name.type.python" },
      { "match": "\\b(print|len|range|enumerate|zip|map|filter|sorted|reversed|isinstance|hasattr|getattr|setattr|super|property|classmethod|staticmethod|input|open)\\b", "name": "support.function.python" },
      { "match": "@\\w+", "name": "entity.name.function.python" },
      { "match": "\\b0[xX][0-9a-fA-F_]+\\b", "name": "constant.numeric.hex.python" },
      { "match": "\\b\\d+\\.\\d+([eE][+-]?\\d+)?j?\\b", "name": "constant.numeric.float.python" },
      { "match": "\\b\\d[\\d_]*j?\\b", "name": "constant.numeric.integer.python" }
    ]
  }""",

    "java" -> """{
    "scopeName": "source.java",
    "patterns": [
      { "match": "//.*$", "name": "comment.line.java" },
      { "begin": "/\\*", "end": "\\*/", "name": "comment.block.java" },
      { "begin": "\"", "end": "\"", "name": "string.quoted.double.java",
        "patterns": [{ "match": "\\\\.", "name": "constant.character.escape.java" }] },
      { "begin": "'", "end": "'", "name": "string.quoted.single.java",
        "patterns": [{ "match": "\\\\.", "name": "constant.character.escape.java" }] },
      { "match": "\\b(abstract|assert|break|case|catch|class|continue|default|do|else|enum|extends|final|finally|for|if|implements|import|instanceof|interface|native|new|package|private|protected|public|record|return|sealed|static|strictfp|super|switch|synchronized|this|throw|throws|transient|try|var|void|volatile|while|yield|permits)\\b", "name": "keyword.control.java" },
      { "match": "\\b(true|false|null)\\b", "name": "constant.language.java" },
      { "match": "\\b(boolean|byte|char|double|float|int|long|short|String|Object|Integer|Long|Double|Float|Boolean|Character|Byte|Short|List|Map|Set|Optional|Stream|Collection|ArrayList|HashMap|HashSet)\\b", "name": "entity.name.type.java" },
      { "match": "@\\w+", "name": "entity.name.function.java" },
      { "match": "\\b0[xX][0-9a-fA-F_]+[lL]?\\b", "name": "constant.numeric.hex.java" },
      { "match": "\\b\\d[\\d_]*\\.\\d[\\d_]*([eE][+-]?\\d+)?[fFdD]?\\b", "name": "constant.numeric.float.java" },
      { "match": "\\b\\d[\\d_]*[lL]?\\b", "name": "constant.numeric.integer.java" }
    ]
  }""",

    "kotlin" -> """{
    "scopeName": "source.kotlin",
    "patterns": [
      { "match": "//.*$", "name": "comment.line.kotlin" },
      { "begin": "/\\*", "end": "\\*/", "name": "comment.block.kotlin" },
      { "begin": "\"\"\"", "end": "\"\"\"", "name": "string.quoted.triple.kotlin" },
      { "begin": "\"", "end": "\"", "name": "string.quoted.double.kotlin",
        "patterns": [{ "match": "\\\\.", "name": "constant.character.escape.kotlin" }] },
      { "begin": "'", "end": "'", "name": "string.quoted.single.kotlin" },
      { "match": "\\b(abstract|actual|annotation|as|break|by|catch|class|companion|const|constructor|continue|crossinline|data|delegate|do|else|enum|expect|external|final|finally|for|fun|if|import|in|infix|init|inline|inner|interface|internal|is|lateinit|noinline|object|open|operator|out|override|package|private|protected|public|reified|return|sealed|super|suspend|tailrec|this|throw|try|typealias|val|var|vararg|when|where|while)\\b", "name": "keyword.control.kotlin" },
      { "match": "\\b(true|false|null)\\b", "name": "constant.language.kotlin" },
      { "match": "\\b(Boolean|Byte|Char|Double|Float|Int|Long|Short|String|Unit|Any|Nothing|Array|List|Map|Set|Pair|Triple)\\b", "name": "entity.name.type.kotlin" },
      { "match": "\\b0[xX][0-9a-fA-F_]+[lL]?\\b", "name": "constant.numeric.hex.kotlin" },
      { "match": "\\b\\d[\\d_]*\\.\\d[\\d_]*([eE][+-]?\\d+)?[fF]?\\b", "name": "constant.numeric.float.kotlin" },
      { "match": "\\b\\d[\\d_]*[lL]?\\b", "name": "constant.numeric.integer.kotlin" }
    ]
  }""",

    "rust" -> """{
    "scopeName": "source.rust",
    "patterns": [
      { "match": "//.*$", "name": "comment.line.rust" },
      { "begin": "/\\*", "end": "\\*/", "name": "comment.block.rust" },
      { "begin": "\"", "end": "\"", "name": "string.quoted.double.rust",
        "patterns": [{ "match": "\\\\.", "name": "constant.character.escape.rust" }] },
      { "begin": "'[a-zA-Z_]", "end": "", "name": "storage.modifier.rust" },
      { "match": "\\b(as|async|await|break|const|continue|crate|dyn|else|enum|extern|fn|for|if|impl|in|let|loop|match|mod|move|mut|pub|ref|return|self|Self|static|struct|super|trait|type|union|unsafe|use|where|while|yield)\\b", "name": "keyword.control.rust" },
      { "match": "\\b(true|false)\\b", "name": "constant.language.rust" },
      { "match": "\\b(bool|char|f32|f64|i8|i16|i32|i64|i128|isize|u8|u16|u32|u64|u128|usize|str|String|Vec|Box|Rc|Arc|Option|Result|Some|None|Ok|Err|HashMap|HashSet|BTreeMap|BTreeSet)\\b", "name": "entity.name.type.rust" },
      { "match": "\\b(println!|print!|format!|vec!|panic!|assert!|assert_eq!|assert_ne!|todo!|unimplemented!|unreachable!|dbg!|eprintln!|eprint!|write!|writeln!)\\b", "name": "support.function.rust" },
      { "match": "#\\[\\w+", "name": "entity.name.function.rust" },
      { "match": "\\b0[xX][0-9a-fA-F_]+\\b", "name": "constant.numeric.hex.rust" },
      { "match": "\\b\\d[\\d_]*\\.\\d[\\d_]*([eE][+-]?\\d+)?(f32|f64)?\\b", "name": "constant.numeric.float.rust" },
      { "match": "\\b\\d[\\d_]*(u8|u16|u32|u64|u128|usize|i8|i16|i32|i64|i128|isize)?\\b", "name": "constant.numeric.integer.rust" },
      { "match": "=>|->|::", "name": "keyword.operator.rust" }
    ]
  }""",

    "go" -> """{
    "scopeName": "source.go",
    "patterns": [
      { "match": "//.*$", "name": "comment.line.go" },
      { "begin": "/\\*", "end": "\\*/", "name": "comment.block.go" },
      { "begin": "`", "end": "`", "name": "string.quoted.raw.go" },
      { "begin": "\"", "end": "\"", "name": "string.quoted.double.go",
        "patterns": [{ "match": "\\\\.", "name": "constant.character.escape.go" }] },
      { "match": "\\b(break|case|chan|const|continue|default|defer|else|fallthrough|for|func|go|goto|if|import|interface|map|package|range|return|select|struct|switch|type|var)\\b", "name": "keyword.control.go" },
      { "match": "\\b(true|false|nil|iota)\\b", "name": "constant.language.go" },
      { "match": "\\b(bool|byte|complex64|complex128|error|float32|float64|int|int8|int16|int32|int64|rune|string|uint|uint8|uint16|uint32|uint64|uintptr|any)\\b", "name": "entity.name.type.go" },
      { "match": "\\b(append|cap|close|copy|delete|imag|len|make|new|panic|print|println|real|recover)\\b", "name": "support.function.go" },
      { "match": "\\b0[xX][0-9a-fA-F_]+\\b", "name": "constant.numeric.hex.go" },
      { "match": "\\b\\d+\\.\\d+([eE][+-]?\\d+)?\\b", "name": "constant.numeric.float.go" },
      { "match": "\\b\\d+\\b", "name": "constant.numeric.integer.go" },
      { "match": ":=|<-", "name": "keyword.operator.go" }
    ]
  }""",

    "c" -> """{
    "scopeName": "source.c",
    "patterns": [
      { "match": "//.*$", "name": "comment.line.c" },
      { "begin": "/\\*", "end": "\\*/", "name": "comment.block.c" },
      { "begin": "\"", "end": "\"", "name": "string.quoted.double.c",
        "patterns": [{ "match": "\\\\.", "name": "constant.character.escape.c" }] },
      { "begin": "'", "end": "'", "name": "string.quoted.single.c",
        "patterns": [{ "match": "\\\\.", "name": "constant.character.escape.c" }] },
      { "match": "#\\s*(include|define|undef|ifdef|ifndef|if|else|elif|endif|pragma|error|warning|line)\\b", "name": "keyword.control.c" },
      { "match": "\\b(auto|break|case|const|continue|default|do|else|enum|extern|for|goto|if|inline|register|restrict|return|sizeof|static|struct|switch|typedef|union|volatile|while|_Alignas|_Alignof|_Atomic|_Bool|_Complex|_Generic|_Imaginary|_Noreturn|_Static_assert|_Thread_local)\\b", "name": "keyword.control.c" },
      { "match": "\\b(true|false|NULL|EOF)\\b", "name": "constant.language.c" },
      { "match": "\\b(void|char|short|int|long|float|double|signed|unsigned|size_t|ptrdiff_t|int8_t|int16_t|int32_t|int64_t|uint8_t|uint16_t|uint32_t|uint64_t|bool|FILE)\\b", "name": "entity.name.type.c" },
      { "match": "\\b(printf|fprintf|sprintf|scanf|malloc|calloc|realloc|free|memcpy|memset|strlen|strcmp|strcpy|strcat|fopen|fclose|fread|fwrite|exit|abort|assert)\\b", "name": "support.function.c" },
      { "match": "\\b0[xX][0-9a-fA-F]+[uUlL]*\\b", "name": "constant.numeric.hex.c" },
      { "match": "\\b\\d+\\.\\d+([eE][+-]?\\d+)?[fFlL]?\\b", "name": "constant.numeric.float.c" },
      { "match": "\\b\\d+[uUlL]*\\b", "name": "constant.numeric.integer.c" },
      { "match": "->|\\+\\+|--|<<|>>|&&|\\|\\|", "name": "keyword.operator.c" }
    ]
  }""",

    "cpp" -> """{
    "scopeName": "source.cpp",
    "patterns": [
      { "match": "//.*$", "name": "comment.line.cpp" },
      { "begin": "/\\*", "end": "\\*/", "name": "comment.block.cpp" },
      { "begin": "\"", "end": "\"", "name": "string.quoted.double.cpp",
        "patterns": [{ "match": "\\\\.", "name": "constant.character.escape.cpp" }] },
      { "begin": "'", "end": "'", "name": "string.quoted.single.cpp" },
      { "match": "#\\s*(include|define|undef|ifdef|ifndef|if|else|elif|endif|pragma)\\b", "name": "keyword.control.cpp" },
      { "match": "\\b(alignas|alignof|auto|break|case|catch|class|concept|const|consteval|constexpr|constinit|const_cast|continue|co_await|co_return|co_yield|decltype|default|delete|do|dynamic_cast|else|enum|explicit|export|extern|for|friend|goto|if|inline|mutable|namespace|new|noexcept|operator|private|protected|public|register|reinterpret_cast|requires|return|sizeof|static|static_assert|static_cast|struct|switch|template|this|throw|try|typedef|typeid|typename|union|using|virtual|volatile|while)\\b", "name": "keyword.control.cpp" },
      { "match": "\\b(true|false|nullptr|NULL)\\b", "name": "constant.language.cpp" },
      { "match": "\\b(void|bool|char|char8_t|char16_t|char32_t|wchar_t|short|int|long|float|double|signed|unsigned|size_t|string|vector|map|set|array|list|deque|queue|stack|pair|tuple|optional|variant|shared_ptr|unique_ptr|weak_ptr)\\b", "name": "entity.name.type.cpp" },
      { "match": "\\b(std|cout|cin|cerr|endl|move|forward|make_shared|make_unique|make_pair|make_tuple|sort|find|begin|end|push_back|emplace_back|size|empty)\\b", "name": "support.function.cpp" },
      { "match": "\\b0[xX][0-9a-fA-F]+[uUlL]*\\b", "name": "constant.numeric.hex.cpp" },
      { "match": "\\b\\d+\\.\\d+([eE][+-]?\\d+)?[fFlL]?\\b", "name": "constant.numeric.float.cpp" },
      { "match": "\\b\\d+[uUlL]*\\b", "name": "constant.numeric.integer.cpp" },
      { "match": "->|::|\\+\\+|--|<<|>>|&&|\\|\\||<=>", "name": "keyword.operator.cpp" }
    ]
  }""",

    "ruby" -> """{
    "scopeName": "source.ruby",
    "patterns": [
      { "match": "#.*$", "name": "comment.line.ruby" },
      { "begin": "=begin", "end": "=end", "name": "comment.block.ruby" },
      { "begin": "\"", "end": "\"", "name": "string.quoted.double.ruby",
        "patterns": [{ "match": "\\\\.", "name": "constant.character.escape.ruby" }] },
      { "begin": "'", "end": "'", "name": "string.quoted.single.ruby" },
      { "match": "\\b(alias|and|begin|break|case|class|def|defined\\?|do|else|elsif|end|ensure|for|if|in|module|next|not|or|redo|rescue|retry|return|self|super|then|undef|unless|until|when|while|yield|require|require_relative|include|extend|prepend|attr_accessor|attr_reader|attr_writer|private|protected|public|raise|puts|print|p)\\b", "name": "keyword.control.ruby" },
      { "match": "\\b(true|false|nil|__FILE__|__LINE__|__dir__)\\b", "name": "constant.language.ruby" },
      { "match": ":\\w+", "name": "constant.language.ruby" },
      { "match": "@{1,2}\\w+", "name": "variable.other.ruby" },
      { "match": "\\$\\w+", "name": "variable.other.ruby" },
      { "match": "\\b0[xX][0-9a-fA-F_]+\\b", "name": "constant.numeric.hex.ruby" },
      { "match": "\\b\\d+\\.\\d+([eE][+-]?\\d+)?\\b", "name": "constant.numeric.float.ruby" },
      { "match": "\\b\\d[\\d_]*\\b", "name": "constant.numeric.integer.ruby" },
      { "match": "=>|->|\\|\\||&&|\\.\\.", "name": "keyword.operator.ruby" }
    ]
  }""",

    "swift" -> """{
    "scopeName": "source.swift",
    "patterns": [
      { "match": "//.*$", "name": "comment.line.swift" },
      { "begin": "/\\*", "end": "\\*/", "name": "comment.block.swift" },
      { "begin": "\"\"\"", "end": "\"\"\"", "name": "string.quoted.triple.swift" },
      { "begin": "\"", "end": "\"", "name": "string.quoted.double.swift",
        "patterns": [{ "match": "\\\\.", "name": "constant.character.escape.swift" }] },
      { "match": "\\b(associatedtype|break|case|catch|class|continue|default|defer|deinit|do|else|enum|extension|fallthrough|for|func|guard|if|import|in|init|inout|internal|is|let|mutating|nonmutating|open|operator|override|private|protocol|public|repeat|rethrows|return|self|Self|static|struct|subscript|super|switch|throw|throws|try|typealias|var|where|while|async|await|actor)\\b", "name": "keyword.control.swift" },
      { "match": "\\b(true|false|nil)\\b", "name": "constant.language.swift" },
      { "match": "\\b(Any|AnyObject|Bool|Character|Double|Float|Int|Int8|Int16|Int32|Int64|Optional|String|UInt|UInt8|UInt16|UInt32|UInt64|Void|Array|Dictionary|Set|Result)\\b", "name": "entity.name.type.swift" },
      { "match": "@\\w+", "name": "entity.name.function.swift" },
      { "match": "\\b0[xX][0-9a-fA-F_]+\\b", "name": "constant.numeric.hex.swift" },
      { "match": "\\b\\d[\\d_]*\\.\\d[\\d_]*([eE][+-]?\\d+)?\\b", "name": "constant.numeric.float.swift" },
      { "match": "\\b\\d[\\d_]*\\b", "name": "constant.numeric.integer.swift" }
    ]
  }""",

    "bash" -> """{
    "scopeName": "source.bash",
    "patterns": [
      { "match": "#.*$", "name": "comment.line.bash" },
      { "begin": "\"", "end": "\"", "name": "string.quoted.double.bash",
        "patterns": [{ "match": "\\\\.", "name": "constant.character.escape.bash" }] },
      { "begin": "'", "end": "'", "name": "string.quoted.single.bash" },
      { "match": "\\b(if|then|else|elif|fi|for|while|until|do|done|case|esac|in|function|return|local|export|unset|readonly|declare|typeset|source|eval|exec|exit|break|continue|shift|trap|set|shopt)\\b", "name": "keyword.control.bash" },
      { "match": "\\b(true|false)\\b", "name": "constant.language.bash" },
      { "match": "\\b(echo|printf|read|test|cd|ls|cp|mv|rm|mkdir|rmdir|cat|grep|sed|awk|find|sort|uniq|wc|head|tail|cut|tr|xargs|chmod|chown|kill|ps|curl|wget)\\b", "name": "support.function.bash" },
      { "match": "\\$\\{?[a-zA-Z_]\\w*\\}?", "name": "variable.other.bash" },
      { "match": "\\$[0-9@#\\?\\$!*-]", "name": "variable.other.bash" },
      { "match": "\\b\\d+\\b", "name": "constant.numeric.integer.bash" },
      { "match": "\\|\\||&&|;;|<<|>>|[|&;]", "name": "keyword.operator.bash" }
    ]
  }""",

    "sql" -> """{
    "scopeName": "source.sql",
    "patterns": [
      { "match": "--.*$", "name": "comment.line.sql" },
      { "begin": "/\\*", "end": "\\*/", "name": "comment.block.sql" },
      { "begin": "'", "end": "'", "name": "string.quoted.single.sql",
        "patterns": [{ "match": "''", "name": "constant.character.escape.sql" }] },
      { "begin": "\"", "end": "\"", "name": "string.quoted.double.sql" },
      { "match": "(?i)\\b(SELECT|FROM|WHERE|AND|OR|NOT|IN|IS|NULL|AS|ON|JOIN|INNER|LEFT|RIGHT|OUTER|FULL|CROSS|UNION|ALL|DISTINCT|ORDER|BY|GROUP|HAVING|LIMIT|OFFSET|INSERT|INTO|VALUES|UPDATE|SET|DELETE|CREATE|ALTER|DROP|TABLE|INDEX|VIEW|DATABASE|SCHEMA|PRIMARY|KEY|FOREIGN|REFERENCES|UNIQUE|CHECK|DEFAULT|CONSTRAINT|CASCADE|EXISTS|BETWEEN|LIKE|CASE|WHEN|THEN|ELSE|END|BEGIN|COMMIT|ROLLBACK|TRANSACTION|GRANT|REVOKE|WITH|RECURSIVE|RETURNING|EXPLAIN|ANALYZE|VACUUM|TRUNCATE)\\b", "name": "keyword.control.sql" },
      { "match": "(?i)\\b(INTEGER|INT|SMALLINT|BIGINT|DECIMAL|NUMERIC|FLOAT|REAL|DOUBLE|CHAR|VARCHAR|TEXT|BLOB|DATE|TIME|TIMESTAMP|DATETIME|BOOLEAN|SERIAL|UUID|JSON|JSONB|ARRAY)\\b", "name": "entity.name.type.sql" },
      { "match": "(?i)\\b(COUNT|SUM|AVG|MIN|MAX|COALESCE|NULLIF|CAST|CONVERT|IFNULL|NVL|CONCAT|SUBSTRING|TRIM|UPPER|LOWER|LENGTH|ROUND|ABS|NOW|CURRENT_TIMESTAMP|CURRENT_DATE|ROW_NUMBER|RANK|DENSE_RANK|LAG|LEAD)\\b", "name": "support.function.sql" },
      { "match": "\\b(TRUE|FALSE|NULL)\\b", "name": "constant.language.sql" },
      { "match": "\\b\\d+\\.\\d+\\b", "name": "constant.numeric.float.sql" },
      { "match": "\\b\\d+\\b", "name": "constant.numeric.integer.sql" },
      { "match": "<=|>=|<>|!=|:=|\\|\\|", "name": "keyword.operator.sql" }
    ]
  }""",

    "json" -> """{
    "scopeName": "source.json",
    "patterns": [
      { "begin": "\"", "end": "\"", "name": "string.quoted.double.json",
        "patterns": [{ "match": "\\\\.", "name": "constant.character.escape.json" }] },
      { "match": "\\b(true|false|null)\\b", "name": "constant.language.json" },
      { "match": "-?\\d+\\.\\d+([eE][+-]?\\d+)?\\b", "name": "constant.numeric.float.json" },
      { "match": "-?\\d+\\b", "name": "constant.numeric.integer.json" },
      { "match": "[{}\\[\\]:,]", "name": "punctuation.json" }
    ]
  }""",

    "html" -> """{
    "scopeName": "source.html",
    "patterns": [
      { "begin": "<!--", "end": "-->", "name": "comment.block.html" },
      { "begin": "<script", "end": "</script>", "name": "meta.script.html",
        "patterns": [{ "include": "$self" }] },
      { "begin": "<style", "end": "</style>", "name": "meta.style.html" },
      { "match": "</[a-zA-Z][a-zA-Z0-9-]*>", "name": "entity.name.type.html" },
      { "match": "<[a-zA-Z][a-zA-Z0-9-]*", "name": "entity.name.type.html" },
      { "match": "/?>", "name": "entity.name.type.html" },
      { "match": "\\b[a-zA-Z-]+=", "name": "entity.name.function.html" },
      { "begin": "\"", "end": "\"", "name": "string.quoted.double.html" },
      { "begin": "'", "end": "'", "name": "string.quoted.single.html" },
      { "match": "&[a-zA-Z]+;|&#\\d+;|&#x[0-9a-fA-F]+;", "name": "constant.character.escape.html" }
    ]
  }""",

    "css" -> """{
    "scopeName": "source.css",
    "patterns": [
      { "begin": "/\\*", "end": "\\*/", "name": "comment.block.css" },
      { "begin": "\"", "end": "\"", "name": "string.quoted.double.css" },
      { "begin": "'", "end": "'", "name": "string.quoted.single.css" },
      { "match": "@(import|media|charset|font-face|keyframes|supports|page|namespace|layer|container|property|scope)\\b", "name": "keyword.control.css" },
      { "match": ":(hover|active|focus|visited|first-child|last-child|nth-child|not|before|after|root|empty|checked|disabled|enabled|required|valid|invalid|placeholder|first-line|first-letter|selection)\\b", "name": "entity.name.function.css" },
      { "match": "\\.[a-zA-Z_][a-zA-Z0-9_-]*", "name": "entity.name.type.css" },
      { "match": "#[a-zA-Z_][a-zA-Z0-9_-]*", "name": "entity.name.function.css" },
      { "match": "\\b(display|position|margin|padding|border|width|height|color|background|font|text|flex|grid|align|justify|overflow|opacity|transform|transition|animation|z-index|box-shadow|cursor|content|visibility|white-space|line-height|max-width|min-width|max-height|min-height|gap|top|right|bottom|left|float|clear)\\b", "name": "support.function.css" },
      { "match": "\\b(none|auto|inherit|initial|unset|block|inline|flex|grid|absolute|relative|fixed|sticky|solid|dashed|dotted|hidden|visible|scroll|center|normal|bold|italic)\\b", "name": "constant.language.css" },
      { "match": "#[0-9a-fA-F]{3,8}\\b", "name": "constant.numeric.hex.css" },
      { "match": "\\b\\d+\\.?\\d*(px|em|rem|%|vh|vw|vmin|vmax|ch|ex|cm|mm|in|pt|pc|deg|rad|s|ms|fr)?\\b", "name": "constant.numeric.css" },
      { "match": "!important\\b", "name": "keyword.control.css" }
    ]
  }""",

    "yaml" -> """{
    "scopeName": "source.yaml",
    "patterns": [
      { "match": "#.*$", "name": "comment.line.yaml" },
      { "begin": "\"", "end": "\"", "name": "string.quoted.double.yaml",
        "patterns": [{ "match": "\\\\.", "name": "constant.character.escape.yaml" }] },
      { "begin": "'", "end": "'", "name": "string.quoted.single.yaml" },
      { "match": "\\b(true|false|yes|no|on|off|null|~)\\b", "name": "constant.language.yaml" },
      { "match": "[a-zA-Z_][a-zA-Z0-9_ -]*(?=:)", "name": "entity.name.type.yaml" },
      { "match": "\\b\\d+\\.\\d+\\b", "name": "constant.numeric.float.yaml" },
      { "match": "\\b\\d+\\b", "name": "constant.numeric.integer.yaml" },
      { "match": "---", "name": "keyword.control.yaml" },
      { "match": "\\*\\w+|&\\w+", "name": "variable.other.yaml" }
    ]
  }""",

    "xml" -> """{
    "scopeName": "source.xml",
    "patterns": [
      { "begin": "<!--", "end": "-->", "name": "comment.block.xml" },
      { "begin": "<!\\[CDATA\\[", "end": "\\]\\]>", "name": "string.quoted.xml" },
      { "match": "</[a-zA-Z][a-zA-Z0-9:.-]*>", "name": "entity.name.type.xml" },
      { "match": "<\\?[a-zA-Z]+", "name": "keyword.control.xml" },
      { "match": "\\?>", "name": "keyword.control.xml" },
      { "match": "<[a-zA-Z][a-zA-Z0-9:.-]*", "name": "entity.name.type.xml" },
      { "match": "/?>", "name": "entity.name.type.xml" },
      { "match": "\\b[a-zA-Z:][a-zA-Z0-9:.-]*=", "name": "entity.name.function.xml" },
      { "begin": "\"", "end": "\"", "name": "string.quoted.double.xml" },
      { "begin": "'", "end": "'", "name": "string.quoted.single.xml" },
      { "match": "&[a-zA-Z]+;|&#\\d+;|&#x[0-9a-fA-F]+;", "name": "constant.character.escape.xml" }
    ]
  }""",

    "markdown" -> """{
    "scopeName": "source.markdown",
    "patterns": [
      { "match": "^#{1,6}\\s+.*$", "name": "entity.name.type.markdown" },
      { "match": "\\*\\*[^*]+\\*\\*", "name": "keyword.control.markdown" },
      { "match": "\\*[^*]+\\*", "name": "keyword.control.markdown" },
      { "begin": "`", "end": "`", "name": "string.quoted.markdown" },
      { "match": "^\\s*[-*+]\\s", "name": "punctuation.markdown" },
      { "match": "^\\s*\\d+\\.\\s", "name": "punctuation.markdown" },
      { "match": "\\[([^\\]]+)\\]\\([^)]+\\)", "name": "entity.name.function.markdown" },
      { "match": "^---+$", "name": "keyword.control.markdown" },
      { "match": "^>\\s", "name": "keyword.control.markdown" }
    ]
  }""",

    "asm" -> """{
    "scopeName": "source.asm",
    "patterns": [
      { "match": ";.*$", "name": "comment.line.asm" },
      { "begin": "\"", "end": "\"", "name": "string.quoted.double.asm",
        "patterns": [{ "match": "\\\\.", "name": "constant.character.escape.asm" }] },
      { "match": "\\b(ldi|sli|sti|ld|st|add|sub|and|or|xor|shl|shr|sra|addi|beq|blu|bls|jalr|halt|trap|rte|wfi|nop|push|pop|mov|not|neg|cmp|cmpu|mul|div|rem|mulu|divu|remu|cli|sti|gusp|susp|spsr)\\b", "name": "keyword.control.asm" },
      { "match": "\\b(r[0-7]|sp|lr|fp|pc|psr|usp)\\b", "name": "variable.other.asm" },
      { "match": "\\b(dd|dw|db|ds|resb|resw|resd|align|global|extern|section)\\b", "name": "keyword.control.asm" },
      { "match": "\\b0[xX][0-9a-fA-F]+\\b", "name": "constant.numeric.hex.asm" },
      { "match": "\\b0[bB][01]+\\b", "name": "constant.numeric.binary.asm" },
      { "match": "\\b\\d+\\b", "name": "constant.numeric.integer.asm" },
      { "match": "^[a-zA-Z_]\\w*:", "name": "entity.name.function.asm" }
    ]
  }""",
  )

  val aliases: Map[String, String] = Map(
    "js"          -> "javascript",
    "jsx"         -> "javascript",
    "ts"          -> "typescript",
    "tsx"         -> "typescript",
    "py"          -> "python",
    "rb"          -> "ruby",
    "sh"          -> "bash",
    "shell"       -> "bash",
    "zsh"         -> "bash",
    "c++"         -> "cpp",
    "cxx"         -> "cpp",
    "yml"         -> "yaml",
    "md"          -> "markdown",
    "htm"         -> "html",
    "postgresql"  -> "sql",
    "postgres"    -> "sql",
    "mysql"       -> "sql",
    "sqlite"      -> "sql",
    "objc"        -> "c",
    "objective-c" -> "c",
    "lsysl"       -> "sysl",
    "assembly"    -> "asm",
    "trisc"       -> "asm",
  )
