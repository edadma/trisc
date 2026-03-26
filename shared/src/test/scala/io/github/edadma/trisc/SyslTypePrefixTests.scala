package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import SyslType.*

class SyslTypePrefixTests extends AnyFreeSpec with Matchers {

  // ===== toPrefix =====

  "int toPrefix" in { IntType.toPrefix shouldBe "int" }
  "char toPrefix" in { CharType.toPrefix shouldBe "char" }
  "byte toPrefix" in { ByteType.toPrefix shouldBe "byte" }
  "bool toPrefix" in { BoolType.toPrefix shouldBe "bool" }
  "void toPrefix" in { VoidType.toPrefix shouldBe "void" }

  "ptr int toPrefix" in {
    PtrType(IntType).toPrefix shouldBe "ptr int"
  }

  "ptr ptr byte toPrefix" in {
    PtrType(PtrType(ByteType)).toPrefix shouldBe "ptr ptr byte"
  }

  "arr 5 int toPrefix" in {
    ArrayType(IntType, 5).toPrefix shouldBe "arr 5 int"
  }

  "arr 10 ptr char toPrefix" in {
    ArrayType(PtrType(CharType), 10).toPrefix shouldBe "arr 10 ptr char"
  }

  "ptr arr 3 byte toPrefix" in {
    PtrType(ArrayType(ByteType, 3)).toPrefix shouldBe "ptr arr 3 byte"
  }

  // ===== fromPrefix =====

  "int fromPrefix" in { SyslType.fromPrefix("int") shouldBe IntType }
  "char fromPrefix" in { SyslType.fromPrefix("char") shouldBe CharType }
  "byte fromPrefix" in { SyslType.fromPrefix("byte") shouldBe ByteType }
  "bool fromPrefix" in { SyslType.fromPrefix("bool") shouldBe BoolType }
  "void fromPrefix" in { SyslType.fromPrefix("void") shouldBe VoidType }

  "ptr int fromPrefix" in {
    SyslType.fromPrefix("ptr int") shouldBe PtrType(IntType)
  }

  "ptr ptr byte fromPrefix" in {
    SyslType.fromPrefix("ptr ptr byte") shouldBe PtrType(PtrType(ByteType))
  }

  "arr 5 int fromPrefix" in {
    SyslType.fromPrefix("arr 5 int") shouldBe ArrayType(IntType, 5)
  }

  "arr 10 ptr char fromPrefix" in {
    SyslType.fromPrefix("arr 10 ptr char") shouldBe ArrayType(PtrType(CharType), 10)
  }

  "ptr arr 3 byte fromPrefix" in {
    SyslType.fromPrefix("ptr arr 3 byte") shouldBe PtrType(ArrayType(ByteType, 3))
  }

  // ===== Round-trips =====

  "all base types round-trip" in {
    for t <- List(IntType, CharType, ByteType, BoolType, VoidType) do
      SyslType.fromPrefix(t.toPrefix) shouldBe t
  }

  "nested pointer round-trips" in {
    val t = PtrType(PtrType(PtrType(IntType)))
    SyslType.fromPrefix(t.toPrefix) shouldBe t
  }

  "nested array round-trips" in {
    val t = ArrayType(ArrayType(IntType, 3), 5)
    SyslType.fromPrefix(t.toPrefix) shouldBe t
  }

  "complex type round-trips" in {
    val t = PtrType(ArrayType(PtrType(ByteType), 10))
    SyslType.fromPrefix(t.toPrefix) shouldBe t
  }

  // ===== funcSigToPrefix / funcSigFromPrefix =====

  "func sig no params" in {
    val sig = SyslType.funcSigToPrefix(Nil, IntType)
    sig shouldBe "0 int"
    val (params, ret) = SyslType.funcSigFromPrefix(sig)
    params shouldBe Nil
    ret shouldBe IntType
  }

  "func sig one int param" in {
    val sig = SyslType.funcSigToPrefix(List(IntType), IntType)
    sig shouldBe "1 int int"
    val (params, ret) = SyslType.funcSigFromPrefix(sig)
    params shouldBe List(IntType)
    ret shouldBe IntType
  }

  "func sig two params" in {
    val sig = SyslType.funcSigToPrefix(List(IntType, IntType), IntType)
    sig shouldBe "2 int int int"
    val (params, ret) = SyslType.funcSigFromPrefix(sig)
    params shouldBe List(IntType, IntType)
    ret shouldBe IntType
  }

  "func sig with pointer params" in {
    val sig = SyslType.funcSigToPrefix(List(PtrType(IntType), PtrType(IntType)), VoidType)
    sig shouldBe "2 ptr int ptr int void"
    val (params, ret) = SyslType.funcSigFromPrefix(sig)
    params shouldBe List(PtrType(IntType), PtrType(IntType))
    ret shouldBe VoidType
  }

  "func sig with array param" in {
    val sig = SyslType.funcSigToPrefix(List(ArrayType(IntType, 5), IntType), VoidType)
    sig shouldBe "2 arr 5 int int void"
    val (params, ret) = SyslType.funcSigFromPrefix(sig)
    params shouldBe List(ArrayType(IntType, 5), IntType)
    ret shouldBe VoidType
  }

  "func sig round-trip complex" in {
    val params = List(PtrType(ArrayType(ByteType, 10)), IntType, PtrType(PtrType(CharType)))
    val ret = PtrType(IntType)
    val sig = SyslType.funcSigToPrefix(params, ret)
    val (params2, ret2) = SyslType.funcSigFromPrefix(sig)
    params2 shouldBe params
    ret2 shouldBe ret
  }

  // ===== Error handling =====

  "fromPrefix rejects unknown token" in {
    an[IllegalArgumentException] should be thrownBy SyslType.fromPrefix("float")
  }
}
