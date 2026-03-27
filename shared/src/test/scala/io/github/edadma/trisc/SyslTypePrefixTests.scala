package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import SyslType.*

class SyslTypePrefixTests extends AnyFreeSpec with Matchers {

  // ===== toPrefix =====

  "i64 toPrefix" in { I64.toPrefix shouldBe "i64" }
  "i32 toPrefix" in { I32.toPrefix shouldBe "i32" }
  "i8 toPrefix" in { I8.toPrefix shouldBe "i8" }
  "bool toPrefix" in { BoolType.toPrefix shouldBe "bool" }
  "void toPrefix" in { VoidType.toPrefix shouldBe "void" }

  "ptr i64 toPrefix" in {
    PtrType(I64).toPrefix shouldBe "ptr i64"
  }

  "ptr ptr i8 toPrefix" in {
    PtrType(PtrType(I8)).toPrefix shouldBe "ptr ptr i8"
  }

  "arr 5 i64 toPrefix" in {
    ArrayType(I64, 5).toPrefix shouldBe "arr 5 i64"
  }

  "arr 10 ptr i32 toPrefix" in {
    ArrayType(PtrType(I32), 10).toPrefix shouldBe "arr 10 ptr i32"
  }

  "ptr arr 3 i8 toPrefix" in {
    PtrType(ArrayType(I8, 3)).toPrefix shouldBe "ptr arr 3 i8"
  }

  // ===== fromPrefix =====

  "int fromPrefix" in { SyslType.fromPrefix("int") shouldBe I64 }
  "char fromPrefix" in { SyslType.fromPrefix("char") shouldBe I32 }
  "byte fromPrefix" in { SyslType.fromPrefix("byte") shouldBe I8 }
  "bool fromPrefix" in { SyslType.fromPrefix("bool") shouldBe BoolType }
  "void fromPrefix" in { SyslType.fromPrefix("void") shouldBe VoidType }

  "ptr int fromPrefix" in {
    SyslType.fromPrefix("ptr int") shouldBe PtrType(I64)
  }

  "ptr ptr byte fromPrefix" in {
    SyslType.fromPrefix("ptr ptr byte") shouldBe PtrType(PtrType(I8))
  }

  "arr 5 int fromPrefix" in {
    SyslType.fromPrefix("arr 5 int") shouldBe ArrayType(I64, 5)
  }

  "arr 10 ptr char fromPrefix" in {
    SyslType.fromPrefix("arr 10 ptr char") shouldBe ArrayType(PtrType(I32), 10)
  }

  "ptr arr 3 byte fromPrefix" in {
    SyslType.fromPrefix("ptr arr 3 byte") shouldBe PtrType(ArrayType(I8, 3))
  }

  // ===== Round-trips =====

  "all base types round-trip" in {
    for t <- List(I64, I32, I8, BoolType, VoidType) do
      SyslType.fromPrefix(t.toPrefix) shouldBe t
  }

  "nested pointer round-trips" in {
    val t = PtrType(PtrType(PtrType(I64)))
    SyslType.fromPrefix(t.toPrefix) shouldBe t
  }

  "nested array round-trips" in {
    val t = ArrayType(ArrayType(I64, 3), 5)
    SyslType.fromPrefix(t.toPrefix) shouldBe t
  }

  "complex type round-trips" in {
    val t = PtrType(ArrayType(PtrType(I8), 10))
    SyslType.fromPrefix(t.toPrefix) shouldBe t
  }

  // ===== funcSigToPrefix / funcSigFromPrefix =====

  "func sig no params" in {
    val sig = SyslType.funcSigToPrefix(Nil, I64)
    sig shouldBe "0 i64"
    val (params, ret) = SyslType.funcSigFromPrefix(sig)
    params shouldBe Nil
    ret shouldBe I64
  }

  "func sig one i64 param" in {
    val sig = SyslType.funcSigToPrefix(List(I64), I64)
    sig shouldBe "1 i64 i64"
    val (params, ret) = SyslType.funcSigFromPrefix(sig)
    params shouldBe List(I64)
    ret shouldBe I64
  }

  "func sig two params" in {
    val sig = SyslType.funcSigToPrefix(List(I64, I64), I64)
    sig shouldBe "2 i64 i64 i64"
    val (params, ret) = SyslType.funcSigFromPrefix(sig)
    params shouldBe List(I64, I64)
    ret shouldBe I64
  }

  "func sig with pointer params" in {
    val sig = SyslType.funcSigToPrefix(List(PtrType(I64), PtrType(I64)), VoidType)
    sig shouldBe "2 ptr i64 ptr i64 void"
    val (params, ret) = SyslType.funcSigFromPrefix(sig)
    params shouldBe List(PtrType(I64), PtrType(I64))
    ret shouldBe VoidType
  }

  "func sig with array param" in {
    val sig = SyslType.funcSigToPrefix(List(ArrayType(I64, 5), I64), VoidType)
    sig shouldBe "2 arr 5 i64 i64 void"
    val (params, ret) = SyslType.funcSigFromPrefix(sig)
    params shouldBe List(ArrayType(I64, 5), I64)
    ret shouldBe VoidType
  }

  "func sig round-trip complex" in {
    val params = List(PtrType(ArrayType(I8, 10)), I64, PtrType(PtrType(I32)))
    val ret = PtrType(I64)
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
