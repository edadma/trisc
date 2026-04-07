package io.github.edadma.trisc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import javax.crypto.SecretKeyFactory
import javax.crypto.spec.PBEKeySpec

class Pbkdf2CompatTests extends AnyFreeSpec with Matchers {

  private def jvmPbkdf2(password: String, salt: String, iterations: Int): String =
    val spec = new PBEKeySpec(password.toCharArray, salt.getBytes("UTF-8"), iterations, 256)
    val hash = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA256").generateSecret(spec).getEncoded
    hash.map(b => f"${b & 0xff}%02x").mkString

  // Expected values from sysl's pbkdf2.lsysl test vectors (RFC 7914 / RFC 6070)
  val expectedC1    = "120fb6cffcf8b32c43e7225256c4f837a86548c92ccc35480805987cb70be17b"
  val expectedC2    = "ae4d0c95af6b46d32d0adff928f06dd02a303f8ef3c251dfd6e2d85a95474c43"
  val expectedShort = "ee3128e0a8ed80ff7d38f46e7a97a2b0a33fc494189aedafcd812acc752f14ca"
  val expectedC100  = "07e6997180cf7f12904f04100d405d34888fdf62af6d506a0ecc23b196fe99d8"

  "JVM PBKDF2 matches sysl test vector c=1" in {
    jvmPbkdf2("password", "salt", 1) shouldBe expectedC1
  }

  "JVM PBKDF2 matches sysl test vector c=2" in {
    jvmPbkdf2("password", "salt", 2) shouldBe expectedC2
  }

  "JVM PBKDF2 matches sysl test vector short" in {
    jvmPbkdf2("pass", "sa", 1) shouldBe expectedShort
  }

  "JVM PBKDF2 matches sysl test vector c=100" in {
    jvmPbkdf2("password", "salt", 100) shouldBe expectedC100
  }

  "compute hash for default root password" in {
    val hash = jvmPbkdf2("toor", "slix", 1)
    info(s"root hash: $hash")
    hash.length shouldBe 64
  }

  "SHA accelerator device: SHA-256 abc" in {
    val sha = new ShaAccelerator(0x1000L)
    // "abc" padded to 512-bit block
    val block = Array(
      0x61626380, 0x00000000, 0x00000000, 0x00000000,
      0x00000000, 0x00000000, 0x00000000, 0x00000000,
      0x00000000, 0x00000000, 0x00000000, 0x00000000,
      0x00000000, 0x00000000, 0x00000000, 0x00000018,
    )
    for i <- 0 until 16 do
      sha.writeInt(0x1000L + i * 4, block(i))
    sha.writeInt(0x1000L + 0x40, 1) // SHA_START
    val h0 = sha.readInt(0x1000L)
    info(f"TEXT[0] = 0x${h0.toLong & 0xFFFFFFFFL}%08x (expected 0xba7816bf)")
    (h0.toLong & 0xFFFFFFFFL) shouldBe 0xba7816bfL
  }
}
