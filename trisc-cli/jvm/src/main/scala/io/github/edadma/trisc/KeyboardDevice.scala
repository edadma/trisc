package io.github.edadma.trisc

import java.awt.event.KeyEvent
import java.util.concurrent.ConcurrentLinkedQueue

class KeyboardDevice(val base: Long, intc: InterruptController, irq: Int) extends Device:
  val name = "Keyboard"
  val size = 4

  private val STATUS = 0
  private val SCANCODE = 1
  private val FLAGS = 2
  private val MODIFIERS = 3

  // Event: (scancode, press, modifiers)
  private case class KeyEvt(scancode: Int, press: Boolean, modifiers: Int)

  private val queue = new ConcurrentLinkedQueue[KeyEvt]
  private var overflow = false

  // Latched state from current event (valid after STATUS read, until next SCANCODE read)
  private var currentScancode: Int = 0
  private var currentFlags: Int = 0
  private var currentModifiers: Int = 0
  private var ready: Boolean = false

  // Press+release per key → 2 slots per character. Long scripted input (e.g. tests
  // typing many shell lines) must not overflow before the guest drains the queue.
  private val MAX_QUEUE = 256

  def enqueue(vkCode: Int, press: Boolean, shiftDown: Boolean, ctrlDown: Boolean, altDown: Boolean, metaDown: Boolean): Unit =
    val scancode = KeyboardDevice.vkToHid(vkCode)
    if scancode != 0 then // 0 = unknown key, ignore
      val mods = (if shiftDown then 1 else 0) |
        (if ctrlDown then 2 else 0) |
        (if altDown then 4 else 0) |
        (if metaDown then 8 else 0)
      if queue.size() >= MAX_QUEUE then
        overflow = true
      else
        queue.add(KeyEvt(scancode, press, mods))
      intc.raise(irq)

  private def advance(): Unit =
    val evt = queue.poll()
    if evt != null then
      ready = true
      currentScancode = evt.scancode
      currentFlags = if evt.press then 1 else 0
      currentModifiers = evt.modifiers
    else
      ready = false
      currentScancode = 0
      currentFlags = 0
      currentModifiers = 0

  // Latched copies of the consumed event's flags/mods, preserved
  // across the advance() that pre-loads the next event after SCANCODE read.
  private var latchedFlags: Int = 0
  private var latchedModifiers: Int = 0

  def readByte(addr: Long): Int =
    (addr - base).toInt match
      case STATUS =>
        if !ready then advance() // peek at next event
        (if ready then 1 else 0) | (if overflow then 2 else 0)
      case SCANCODE =>
        if !ready then advance()
        val sc = currentScancode
        latchedFlags = currentFlags
        latchedModifiers = currentModifiers
        ready = false // consume this event
        intc.lower(irq)
        // Pre-load next event so STATUS is immediately correct
        advance()
        sc
      case FLAGS => latchedFlags
      case MODIFIERS => latchedModifiers
      case _ => 0

  def writeByte(addr: Long, data: Long): Unit =
    (addr - base).toInt match
      case STATUS =>
        overflow = false // writing STATUS clears overflow flag
      case _ => ()

object KeyboardDevice:
  // Java VK_* to USB HID Usage Code mapping (HID Usage Tables, Page 0x07)
  val vkToHid: Map[Int, Int] = Map(
    // Letters A-Z → 0x04-0x1D
    KeyEvent.VK_A -> 0x04, KeyEvent.VK_B -> 0x05, KeyEvent.VK_C -> 0x06,
    KeyEvent.VK_D -> 0x07, KeyEvent.VK_E -> 0x08, KeyEvent.VK_F -> 0x09,
    KeyEvent.VK_G -> 0x0A, KeyEvent.VK_H -> 0x0B, KeyEvent.VK_I -> 0x0C,
    KeyEvent.VK_J -> 0x0D, KeyEvent.VK_K -> 0x0E, KeyEvent.VK_L -> 0x0F,
    KeyEvent.VK_M -> 0x10, KeyEvent.VK_N -> 0x11, KeyEvent.VK_O -> 0x12,
    KeyEvent.VK_P -> 0x13, KeyEvent.VK_Q -> 0x14, KeyEvent.VK_R -> 0x15,
    KeyEvent.VK_S -> 0x16, KeyEvent.VK_T -> 0x17, KeyEvent.VK_U -> 0x18,
    KeyEvent.VK_V -> 0x19, KeyEvent.VK_W -> 0x1A, KeyEvent.VK_X -> 0x1B,
    KeyEvent.VK_Y -> 0x1C, KeyEvent.VK_Z -> 0x1D,
    // Digits 1-9, 0 → 0x1E-0x27
    KeyEvent.VK_1 -> 0x1E, KeyEvent.VK_2 -> 0x1F, KeyEvent.VK_3 -> 0x20,
    KeyEvent.VK_4 -> 0x21, KeyEvent.VK_5 -> 0x22, KeyEvent.VK_6 -> 0x23,
    KeyEvent.VK_7 -> 0x24, KeyEvent.VK_8 -> 0x25, KeyEvent.VK_9 -> 0x26,
    KeyEvent.VK_0 -> 0x27,
    // Control keys
    KeyEvent.VK_ENTER -> 0x28,
    KeyEvent.VK_ESCAPE -> 0x29,
    KeyEvent.VK_BACK_SPACE -> 0x2A,
    KeyEvent.VK_TAB -> 0x2B,
    KeyEvent.VK_SPACE -> 0x2C,
    // Punctuation
    KeyEvent.VK_MINUS -> 0x2D,
    KeyEvent.VK_EQUALS -> 0x2E,
    KeyEvent.VK_OPEN_BRACKET -> 0x2F,
    KeyEvent.VK_CLOSE_BRACKET -> 0x30,
    KeyEvent.VK_BACK_SLASH -> 0x31,
    KeyEvent.VK_SEMICOLON -> 0x33,
    KeyEvent.VK_QUOTE -> 0x34,
    KeyEvent.VK_BACK_QUOTE -> 0x35,
    KeyEvent.VK_COMMA -> 0x36,
    KeyEvent.VK_PERIOD -> 0x37,
    KeyEvent.VK_SLASH -> 0x38,
    // Caps Lock
    KeyEvent.VK_CAPS_LOCK -> 0x39,
    // Function keys F1-F12 → 0x3A-0x45
    KeyEvent.VK_F1 -> 0x3A, KeyEvent.VK_F2 -> 0x3B, KeyEvent.VK_F3 -> 0x3C,
    KeyEvent.VK_F4 -> 0x3D, KeyEvent.VK_F5 -> 0x3E, KeyEvent.VK_F6 -> 0x3F,
    KeyEvent.VK_F7 -> 0x40, KeyEvent.VK_F8 -> 0x41, KeyEvent.VK_F9 -> 0x42,
    KeyEvent.VK_F10 -> 0x43, KeyEvent.VK_F11 -> 0x44, KeyEvent.VK_F12 -> 0x45,
    // Navigation
    KeyEvent.VK_INSERT -> 0x49,
    KeyEvent.VK_HOME -> 0x4A,
    KeyEvent.VK_PAGE_UP -> 0x4B,
    KeyEvent.VK_DELETE -> 0x4C,
    KeyEvent.VK_END -> 0x4D,
    KeyEvent.VK_PAGE_DOWN -> 0x4E,
    // Arrow keys
    KeyEvent.VK_RIGHT -> 0x4F,
    KeyEvent.VK_LEFT -> 0x50,
    KeyEvent.VK_DOWN -> 0x51,
    KeyEvent.VK_UP -> 0x52,
    // Modifiers (reported as scancodes for key-up/key-down tracking)
    KeyEvent.VK_CONTROL -> 0xE0,
    KeyEvent.VK_SHIFT -> 0xE1,
    KeyEvent.VK_ALT -> 0xE2,
    KeyEvent.VK_META -> 0xE3,
  ).withDefaultValue(0)
