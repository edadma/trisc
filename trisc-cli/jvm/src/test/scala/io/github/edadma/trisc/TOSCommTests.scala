package io.github.edadma.trisc

class TOSCommTests extends TOSTestHelpers {

  // ===== Channel tests =====

  "TOS: channel send and receive" in {
    val (_, output) = runTOS(Map(
      "app" ->
        """import "kernel"
          |import "services"
          |import "timer"
          |import "channel"
          |
          |var ch: Channel
          |var buf: [4]i64
          |
          |kernel_main() -> int
          |    chan_init(&ch, &buf[0], 4)
          |    create_thread(sender, 0x6000, 0x5000, "s")
          |    create_thread(receiver, 0x8000, 0x7000, "r")
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |sender()
          |    chan_send(&ch, 72)
          |    chan_send(&ch, 105)
          |    chan_send(&ch, 10)
          |
          |receiver()
          |    putc(int(chan_recv(&ch)))
          |    putc(int(chan_recv(&ch)))
          |    putc(int(chan_recv(&ch)))
          |""".stripMargin
    ))

    output should startWith("Hi\n")
  }

  "TOS: channel blocks sender when full" in {
    val (_, output) = runTOS(Map(
      "app" ->
        """import "kernel"
          |import "services"
          |import "timer"
          |import "channel"
          |
          |var ch: Channel
          |var buf: [2]i64
          |
          |kernel_main() -> int
          |    chan_init(&ch, &buf[0], 2)
          |    create_thread(sender, 0x6000, 0x5000, "s")
          |    create_thread(receiver, 0x8000, 0x7000, "r")
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |sender()
          |    chan_send(&ch, 65)
          |    chan_send(&ch, 66)
          |    chan_send(&ch, 67)
          |    putc('S')
          |
          |receiver()
          |    sleep(30)
          |    putc(int(chan_recv(&ch)))
          |    putc(int(chan_recv(&ch)))
          |    putc(int(chan_recv(&ch)))
          |""".stripMargin
    ))

    // Sender blocks on 3rd send until receiver drains — all values arrive
    output should include("A")
    output should include("B")
    output should include("C")
    output should include("S")
  }

  // ===== Mailbox tests =====

  "TOS: mailbox send and receive" in {
    val (_, output) = runTOS(Map(
      "app" ->
        """import "kernel"
          |import "services"
          |import "timer"
          |import "mailbox"
          |
          |var mb: Mailbox
          |
          |kernel_main() -> int
          |    mbox_init(&mb)
          |    create_thread(sender, 0x6000, 0x5000, "s")
          |    create_thread(receiver, 0x8000, 0x7000, "r")
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |sender()
          |    mbox_send(&mb, 72)
          |    mbox_send(&mb, 105)
          |
          |receiver()
          |    putc(int(mbox_recv(&mb)))
          |    putc(int(mbox_recv(&mb)))
          |    putc('\n')
          |""".stripMargin
    ))

    output should startWith("Hi\n")
  }

  "TOS: mailbox blocks sender until receiver drains" in {
    val (_, output) = runTOS(Map(
      "app" ->
        """import "kernel"
          |import "services"
          |import "timer"
          |import "mailbox"
          |
          |var mb: Mailbox
          |
          |kernel_main() -> int
          |    mbox_init(&mb)
          |    create_thread(sender, 0x6000, 0x5000, "s")
          |    create_thread(receiver, 0x8000, 0x7000, "r")
          |    timer_init(1000)
          |    first_thread_ssp()
          |
          |sender()
          |    mbox_send(&mb, 49)
          |    putc('A')
          |    mbox_send(&mb, 50)
          |    putc('B')
          |
          |receiver()
          |    sleep(20)
          |    putc(int(mbox_recv(&mb)))
          |    putc(int(mbox_recv(&mb)))
          |""".stripMargin
    ))

    // Sender sends 1, prints A, sends 2 (blocks until recv), prints B
    // Receiver wakes, receives 1, receives 2
    output should include("A")
    output should include("B")
    output should include("1")
    output should include("2")
  }
}
