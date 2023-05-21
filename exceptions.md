Exception Vectors
=================

Exception Number | Address | Definition
---------------- |---------| ----------
0| 0        |Reset (the VBR is Cleared Before Vectoring) Exception
1| 8        |Interrupt Exception
2| 10       |Instruction Access Exception
3| 18       |Data Access Exception
4| 20       |Misaligned Access Exception
5| 28       |Unimplemented Opcode Exception
6| 30       |Privilege Violation Exception
7| 38       |Bounds Check Violation Exception
8| 40       |Illegal Integer Divide Exception
9| 48       |Integer Overflow Exception
10| 50       |Error Exception
11-15| 58       |Supervisor Call Exceptions - Reserved for User Definition (Trap Vectors)
