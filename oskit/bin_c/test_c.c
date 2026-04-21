/* test_c — minimal C program for SLIX.
 *
 * Exercises the C crt0 (c_crt0.c) by printing argv[1..] joined with
 * spaces. Returns argc as exit code so callers can observe the
 * received argument count.
 */

extern int putchar(int c);

static void puts_no_nl(const char *s) {
    while (*s) putchar((unsigned char)*s++);
}

int main(int argc, char **argv) {
    puts_no_nl("argc=");
    putchar('0' + (argc % 10));
    putchar(' ');
    for (int i = 1; i < argc; i++) {
        if (i > 1) putchar(' ');
        puts_no_nl(argv[i]);
    }
    putchar('\n');
    return argc;
}
