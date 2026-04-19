/* C crt0 for SLIX programs.
 *
 * Bridges the Sysl crt0 convention (_start -> sysl_start) to a POSIX
 * `int main(int argc, char **argv)` C entry point. For C programs, the
 * conversion is trivial: argc/argv are already POSIX-style, so
 * sysl_start just forwards unchanged.
 *
 * The arch-specific _start in prog_start.s reads the argv blob from
 * PROG_ARGS_ADDR and calls oskit_ulib__sysl_start(argc, argv). Sysl
 * programs pull that symbol from oskit/ulib/ulib.lsysl; C programs
 * get it from this file instead (link against c_crt0.o instead of
 * ulib.o).
 */

extern int main(int argc, char **argv);
extern void exit(int status);

void oskit_ulib__sysl_start(int argc, char **argv) {
    int rc = main(argc, argv);
    exit(rc);
}
