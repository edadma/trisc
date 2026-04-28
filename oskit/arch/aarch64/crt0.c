/* C crt0 for SLIX programs (aarch64).
 *
 * Bridges the Sysl crt0 convention (_start -> sysl_start) to a POSIX
 * `int main(int argc, char **argv)` C entry point. For C programs, the
 * conversion is trivial: argc/argv are already POSIX-style, so
 * sysl_start just forwards unchanged.
 *
 * The arch-specific _start in prog_start.s loads argc from [sp] and
 * &argv[0] from sp+8 (System V ABI process init stack, built by PM
 * during spawn) and calls oskit_ulib__sysl_start(argc, argv). Sysl
 * programs pull that symbol from oskit/ulib/srt0.lsysl; C programs
 * get it from this file instead (link against crt0.o instead of
 * srt0.o).
 *
 * Identical to oskit/arch/x86_64/crt0.c — kept per-arch for symmetry
 * with the surrounding build pipeline; merge into a shared file once
 * arches are fully unified.
 */

extern int main(int argc, char **argv);
extern void exit(int status);

void oskit_ulib__sysl_start(int argc, char **argv) {
    int rc = main(argc, argv);
    exit(rc);
}
