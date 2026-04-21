# Cross-compiling GNU/POSIX software from macOS to aarch64 Linux musl

A working recipe, built up from a hello world to a real GNU autotools package
(GNU Hello), using clang + compiler-rt + musl. No GCC anywhere. Intended as
the foundation for cross-compiling the same way to SLIX once a SLIX sysroot
exists.

## Goal

Cross-compile C programs on macOS (Apple Silicon) that run unmodified on any
aarch64 Linux musl system. The long-term target is SLIX on aarch64; Chimera
Linux is the stand-in sysroot while the SLIX port is in progress.

## Why Chimera instead of Alpine

- Chimera is already clang + musl + compiler-rt + libunwind. No `libgcc`,
  no GCC startup files — matches the exact toolchain we want for SLIX.
- Alpine's musl-dev package doesn't include startup files (`crtbeginT.o`,
  `crtend.o`) on its own, so Alpine forces you to pull in GCC just to get
  those files — which you then throw away. Chimera ships clang-native
  equivalents as `clang_rt.crtbegin.o` / `clang_rt.crtend.o`.
- Chimera's package triple is `aarch64-chimera-linux-musl`, so clang's
  resource lookups land in the right directory when we pass that triple.

Static-linked binaries we produce run in any aarch64 musl environment
(Chimera, Alpine, etc.); the sysroot choice only affects what the compiler
and linker find during the build.

## Prerequisites

- macOS on Apple Silicon (arm64)
- `clang` (Apple's is fine) — tested with clang 21 / 22
- `lld` via Homebrew: `brew install lld`
- `llvm` via Homebrew (for `llvm-ar`, `llvm-ranlib`, `llvm-nm`, `llvm-strip`):
  `brew install llvm`
- Docker (Colima or Docker Desktop) running aarch64 Linux containers

## Layout used below

All experimental work lives under `/Users/ed/xtest/`:

```
xtest/
├── hello.c                         # tiny test program
├── chimera-sysroot/                # extracted Chimera sysroot (~90 MB)
├── aarch64-chimera-musl-clang      # wrapper script (CC)
├── chimera-sysroot.sh              # produces the sysroot via Docker
├── configure-hello.sh              # autotools configure incantation
├── hello-2.12.2/                   # extracted GNU Hello source
└── README.md                       # this file
```

Docker volume-mounts land at `/work` inside containers.

## Step 1: Build the sysroot from Chimera

`chimera-sysroot.sh`:

```sh
#!/bin/sh
set -e
apk update
apk add musl-devel musl-devel-static clang-rt-devel libunwind-devel linux-headers
rm -rf /work/chimera-sysroot
mkdir -p /work/chimera-sysroot/usr
cp -r /usr/include /work/chimera-sysroot/usr/
cp -r /usr/lib     /work/chimera-sysroot/usr/
if [ -d /lib ]; then cp -r /lib /work/chimera-sysroot/; fi
```

Run it:

```sh
docker run --rm -v /Users/ed/xtest:/work -w /work \
    chimeralinux/chimera:latest sh chimera-sysroot.sh
```

Result: `/Users/ed/xtest/chimera-sysroot/` ~90 MB, containing:

- `usr/include/` — musl + linux kernel headers
- `usr/lib/crt1.o`, `crti.o`, `crtn.o` — musl startup
- `usr/lib/libc.a` — static musl (2.5 MB)
- `usr/lib/clang/22/lib/aarch64-chimera-linux-musl/libclang_rt.builtins.a` —
  compiler-rt builtins (clang's equivalent of `libgcc`)
- `usr/lib/clang/22/lib/aarch64-chimera-linux-musl/clang_rt.crtbegin.o`,
  `clang_rt.crtend.o` — clang-native CRT glue
- `usr/lib/libunwind.so*` — stack unwinder

## Step 2: Compiler wrapper

Autotools (and most Makefiles) want `CC` to be a single command. Rather than
passing a dozen flags every invocation, wrap clang once.

`aarch64-chimera-musl-clang`:

```sh
#!/bin/sh
SYSROOT=/Users/ed/xtest/chimera-sysroot
exec clang \
    --target=aarch64-chimera-linux-musl \
    --sysroot="$SYSROOT" \
    -resource-dir="$SYSROOT/usr/lib/clang/22" \
    --rtlib=compiler-rt --unwindlib=none \
    -fuse-ld=/opt/homebrew/opt/lld/bin/ld.lld \
    "$@"
```

Make it executable: `chmod +x aarch64-chimera-musl-clang`.

Why these flags matter:

| Flag | Reason |
|------|--------|
| `--target=aarch64-chimera-linux-musl` | Must match the vendor string under `usr/lib/clang/22/lib/` so clang finds compiler-rt and CRT files. |
| `--sysroot` | Headers and core libraries (libc, crt*.o) come from here. |
| `-resource-dir` | Clang's resource files are versioned by clang version, independent of the sysroot. Override points clang at Chimera's 22.x resources instead of the host's 21.x. |
| `--rtlib=compiler-rt` | Use clang's native runtime (libclang_rt.builtins) instead of libgcc. |
| `--unwindlib=none` | Static builds don't need the unwinder; skip it. |
| `-fuse-ld=ld.lld` | Apple's linker can't produce Linux ELF. Use lld. |

## Step 3: Hello world cross-compile

```c
/* hello.c */
#include <stdio.h>
#include <sys/utsname.h>
int main(void) {
    struct utsname u;
    uname(&u);
    printf("hello from %s %s %s\n", u.sysname, u.machine, u.release);
    return 0;
}
```

```sh
/Users/ed/xtest/aarch64-chimera-musl-clang -static hello.c -o hello
```

Verify:

```sh
docker run --rm -v /Users/ed/xtest:/work -w /work \
    chimeralinux/chimera:latest ./hello
# hello from Linux aarch64 ...
```

Static, ~157 KB. Runs in any aarch64 musl distro (tested against both
Chimera and Alpine).

## Step 4: GNU Hello (real autotools package)

Download and extract:

```sh
curl -sSLO https://ftp.gnu.org/gnu/hello/hello-2.12.2.tar.gz
tar xzf hello-2.12.2.tar.gz
cd hello-2.12.2
```

Configure (`configure-hello.sh`):

```sh
#!/bin/sh
set -e
cd /Users/ed/xtest/hello-2.12.2
make distclean 2>/dev/null || true
./configure \
    --host=aarch64-chimera-linux-musl \
    --build=aarch64-apple-darwin \
    --prefix=/usr \
    --disable-nls \
    CC=/Users/ed/xtest/aarch64-chimera-musl-clang \
    AR=/opt/homebrew/opt/llvm/bin/llvm-ar \
    RANLIB=/opt/homebrew/opt/llvm/bin/llvm-ranlib \
    NM=/opt/homebrew/opt/llvm/bin/llvm-nm \
    STRIP=/opt/homebrew/opt/llvm/bin/llvm-strip \
    LDFLAGS=-static
```

Build:

```sh
make -j4
```

Verify:

```sh
docker run --rm -v /Users/ed/xtest/hello-2.12.2:/work -w /work \
    chimeralinux/chimera:latest ./hello
# Hello, world!
```

Static, ~454 KB (larger because gnulib drags in a lot of portability shims).
Full `--help` / `--version` output works.

## Gotchas worth remembering

### macOS `ar` silently produces empty archives of Linux ELF objects

Apple's `ar` creates a valid BSD-format archive but doesn't include objects
whose file format it doesn't understand — resulting in an archive with only
the symbol table. You get link errors like `undefined symbol: xmalloc` even
though the .o files exist on disk.

**Fix**: Always set `AR=llvm-ar`, `RANLIB=llvm-ranlib`, `NM=llvm-nm`,
`STRIP=llvm-strip` for cross-compile-on-mac. Every non-trivial autotools
package will hit this.

### Triple vendor string must match what's on disk

Clang's compiler-rt lookup uses the exact `<arch>-<vendor>-<os>-<env>`
string. Chimera's files are under `aarch64-chimera-linux-musl/`, so
`--target=aarch64-linux-musl` fails (clang looks for
`aarch64-unknown-linux-musl/`). Either match the directory name in
`--target` or symlink the resource subdir.

For SLIX this means: whatever vendor string you choose
(`aarch64-slix-linux-musl`? `aarch64-slix-musl`?) has to be the name of the
subdirectory clang's resources are deployed to.

### `-resource-dir` and `--sysroot` are separate

Clang-native resource files (compiler-rt, clang's own headers) are versioned
by clang version and live under `usr/lib/clang/N/`. The rest of the sysroot
(musl headers, libc, crt*.o) is versioned by libc/kernel. When you point
`--sysroot` at Chimera's tree, clang also needs `-resource-dir` pointing
into that tree — otherwise it falls back to the host clang's resource dir,
which has no Linux-musl builtins.

### `ld.lld` is not shipped with Homebrew's `llvm` formula

You have to `brew install lld` separately. The llvm package ships `lldb`
(the debugger) but not `ld.lld` (the linker). Easy to miss.

### Autotools' "guessing yes/no" messages are normal

Cross-compile means configure can't *run* test binaries, so it falls back to
educated defaults. Usually these are correct. If a specific package breaks
because of a bad guess, preseed the relevant cache var:
`ac_cv_func_malloc_0_nonnull=yes ./configure ...`

## What changes for SLIX

The recipe is the same; only the sysroot source and triple change.

1. **Port musl to SLIX syscalls.** Produce a sysroot with the same layout as
   Chimera's: `crt1.o`, `crti.o`, `crtn.o`, `libc.a`, headers in
   `usr/include/`, etc.
2. **Build compiler-rt targeting that musl.** Drop builtins into
   `usr/lib/clang/N/lib/aarch64-slix-musl/libclang_rt.builtins.a`, plus the
   CRT glue at `clang_rt.crtbegin.o` / `clang_rt.crtend.o` in the same
   directory. Build `libunwind` if you want stack unwinding.
3. **Pick a triple** (probably `aarch64-slix-musl` or
   `aarch64-slix-linux-musl` — the latter keeps the Linux-ABI substring in
   case any packages key off it).
4. **Swap the wrapper script's sysroot and target**:
   ```sh
   SYSROOT=/path/to/slix-sysroot
   clang --target=aarch64-slix-musl \
         --sysroot="$SYSROOT" \
         -resource-dir="$SYSROOT/usr/lib/clang/22" ...
   ```
5. **Everything else** — the autotools incantation, `llvm-ar` / `llvm-ranlib`
   requirements, the static-link default — carries over unchanged.

Once that exists, any autotools or Makefile-based C package should build for
SLIX with at most minor tweaks, and the output binaries are ready to be
packaged with `kit`.
