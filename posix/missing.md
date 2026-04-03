# Missing POSIX Functions

## Needs hardware trig/exp instructions (planned for cpu branch)

- `sin`, `cos`, `tan`
- `asin`, `acos`, `atan`, `atan2`
- `sinh`, `cosh`, `tanh`
- `exp`, `exp2`, `log`, `log2`, `log10`
- `frexp`, `ldexp`, `modf`

## Needs inline assembly or compiler intrinsics

- `sqrt` — has Newton's method placeholder, should use `fsqrt` instruction
- `fpow` — non-integer exponents need `exp`/`log`, should use `fpow` instruction
- `div` — works but could be single `div` instruction returning register pair

## Needs allocator import

- `strdup` — duplicate a string (malloc + strcpy)
- `strndup` — duplicate at most n bytes (malloc + strncpy)

## Needs OS/locale support

- `strerror` — maps errno to error message string
- `strcoll` — locale-dependent string comparison (defaults to `strcmp` in C locale)
- `strxfrm` — locale-dependent string transformation
