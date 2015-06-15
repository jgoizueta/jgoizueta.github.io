---
layout: post
title:  "C Versions Cheatsheet"
date:   2015-01-09 18:40:00
categories: languages revisions releases cheatsheets
---

## Before the standad

1978
: The C Programming Language 1st ed. (Kernigham & Ritchie) "K&R C"


## Standard Revisions

| Year | C Standard              | Informal name    |
|------|-------------------------|------------------|
| 1989 |  ANSI X3.159-1989       | ANSI C, C89      |
| 1990 |  ISO/IEC 9899:1990      | C90              |
| 1995 |  ISO/IEC 9899/AMD1:1995 | C95, Amendment 1 |
| 2000 |  ISO/IEC 9899/1999      | C99              |
| 2011 |  ISO/IEC 9899:20001     | C11 (C1X)        |

*Note: C90 is equivalent to C89*

## Major features per revision

### K&R C

* long int
* unsigned int
* compound += operators instead of =+
* I/O library

### ANSI C

* `__STDC__`
* Function prototypes
* preprocessor: token concatenation ##, string creation #elif, #pragma
* trigraph sequences ??
* new keyworkds: void, const, volatile, signed, enum
* new escape sequences `\`
* constant suffixes U L, F L
* wide char strings
* signed/unsigned
* `void *` instead of `char *``
* enumerations
* type qualifiers (const)
* type properties in <limits.h> <float.h>
* assignment operators single token; old =+ operators gone
* unary +
* pointer to function use without `*`
* structure copy (assignment, passed to or returned from functions)
* address of array returns pointer to array
* `size_t`, `ptrdiff_t` `<stddef.h>`
* automatic structures, unions and arrays initialization
* switch for any integral type
* Standard Library

### C95

* `__STDC_VERSION__ == 199409L`
* digraphs for `[]{}`
* extended character sets support

* Standard Library:
  - `<iso646.h>`
  - EILSEQ in '<errno.h>'
  - changes in '<wctype.h> <wchar.h>'

### C99

* `__STDC_VERSION__ == 199901L`
* inline functions
* intermingled declarations
* long long int, boolean, complex
* variable-length arrays
* flexible array members
* one-line comments `//`
* improved IEEE floating point support
* designated initializers
* compound literals
* variadic macros
* restrict qualification
* universal character names

* Standar Library:std lib
  - snprintf
  - `<stdbool.h> <complex.h> <tgmath.h> <inttypes.h>`

### C11

* `__STDC_VERSION__ == 201112L`
* alignment specification `_Alignas`, `_Alignof`, `aligned_alloc`
* `_Noreturn`
* `_Generic`
* `_Thread_local`
* Unicode support `chart16_t`, `char32_t`
* anonymous structures and unions
* static assertions

* Standard Library
  - `<stdalign.h> <threads.h> <uchar.h>`
  - gets_s
  - bound-checking interfaces
  - exclusive create and open mode "x"
  - quick_exit
  - complex variable macros
  - floating point macros
