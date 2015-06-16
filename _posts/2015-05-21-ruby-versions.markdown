---
layout: post
title:  "Ruby Major Releases Cheatsheet"
date:   2014-05-21 21:37:00
categories: languages
---

Ruby (MRI) most relevant versions.

| **Ruby 1.8.0**
| 2003-08
||  `class.allocate`
||  first argument parentheses change
||  `to_str`
||  multiple assignment behaviour change
||  many lib changes
|
| **Ruby 1.8.5**
| 2006-08
||  Non-blocking IO
||  rdoc/ri changes
|
| **Ruby 1.8.6**
| 2007-03
||  Faster mutex
|
| **Ruby 1.8.7**
| 2008-06
||  Enumerator (from 1.9)
|
| **Ruby 1.9**
| 2007-12
||  new hash syntax
||  new lambda syntax
||  splats/mandatory arguments changes...
||  block arguments are local; new semantics
||  new lines before : or .
||  source code encoding/magic comments
||  constant definition semantics
||  Enumerators
||  Hash preserves order
||  String encodings
||  `String#[]` semantics, `?x`
||  `rray#choice` nitems
||  `to_s == inspect` in Hash, etc
||  Fibers
||  Native Threads / GIL
||  YARV
|
||  `String#each_line/lines`, not `each`
||  continuations moved to library
||  Precision removed
|
| **Ruby 1.9.2**
| 2010-08
||  Regexp chages (`\p`...)
||  `$:` no current dir; use require_relative
||  `Float#rationalize` `INFINITY` `NAN`
|
| **Ruby 1.9.3**
| 2011-10
||  Regexp changes
|
||  libs/internal
|
| **Ruby 2.0**
| 2013-02
||  keyword arguments
||  refinements (experimental)
||  `%i %I` symbol arrays
||  UTF-8 default encoding
||  lazy enumeration
|
| **Ruby 2.1**
| 2013-12 | *semantic versioning from now on; 2.1.x vs 1.9.3-px*
||  refinements
||  frozen string literals
||  r,i numeric suffixes
||  def returns name as symbol
||  required keyword arguments (without default value)
|
||  RGenGC (generational GC)
|
| **Ruby 2.2**
| 2014-12-25
||  Incremental GC, Symbol GC
||  `Float#next_float`, `prev_float`
||  `Enumerable#slice_after`
||  `File.birthtime`

## Numerical features availability

* `Float#round` with precision argument **1.9**
* `Rational` with 0 denominator raises `ZeroDivisionError` since **1.9.1** [^note]
* `Float#to_r` **1.9.1** [^note]
* `Float#rationalize` **1.9.2**
* `Float::NAN`, `INFINITY` : **1.9.2**
* `Float(hex-string)` / `printf %a %A` : **1.9.2**
* `BigDecimal` constructor from `Integer`, `Float+precision`, `Rational+precision`: **1.9.3**
* `BigMath` module functions (rather that needing to include `BigMath`...): **1.9.3**
* bigdecimal/util `#to_d` **1.9.3**
* `Float#next_float` Ruby **2.2**
* `Float#prev_float` Ruby **2.2**

[^note]: Internal version 1.9.0-2 actually, when Rational was moved from stdlib (Ruby) into core (C)
