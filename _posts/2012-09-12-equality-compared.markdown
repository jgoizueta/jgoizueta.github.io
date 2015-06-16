---
layout: post
title:  "Equality Cheatsheet"
date:   2012-09-12 09:29:00
categories: languages
---

A compendium of equality, asignment, etc. operators
in programming languages.

* This will become a table of contents
{:toc}


## Ruby

```
=          assignment

equal?     object identity
  defined in Object for object identity (compares object_id)
  should not be redefined

eql? hash  equal and same type
  defined in Object as object identity (use object_id)
  defined for Arrays etc as equality
  used by Hash for keys
  in numeric types defined as value (and type)-equality
    1.eql?(1.0) # false
    1.eql?(3-2) # true
  should be coherent (redefine both)

==         equal
  defined in Object as eql?
  for numeric types define as value equality
    1==1.0 # true
  Array#== compares each element with ==
  Hash#== compares keys with eql? and values with ==

===       matching
  defined generally as equality (==) and in some cases as membership or match:
  Range#=== test if value included in range
  Regexp#=== test if string matches expression
  Class#=== tests if object is instance of class
  in Ruby 1.19 Symbol#=== test equality with Strings also
  (note: for numeric types is defined as ==, so 1===1.0 is true)
  used in case:
    case a
      when b
      ...
  is equivalent to  
    case
      when b===a
      …

<=>
  Comparable defines == in terms of <=>

!=
  automatically defined as ! ==, can be redefined in Ruby 1.9

Notes:
  Array#include? uses ==
    [1,2].include?(1.0) # true
  Array#- & | use eql?
    [1,2] - [1.0] # [1,2]
  Array#delete uses ==
   (a=[1,2]).delete(1.0) # 1.0 (a==[2])
```

## Java

```
=       assignment
==      test for object identity (for primitive types it is value equality)
equals  equality
(usually intern used on Strings to make identity check for equality efficiently)
```  

## Scala

```
=   assignment
==  equality (value equality for primitive types, uses equals method for references)
eq  object identity    ne non-identity
=== equality defined for tests for more informative messages
```

## Groovy

```
= assignment
== equality (uses equals)
is identity
```

## Magick  

```
 << assignment
  = equality     ~= inequality     (same type too, can be redefined)
_is identity   _isnt non-identity
```

## C/C++

```
= assignment (redefinable in C++)
== equality (redefinable in C++)
(identity can be checked with pointers: &x == &y)
```

## Pascal

```
:= assignment
= equality
(identity can be checked with pointers: @x == @y)
```

## Python

```
= assignent
== equality (uses __eq__ or __cmp__ method)   != inequality
is identity                                   is not non-identity
```

## C\#

```
= assignment
Equals equality
ReferenceEquals identity
== defined as ReferenceEquals by default (redefined as Equals for String) != inequality
```

## Common Lisp

```
setq  assignment (simple variable assignment statement) (lhs is quoted)
 (setf is the general assignment statement, psetq parallel setq, ...)
equal equality
eq    identity
```

## Scheme

```
set!   assignment
equal? equality
eq?    identity
```

## Dylan

```
let (let x=1;) assignment (defines and binds variable)
:= assignment (binds existing variable)
=  equality (same value and same type) (:= is used for assignment)
== identity
```

## Fortran

```
= assignment
.EQ. (==) equality   .NE. (/=) inequality
```

## Matlab

```
= assignment
== (eq) equality   ~= (ne) inequality
```

## JavaScript

```
= assignment
== equality (applying conversions) != inequality
=== strict equality (same type & value, also called identity) !== strict inequality (nonidentity)
for objects, arrays and functions both operators mean object identity
note that == equality is relaxed and complicated:
  0 == ''           # true
  0 == '0'          # true
  '' == '0'         # false (intransitive!)
  false == '0'      # true
  false == 'false'  # false
  null == undefined # true
  false == undefined # false
  ' \t\r\n' == 0    # true
```

## Smalltalk

```
:= assignment (also _ which in the original implementation appeared as an left arrow glyph ←)
= equality (must be define according to hash)
== identity (primitive)
```

## Io

```
::= (newSlot)    assignment (creates slot, creates setter, assigns value)
:=  (setSlot)    assignment (creates slot, assigns value)
=   (updateSlot) assignment (assigns value)
== equality (uses compare methos) != inequality
isIdenticalTo    identity
```

## SQL

```
= equality <> inequality
```

## BASIC

```
LET assignment (LET X = ... or just X = ...)
= equality <> inequality
```

## Visual Basic .NET

```
= assignment
=  equality  <> inequality
Is identity
```  

## PHP

```
= assignment
== equal                            != not equal
=== identical (equal and same type) !== not identical
```

## Perl

```
= assigment
== equality   != inequality
eq String equality ne String inequality
```

## RPL

```
STO   assignment ( \->   local variable definition and assignment  )
==    equality -- logical function
      (evaluates both arguments; can equal a number to an alebraic or name)
SAME  identity -- logical command
      (cannot be used in alebraic expression)
=     equation -- equals analytic function -- in numerical mode equivalente to - (minus)
      (not a logical function; used in algebraic objects; also to DEFINE variables)

2 'X' STO
3 'Y' STO
'X+Y' 5 SAME    # returns 0 (false)
'X+Y' 5 == EVAL # returns 1 (true).
'X+Y' 5 =       # returns an equation 'X+Y=5' that can be solved, etc.
'X=5' DEFINE    # like 5 STO 'X'

There are some other differences between == and SAME depending on the object types.
```

## HP PPL

```
:= assignment
== equality
<> inequality
▶  store (assignment)  
= equality (used mostly for equations, also equivalent to == in programs)
≠ inequality (also equivalent to <> in programs)
```

## Mathematica

```
=   (Set)         inmediate assignment
:=  (SetDelayed)  delayed assignment (rhs unevaluated)
==  (Equal)       equality (both equation and logical test)   != Unequal
=== (SameQ)       identical   =!= (UnsameQ)
->  (Rule)        transformation rule
:>  (RuleDelayed) delayed transformation rule
```

*note:* function definitions are assignments of transformation rules to patterns

## Swift

```
=   assignment
  let x = ... // constant definition
  var x = ... // variable declaration & assignment
  x = ...     // declared variable assignment
==  equality   !=  inequality
=== identity   !== non-identity
```

## Prolog

```
:- define rule (pattern)
=  unify (match operator; matching bounds variables)
```

## Erlang

```
-><-  define rule (pattern)
=   match operator; matching bounds variables
==  equal to
/=  not equal to
=:= exactly equal to
=/= exactly not equal to
<-  (used in list comprehensions)
```

## Elixir

```
=  match operator (matches and bounds/rebounds variables)
^= pin operator   (always matches instead of rebounding)
==  equality
!=  not equals        (/= in Erlang)
=== strict equality   (=:= in Erlang)
!== strict not equals (=/= in Erlang)
<>  concatenation (of binaries/strings)
```

*note:* def is used to define rules (functions)

## Julia

```
=            assignment
==           equality
!=           inequality
≠            inequality
.==          vectorized equality   (element-wise equality)
.!=          vectorized inequality (element-wise inequality)
.≠           vectorized inequality (element-wise inequality)
===          identity
≡            identity
is(x,y)      identity
!==          non identity
≢            non identity
isequal(x,y) calls == by default; defined specially for floating point; used by Dict
             (must imply that hash(x)==hash(y))
```

## Haskell

```
= definition (defines names / introduces local variables with "let")
== equality
<- assignment inside do blocks (monads)
>>= monadic bind operator
```

## Historic notes:

see <http://mail.python.org/pipermail/edu-sig/2002-April/002038.html>

* `=` assignment languages: Fortran, Basic, C, Java, ...
* `:=` assignment languages: Algol, Pascal, Ada, Dylan, ...
