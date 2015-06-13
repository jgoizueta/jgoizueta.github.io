---
layout: post
title:  "C++ Standard Revisions"
date:   2015-01-07 18:51:00
categories: languages revisions releases
---

<pre>

1985-10 Cfront 1.0
1985-10 The C++ Programming Language 1st ed. (TC++PL)
1987-02 Cfront 1.2
1989-06 Cfront 2.0
1990-05 The Annotated C++ Reference Manual (ARM)
1991-06 The C++ Programming Language 2nd ed. (TC++PL2)
1991-10 Cfront 3.0

Year  C++ Standard               Informal name
1998  ISO/IEC 14882:1998[19]     C++98
2003  ISO/IEC 14882:2003[18]     C++03
2007  ISO/IEC TR 19768:2007[17]  C++TR1
2011  ISO/IEC 14882:2011[16]     C++11    (C++0x)
2014  ISO/IEC 14882:2014(E)      C++14    (C++1y)
2017                             C++17    (C++1z)

Relevant language revisions: (which roughly correspond to the editions of TC++PL)
1985 - Cfront 1.0 / TC++PL                        (initial release)
1991 - ARM (includes Cfront 2 changes)            (preliminar standard)   refinement: TC++PL2
1998 - C++98 / TC++PL3                            (final standard)        refinement: C++03
2011 - C++11 / TC++PL4                            (standard revision)     refinement: C++14
C++ revisions Major features

TC++PL / Cfront 1.0
  (extensions to K&R C)
  function prototypes
  single precision float
  function overloading
  operator overloading
  inline functions
  const
  reference types
  new & delete operators
  classes
  class / enum types
  void*
  anonimous functions
  Stream I/O libraries

Cfront 2.0
  multiple inheritance
  type-safe linkage
  abstract classes
  static member functions
  const member functions
  protected members (Cfront 1.2)
  generalized initializers, base and member initializers
  overloading ->
  pointers tomembers (Cfront 1.2)

ARM
  templates
  exception handling
  nested classes
  independent prefix/postfix ++ overloading
  initialization of local static arrays allowed
  volatile
  (implemented in Cfront 3.0 except for exceptions)

TC++PL2
  overload consider anachronism
  new deleted can be overloaded
  extern linkage specifications

C++98
  __cplusplus = 199711L
  covariant return types (overriding relaxation)
  namespaces
  template improvements, partial specialization, member templates, default arguments
  use of members ahead of declaration
  for statement initializers (scope of controlled variables)
  run time type identification
  Standard Library
C++03
  (standard bug fixes)
  Standard Library:
    contiguous storage requirement for vector

C++TR1 (Library Extensions)
  Standard Library: (std::tr1 namespace) (mostly from Boost)
    reference wrapper
    smart pointers (auto_ptr)
    function objects
    metaprogramming type traits
    random number generation
    mathematical special functions
    tuple types
    fixed-size array
    hash tables
    regular expressions

C++11
  __cplusplus = 201103L
  move constructors, Rvalue references
  constexpr
  extern template
  initializer lists
  uniform initialization syntax type var{...};
  auto type inference
  range for loops
  lambda expressions
  new function declaration with trailing return type -> ...
  object construction delegation
  explicit override and final
  nullptr
  enum class (strongly type enumerations)
  multiple right angle closing brackets >>
  explicit
  template aliases
  unrestricted unions
  variadic templates
  new string literals u8"", u"" ...
  user-defined literals
  multithreading memory model, thread-local storeage, ...
  default and delete member functions
  long long int
  static_assert
  sizeof class member
  alignof, alignas
  attributes [[xxx]]

  Standard Library:
    threading facilites
    tuple types
    hash tables
    regular expressions
    smart pointers (unique_ptr to replace auto_ptr; shared_ptr; weak_ptr; make_unique, make_shared)
    extensible randon numbers
    wrapper reference
    polymorphic function wrappers  
    type traits for metaprogramming

C++14
  __cplusplus = 201402L
  auto function return type deduction
  alternate type deduction on declaration
  relaxed constexpr restrictions
  variable templates
  aggregate member initialization
  binary literals
  digit separators
  generic lambdas
  lambda captures expressions
  [[deprecated]] attribute
  Standard Library:
    shared mutexes and locking
    standard user-defined literals

  </pre>
