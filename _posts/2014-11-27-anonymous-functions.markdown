---
layout: post
title:  "Anonymous functions syntax cheatsheet"
date:   2014-11-27 22:46:00
categories: languages
---

Here's the syntax to define and call anonymous functions (lambdas)
in a variety of programming languages (which I'm more or less interested in).


* This will become a table of contents
{:toc}

# Elixir

Assign anonymous function to `f` defined
as the sum of its two arguments:

```elixir
f = fn x, y -> x + y end
```

Call the function assigned to `f` with arguments `1`
and `2`:

```elixir
f.(1, 2)
```

Shortcut notation:

```elixir
f = &(&1 + &2)
```

# Erlang

Definition:

```erlang
F = fun(X, Y) -> X + Y end
```

Use (invocation):

```erlang
F(1, 2)
```


# Julia

Definition:

```julia
f = (x, y) -> x + y
```

Multiline `block(x,y)` definition:

```julia
f = (x, y) -> begin
  block(x,y)
end
```

Invocation:

```julia
f(1, 2)
```

Single argument case (parentheses can be omitted):

```julia
x -> expr(x)
```

Block syntax: a closure first argument
(of some function `receiver`) can be passed
outside the parenthesis with do notation:

```julia
receiver() do x, y
  block(x,y)
end
```


named function definition:

```julia
f(x,y) = x + y
function f(x,y)
  x + y
end  
```

# Swift

```swift
let f = { (x: Int, y: Int) -> Int in x + y }
```

```swift
let f = { (x: Int, y: Int) -> Int in x + y }
```

Implicit return type:

```swift
{ (x: Int, y: Int) in x + y }
```

Shorthand:

```swift
let f:(Int, Int) -> Int = { $0 + $1 }
```

Trailing syntax: a closure last argument
ca be passed outside the parentheses:
(here the parentheses could be ommitted since there aren't any other arguments)

```julia
receiver() {
  block($0,$1)
}
```

Or, with explicit arguments:

```julia
receiver {
  (x: Int, y: Int) -> Int in
  block(x,y)
}
```

# Ruby

Assignment, with several alternative syntaxes:

```ruby
f = ->(x, y){ x + y }
f = lambda{ |x,y| x + y }
f = proc{ |x,y| x + y }
f = lambda do |x,y| x + y end
f = proc do |x,y| x + y end
```

Invocation, with alternative syntaxes:

```ruby
f[1,2]
f.call(1,2)
f.(1,2)
```

Passing as a block to a `receiver` method with do notation:

```ruby
receiver do |x,y|
  block(x,y)
end
```

With curly braces:

```ruby
receiver { |x,y|
  block(x,y)
}
```

Passing an anonymous function as a block:

```ruby
receiver &f
```

# CoffeeScript

Definition, single line:

```coffeescript
f = (x, y) -> x + y
```

Definition, indented block:

```coffeescript
f = (x, y) ->
  block(x,y)
```

Invocation:

```coffeescript
f(1, 2)
```

# JavaScript

Definition:

```javascript
f = function(x, y) { return x + y }
```

Invocation:

```javascript
f(1, 2)
```

# Dart

Definition:

```dart
f = (x, y) => x + y
```

Invocation:

```dart
f(1, 2)
```

# Go

Definition:

```go
f := func(x, y int) int { return x + y }
```

Invocation:

```go
f(1, 2)
```

# C#

Definition:

```csharp
delegate int f_type(int x, int y);
f_type f = (x, y) => x + y;
```

Definition with multiline block:

```csharp
delegate int f_type(int x, int y);
f_type f = (x, y) => { block(x,y); }
```

Invocation:

```csharp
f(1, 2)
```

# C++11

Definition:

```c++
auto f = [=] (intx, int y) -> int { return x + y; };
```

Definition with implicit return type

```c++
auto f = [=] (intx, int y) { return x + y; };
```

Invocation:

```c++
f(1, 2)
```

# Java 8

Definition:

```java
interface Ftype {
  public Integer call(Integer x, Integer y);
}
Ftype f = (x, y) -> x + y;
```


Invocation:

```java
f.call(1, 2)
```

# Groovy

Definition:

```groovy
def f = { x ,y -> x + y }
```


Invocation:

```groovy
f(1, 2)
```

# Haskell

Definition:

```haskell
f = \x y -> x + y
```


Invocation:

```haskell
f 1, 2
```

# Python

Definition:

```python
f = lambda x, y: x + y
```

Invocation:

```python
f(1, 2)
```

# Lisp (Scheme)

Definition:

```lisp
(define f (lambda (x y) (+ x y)))
```

Invocation:

```lisp
(f 1 2)
```

# Clojure

Definition:

```clojure
(def f (fn [x y] (+ x y)))
```

Invocation:

```clojure
(f 1 2)
```

# Lua

Definition:

```lua
f = function(x,y) return x + y end
```

Invocation:

```lua
f(1, 2)
```

# Mathematica / Wolfram Language

Definition:

```mathematica
f = Function[{x, y}, x + y]
```

Shortcut:

```mathematica
f = (#1 + #2)&
```

Invocation:

```mathematica
f[1, 2]
```

# R

Definition:

```r
f <- function(x, y) x + y
```

Invocation:

```r
f(1, 2)
```

# Scala

Definition:

```scala
val f = (x: Integer, y: Integer) => x + y
```

Invocation:

```scala
f(1, 2)
```

# Smalltalk

Definition:

```smalltalk
f := [ :x :y | x + y ].
```

Invocation:

```smalltalk
f value: 1 value: 2
```

# Matlab / GNU Octave

Definition:

```matlab
f = @(x,y) x + y
```

Invocation:

```matlab
f(1, 2)
```


# Perl 5

Definition:

```perl
my $f = sub { my $x = shift; my $ y = shift; return x + y; };
```

Invocation:

```perl
$f->(1, 2)
```

# Perl 6

Definition:

```perl
my $f = -> $x, $y { x + y };
```

Shortcut (twigil parameters)

```perl
my $f = { $^x + $^y };
```

Invocation:

```perl
$f->(1, 2)
```

# Magik

Definition:

    f << _proc(x, y)
      _return x + y
    _endproc #_

{% comment %}
The final comment (hash-underscore) is there to
avoid breaking Atom's syntax highlight.
{% endcomment %}

Invocation:

    f(1, 2)

Alternative invocation:

    f.invoke(1, 2)

# OCaml

Definition:

```ocaml
let f x y = x + y;;
```

Invocation:

```ocaml
f 1 2
```

# HP PPL CAS (GIAC/XCAS)

Definition: (entered form)

    f(x,y) := x + y

Definition: (alternative)

    f := (x,y) -> x + y

Definition: (shown / edition form)

    f := (x,y) -> (x + y)

Definition: (block form as entered)

    f(x,y) := begin block(x,y) end

Definition: (block form as shown)

    f := (x,y)->[block(x,y)]

Definition: (block form as edited)

    f := (x,y)->BEGIN block(x,y); END;

For a single argument, parentheses can be ommitted:

    f := x -> expr(x)

Invocation:

    f(1, 2)

# PHP (>=5.3)

Definition:

```php
$f = function($x, $y) { return $x+$y; };
```

Invocation:

```php
$f(1, 2)
```

# TCL

Definition:

```tcl
set f { {x y} {expr {($x+$y)} } }
```

Invocation:

```tcl
apply $f 1 2
```

# Maxima

Definition:

    f: lambda([x, y], x + y);

Invocation:

    f(1, 2)

# Clang block extension (C / C++ / Objective-C)

Definition (note: f_lambda is an arbitrary identifier):

```c++
int (*f)(int x, int y) = ({ int f_lambda(int x, int y) { return x + y; } &f_lambda; }); //*
```

{% comment %}
The comment with an asterisk is there in the previous and following snippets
to avoid breaking Atom's syntax highlight.
{% endcomment %}

Invocation:

```c++
(*f)(1, 2) //*
```

# Visual Basic.NET

Definition

```basic
Dim f = Function(x,y) x + y
```

Definition (multiline block)

```basic
Dim f = Function(x,y)
  block(x,y)
End Function
```

Invocation:

```basic
f(1, 2)
```

# Factor

Definition

```factor
CONSTANT: f [ + ]
```

Invocation:

```factor
1 2 f call
```

# RPL

Definition

    \<< + \>> 'F' STO

Invocation

    1 2 F

If `F` is a local variable

    1 2 F EVAL

Alternative invocation forms:

    1 2 'F' EVAL
    'F(1,2)' EVAL

Definition using local variables:

    \<< \-> X Y 'X+Y' \>> 'F' STO

Definition with RPL block:

    \<< \-> X Y \<< block(X,Y) \>> \>> 'F' STO

Algebraic definition:

    'F(X,Y)=X+Y' DEFINE
