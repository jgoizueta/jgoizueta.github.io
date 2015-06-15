---
layout: post
title:  "Anonymous functions syntax cheatsheet"
date:   2014-11-27 22:46:00
categories: languages cheatsheets
---

Here's the syntax to define and call anonymous functions (lambdas)
in a variety of programming languages (which I'm more or less interested in).


* This will become a table of contents
{:toc}

# Elixir

Assign anonymous function to `f` defined
as the sum of its two arguments:

{% highlight elixir %}
f = fn x, y -> x + y end
{% endhighlight %}

Call the function assigned to `f` with arguments `1`
and `2`:

{% highlight elixir %}
f.(1, 2)
{% endhighlight %}

Shortcut notation:

{% highlight elixir %}
f = &(&1 + &2)
{% endhighlight %}

# Erlang

Definition:

{% highlight erlang %}
F = fun(X, Y) -> X + Y end
{% endhighlight %}

Use (invocation):

{% highlight erlang %}
F(1, 2)
{% endhighlight %}


# Julia

Definition:

{% highlight julia %}
f = (x, y) -> x + y
{% endhighlight %}

Multiline `block(x,y)` definition:

{% highlight julia %}
f = (x, y) -> begin
  block(x,y)
end
{% endhighlight %}

Invocation:

{% highlight julia %}
f(1, 2)
{% endhighlight %}

Single argument case (parentheses can be omitted):

{% highlight julia %}
x -> expr(x)
{% endhighlight %}

Block syntax: a closure first argument
(of some function `receiver`) can be passed
outside the parenthesis with do notation:

{% highlight julia %}
receiver() do x, y
  block(x,y)
end
{% endhighlight %}


named function definition:

{% highlight julia %}
f(x,y) = x + y
function f(x,y)
  x + y
end  
{% endhighlight %}

# Swift

{% highlight swift %}
let f = { (x: Int, y: Int) -> Int in x + y }
{% endhighlight %}

{% highlight swift %}
let f = { (x: Int, y: Int) -> Int in x + y }
{% endhighlight %}

Implicit return type:

{% highlight swift %}
{ (x: Int, y: Int) in x + y }
{% endhighlight %}

Shorthand:

{% highlight swift %}
let f:(Int, Int) -> Int = { $0 + $1 }
{% endhighlight %}

Trailing syntax: a closure last argument
ca be passed outside the parentheses:
(here the parentheses could be ommitted since there aren't any other arguments)

{% highlight julia %}
receiver() {
  block($0,$1)
}
{% endhighlight %}

Or, with explicit arguments:

{% highlight julia %}
receiver {
  (x: Int, y: Int) -> Int in
  block(x,y)
}
{% endhighlight %}

# Ruby

Assignment, with several alternative syntaxes:

{% highlight ruby %}
f = ->(x, y){ x + y }
f = lambda{ |x,y| x + y }
f = proc{ |x,y| x + y }
f = lambda do |x,y| x + y end
f = proc do |x,y| x + y end
{% endhighlight %}

Invocation, with alternative syntaxes:

{% highlight ruby %}
f[1,2]
f.call(1,2)
f.(1,2)
{% endhighlight %}

Passing as a block to a `receiver` method with do notation:

{% highlight ruby %}
receiver do |x,y|
  block(x,y)
end
{% endhighlight %}

With curly braces:

{% highlight ruby %}
receiver { |x,y|
  block(x,y)
}
{% endhighlight %}

Passing an anonymous function as a block:

{% highlight ruby %}
receiver &f
{% endhighlight %}

# CoffeeScript

Definition, single line:

{% highlight coffeescript %}
f = (x, y) -> x + y
{% endhighlight %}

Definition, indented block:

{% highlight coffeescript %}
f = (x, y) ->
  block(x,y)
{% endhighlight %}

Invocation:

{% highlight coffeescript %}
f(1, 2)
{% endhighlight %}

# JavaScript

Definition:

{% highlight javascript %}
f = function(x, y) { return x + y }
{% endhighlight %}

Invocation:

{% highlight javascript %}
f(1, 2)
{% endhighlight %}

# Dart

Definition:

{% highlight dart %}
f = (x, y) => x + y
{% endhighlight %}

Invocation:

{% highlight dart %}
f(1, 2)
{% endhighlight %}

# Go

Definition:

{% highlight go %}
f := func(x, y int) int { return x + y }
{% endhighlight %}

Invocation:

{% highlight go %}
f(1, 2)
{% endhighlight %}

# C#

Definition:

{% highlight csharp %}
delegate int f_type(int x, int y);
f_type f = (x, y) => x + y;
{% endhighlight %}

Definition with multiline block:

{% highlight csharp %}
delegate int f_type(int x, int y);
f_type f = (x, y) => { block(x,y); }
{% endhighlight %}

Invocation:

{% highlight csharp %}
f(1, 2)
{% endhighlight %}

# C++11

Definition:

{% highlight c++ %}
auto f = [=] (intx, int y) -> int { return x + y; };
{% endhighlight %}

Definition with implicit return type

{% highlight c++ %}
auto f = [=] (intx, int y) { return x + y; };
{% endhighlight %}

Invocation:

{% highlight c++ %}
f(1, 2)
{% endhighlight %}

# Java 8

Definition:

{% highlight java %}
interface Ftype {
  public Integer call(Integer x, Integer y);
}
Ftype f = (x, y) -> x + y;
{% endhighlight %}


Invocation:

{% highlight java %}
f.call(1, 2)
{% endhighlight %}

# Groovy

Definition:

{% highlight groovy %}
def f = { x ,y -> x + y }
{% endhighlight %}


Invocation:

{% highlight groovy %}
f(1, 2)
{% endhighlight %}

# Haskell

Definition:

{% highlight haskell %}
f = \x y -> x + y
{% endhighlight %}


Invocation:

{% highlight haskell %}
f 1, 2
{% endhighlight %}

# Python

Definition:

{% highlight python %}
f = lambda x, y: x + y
{% endhighlight %}

Invocation:

{% highlight python %}
f(1, 2)
{% endhighlight %}

# Lisp (Scheme)

Definition:

{% highlight lisp %}
(define f (lambda (x y) (+ x y)))
{% endhighlight %}

Invocation:

{% highlight lisp %}
(f 1 2)
{% endhighlight %}

# Clojure

Definition:

{% highlight clojure %}
(def f (fn [x y] (+ x y)))
{% endhighlight %}

Invocation:

{% highlight clojure %}
(f 1 2)
{% endhighlight %}

# Lua

Definition:

{% highlight lua %}
f = function(x,y) return x + y end
{% endhighlight %}

Invocation:

{% highlight lua %}
f(1, 2)
{% endhighlight %}

# Mathematica / Wolfram Language

Definition:

{% highlight mathematica %}
f = Function[{x, y}, x + y]
{% endhighlight %}

Shortcut:

{% highlight mathematica %}
f = (#1 + #2)&
{% endhighlight %}

Invocation:

{% highlight mathematica %}
f[1, 2]
{% endhighlight %}

# R

Definition:

{% highlight r %}
f <- function(x, y) x + y
{% endhighlight %}

Invocation:

{% highlight r %}
f(1, 2)
{% endhighlight %}

# Scala

Definition:

{% highlight scala %}
val f = (x: Integer, y: Integer) => x + y
{% endhighlight %}

Invocation:

{% highlight scala %}
f(1, 2)
{% endhighlight %}

# Smalltalk

Definition:

{% highlight smalltalk %}
f := [ :x :y | x + y ].
{% endhighlight %}

Invocation:

{% highlight smalltalk %}
f value: 1 value: 2
{% endhighlight %}

# Matlab / GNU Octave

Definition:

{% highlight matlab %}
f = @(x,y) x + y
{% endhighlight %}

Invocation:

{% highlight matlab %}
f(1, 2)
{% endhighlight %}


# Perl 5

Definition:

{% highlight perl %}
my $f = sub { my $x = shift; my $ y = shift; return x + y; };
{% endhighlight %}

Invocation:

{% highlight perl %}
$f->(1, 2)
{% endhighlight %}

# Perl 6

Definition:

{% highlight perl %}
my $f = -> $x, $y { x + y };
{% endhighlight %}

Shortcut (twigil parameters)

{% highlight perl %}
my $f = { $^x + $^y };
{% endhighlight %}

Invocation:

{% highlight perl %}
$f->(1, 2)
{% endhighlight %}

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

{% highlight ocaml %}
let f x y = x + y;;
{% endhighlight %}

Invocation:

{% highlight ocaml %}
f 1 2
{% endhighlight %}

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

{% highlight php %}
$f = function($x, $y) { return $x+$y; };
{% endhighlight %}

Invocation:

{% highlight php %}
$f(1, 2)
{% endhighlight %}

# TCL

Definition:

{% highlight tcl %}
set f { {x y} {expr {($x+$y)} } }
{% endhighlight %}

Invocation:

{% highlight tcl %}
apply $f 1 2
{% endhighlight %}

# Maxima

Definition:

    f: lambda([x, y], x + y);

Invocation:

    f(1, 2)

# Clang block extension (C / C++ / Objective-C)

Definition (note: f_lambda is an arbitrary identifier):

{% highlight c++ %}
int (*f)(int x, int y) = ({ int f_lambda(int x, int y) { return x + y; } &f_lambda; }); //*
{% endhighlight %}

{% comment %}
The comment with an asterisk is there in the previous and following snippets
to avoid breaking Atom's syntax highlight.
{% endcomment %}

Invocation:

{% highlight c++ %}
(*f)(1, 2) //*
{% endhighlight %}

# Visual Basic.NET

Definition

{% highlight basic %}
Dim f = Function(x,y) x + y
{% endhighlight %}

Definition (multiline block)

{% highlight basic %}
Dim f = Function(x,y)
  block(x,y)
End Function
{% endhighlight %}

Invocation:

{% highlight basic %}
f(1, 2)
{% endhighlight %}

# Factor

Definition

{% highlight factor %}
CONSTANT: f [ + ]
{% endhighlight %}

Invocation:

{% highlight factor %}
1 2 f call
{% endhighlight %}

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
