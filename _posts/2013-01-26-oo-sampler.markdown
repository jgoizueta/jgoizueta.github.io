---
layout: post
title:  "Class definition"
date:   2013-01-26 18:13:00
categories: languages cheatsheet
---

Here's a very simple OO chore written in many languages:

A class `Point` is defined to represent points composed of two integer
attributes, `x` and `y`.

An object of that class, `p` is instantiated asigning the values
10 to `x` and 20 to `y`.

The object is printed as "Point: X Y"

For each language the date of its inception is show; the second
date that appears in some cases is the date of the language form
used here.

* This will become a table of contents
{:toc}


## Simula [1964; 1967]

```simula
begin

class Point(x, y); integer x, y;
begin
  integer procedure getX;
    getX := x;
  integer procedure getY;
    getY := y;
end

ref(Point) p;
p :- new Point(10, 20);

OutText("Point: ");
OutInt(p.getX, 5);
OutInt(p.getY, 5);
OutImage;

end
```

## Smalltalk [1971; 1980]

```smalltalk
Object subclass: #Point
  instanceVariableNames: 'x y'
  classVariableName: ''

!Point instance methods!
  x
    ^x
  y
    ^y

  setX: xValue setY: yValue
    x := xValue.
    y := yValue

!Point class methods!
  x: xv y: yv
    ^self new setX: xv setY yv

p := Point x:10 y:20.
Transcript show: 'Point: '; show: p x; show: ' '; show: p y; cr.
```

## C [1972]

```c
#include <stdlib.h>
#include <stdio.h>

typedef struct sPoint {
  int x, y;
} Point;

Point* Point_new(int x, int y) {
  Point* this = (Point*)malloc(sizeof(Point));
  if (this != 0) {
    this->x = x;
    this->y = y;
  }
  return this;
}

int main() {
  Point * p = Point_new(10, 20);
  if (p) {
    printf("Point: %d %d\n", p->x, p->y);
  }
  free(p);
  return 0;
}
```

## CLU [1974]

```clu
  point = cluster is create, get_x, get_y
    rep = record [ x: int, y: int ]

    create = proc(x, y: int) returns (cvt) signals (overflow, underflow)
       return(rep${x: x, y: y})
    end create

    get_x = proc(c: cvt) returns (int) signals (overflow, underflow)
      return(c.x)
    end get_x

    get_y = proc(c: cvt) returns (int) signals (overflow, underflow)
      return(c.y)
    end get_y
  end point

  start_up = proc ()
    out: stream := stream$primary_output ()
    p: point := point$create(10, 20)
    stream$putl (pout, "Point: " || int$unparse(p.get_x()) || " " || int$unparse(p.get_y())
  end start_up
```  

## Ada [1979; 1983]

```ada
package Geometry is
  type Point is tagged private;

  function Set(X_Value : in Integer, Y_Value : in Integer) return Point;

  function Setx(Item : in Point;  Value : Integer) return Point;
  function Sety(Item : in Point;  Value : Integer) return Point;
  function Getx(Item : in Point) return Integer;
  function Gety(Item : in Point) return Integer;

private
  type Point is tagged record
    X : Integer := 0;
    Y : Integer := 0;
  end record;
end Geometry;

package body Geometry is
  function Getx (Item : in Point) return Integer is
  begin
    return Item.X;
  end Getx;

  function Gety (Item : in Point) return Integer is
  begin
    return Item.Y;
  end Gety;

  function Setx (Item : in Point; Val : Integer) return Point is
  begin
    return (Val, Item.Y);
  end Setx;

  function Sety (Item : in Point; Val : Integer) return Point is
  begin
    return (Item.X, Val);
  end Sety;

  function Create(X, Y : Integer) return Point is
  begin
    return (X, Y);
  end Create;

  function Set(X_Value : in Integer, Y_Value : in Integer) return Point is
    Temp : Point;
  begin
    Temp.X := X_Value;
    Temp.Y := Y_Value;
  end Set;
end Geometry;

with Geometry; use Geometry;
with Ada.Text_IO; use Ada.Text_IO;
procedure Main is
  P_Point : Point;
begin
  -- P_Point := Set(10, 20)
  P_Point := Create(10, 20)
  Ada.Text_IO.Put_Line(Item => "Point: " & Integer'Image(P_Point.Getx) & " " & Integer'Image(P_Point.Gety));
end Main;
```

## C++ [1980; 1983; 1998]
```cpp
class Point {
  public:
  int x, y;
  Point(int xv=0, int yv=0) { // :x(xv), y(yv)
    x = xv;
    y = yv;
  }
};

#include <iostream>
int main() {

// Stack (automatic) objects
{
Point p(10, 20);
std::cout << "Point: " << p.x << " " << p.y << std::endl;
}

// Heap objects
Point *p = new Point(10, 20);
std::cout << "Point: " << p->x << " " << p->y << std::endl;
delete p;

return 0;
}
```

Using private data members and defining getters to access them:

```cpp
class Point
  private:
  int x, y;
  public:
  Point(int xv=0, int yv=0) { // :x(xv), y(yv)
    x = xv;
    y = yv;
  }
  int get_x() { return x; }
  int get_y() { return y; }
};

Point p(10, 20);
std::cout << "Point: " << p.get_x() << " " << p.get_y() << std::endl;
```

## Objective-C [1983]

```objective-c
#import <stdio.h>
#import <objc/Object.h>

@interface Point : Object {
  @private
     int x;
     int y;
}
- (int) x;
- (int) y;
- (id) x: (int) x_value y: (int) y_value;
+ (id) newWithX: (int) x_value AndY: (int) y_value;
@end

@implementation Point
- (int) x {
   return x;
}
- (int) y {
   return y;
}
- (id) x: (int) x_value y: (int) y_value {
    x = x_value;
    y = y_value;
}
+ (id) newWithX: (int) x_value AndY: (int) y_value {
    return [[Point new] x:x_value y:y_value];
}
@end

int main(void) {
  // Point *point = [[Point new] x:10 y:20];
  Point *point = [Point newWithX:10 AndY:20];
  printf("Point: %d %d\n", [point x], [point y]);

  return 0;
}
```

## Common Lisp [1984]

```lisp
(defclass point ()
  ((x :initarg :x :initform 0 :accessor x)
   (y :initarg :y :initform 0 :accessor y)))

(let ((p (make-instance 'point :x 10 :y 20)))
   (format t "Point: ~D ~D~%" (x p) (y p)))
```

## PHP [1994; 1995]

```php
class Point {
  public $x, $y;
  function __construct($vx, $vy) {
    $this->x = $vx;
    $this->y = $vy;
  }
}
$p = new Point(10, 20);
print "Point: $p->x $p->y\n"
```

## Eiffel [1986]

```eiffel
class
    POINT
inherit
    ANY
        redefine
            out
        end
create
    make

feature -- Initialization

    make (a_x, a_y: INTEGER)
            -- Create with values `a_x' and `a_y'
        do
            set_x (a_x)
            set_y (a_y)
        ensure
            x_set: x = a_x
            y_set: y = a_y
        end

feature -- Access

    x: INTEGER assign set_x
            -- Horizontal axis coordinate

    y: INTEGER assign set_y
            -- Vertical axis coordinate

feature -- Element change

    set_x (a_x: INTEGER)
            -- Set `x' coordinate to `a_x'
        do
            x := a_x
        ensure
            x_set: x = a_x
        end

    set_y (a_y: INTEGER)
            -- Set `y' coordinate to `a_y'
        do
            y := a_y
        ensure
            y_set: y = a_y
        end

feature -- Output

    out: STRING
            -- Display as string
        do
            Result := "Point: " + x.out + " " + y.out
        end
end

class
  TEST_POINT
create
  make
feature
  make
    do
      p := create point.make(10, 20)
      io.put_string("Point: " + p.x.out + " " + p.y.out)
      io.put_new_line
      print ("Point: " + p.x.out + " " + p.y.out + "%N")
    end
end
```

## Self [1987]

```self
( |     parent* = traits point.
        x <- 10.
        y <- 20.
| )
```

## Perl [1987]

```perl
{
   package Point;
   sub new {
     my $class = shift;
     bless {x=>shift || 0, y=>shift || 0}, $class;
   }
}

my $p = Point->new(10,20);
print "Point: ", $p->{x}, " ", $p->{y}, "\n";
```

## Tcl (TclOO library) [1988]

```tcl
package require TclOO
oo::class create Point {
    variable X Y
    constructor {x y} {
        set X $x
        set Y $y
    }
    method x args {
        set X {*}$args
    }
    method y args {
        set Y {*}$args
    }
}

set p [Point new 10 20]
puts "Point: [$p x] [$p y]"
```

## Magik [1989; 1990]

```magik
def_slotted_exemplar(:Point, {
  {:x, 0}, {:y, 0}
})

_method Point.new(x, y)
  _return clone.init(x, y)
_endmethod

_private _method Point.init(x, y)
  .x << x
  .y << y
  _return _self
_endmethod

p << Point.new(10, 20)
write("Point: ", p.x, " ", p.y, newline_char)
```

## Haskell [1990]

```haskell
data Point = Point Integer Integer
instance Show Point where
    show (Point x y) = "Point: "++(show x)++" "++(show y)

main = do
  print $ show $ Point 10 20
```

## Java [1991; 1995]

```java
public class Point {
  public int x, y;
  public Point(int xv, int yv) {
    this.x = xv;
    this.y = yv;
  }
}

public class TestPoint {
  public static void main(String[] args) {
    Point p = new Point(10,20);
    System.out.format("Point: %d %d\n",p.x,p.y);
  }
}
```

Defining getters:

```java
public class Point {
  private int x, y;
  public Point(int xv, int yv) {
    this.x = xv;
    this.y = yv;
  }
  public int getX() {
    return this.x;
  }
}

Point p = new Point(10,20);
System.out.format("Point: %d %d\n",p.getX(),p.getY());
```

## Python [1991]

```python
class Point:
  def __init__(self, x, y):
    self.x = x
    self.y = y

p = Point(10,20)
print "Point:", p.x, p.y
```

## Ruby [1993; 1995]

```ruby
class Point
  def initialize(x=0, y=0)
    @x = x
    @y = y
  end

  attr_reader :x, :y
end

p = Point.new(10,20)
puts "Point: #{p.x} #{p.y}"
```

## Lua [1993]
```lua
point = setmetatable({
__index = function(z, i) return point[i] end,
}, {
__call = function(z, xv, yv) return setmetatable({x = xv, y = yv}, point) end
})

p = point(10, 20)
print("Point:", p.x, p.y)
```

With getters:

```lua
Point = {}
Point.new = function(x, y)
  local self = {}
  x = x or 0
  y = y or 0
  self.getx = function() return x end
  self.gety = function() return y end
  return self
end
p = Point.new(10,20)
print("Point:", p.getx(), p.gety())
```

## R (S4) [1993]

```r
setClass("point",
   representation(
      x="numeric",
      y="numeric"),
   prototype(
      x=0,
      y=0))

p <- new("point", x=10, y=20)

print(cat("Point:", p@x, p@y,"\n"))
```

## JavaScript [1994; 1995]

```javascript
function Point(x, y) {
  this.x = x;
  this.y = y;
};
var p = new Point(10, 20);
console.log("Point: "+p.x+" "+p.y);
```

## OCaml [1996]

```ocaml
class point ?(x=0) ?(y=0) () =
object(self)
  val mutable x = x
  val mutable y = y

  method x = x
  method y = y
  method set_x x' = x <- x'
  method set_y y' = y <- y'
end

let () =
  let p = new point (10, 20) in
    Printf.sprintf "Point: %d %d\n" p#x p#y
```

## C# [2000]

```csharp
using System;

public class Point {
  public Point(int xv, int yv) {
    x = xv;
    y = yv;
  }
  public int x, y;
}

public static void main(String args[]) {
  Point p = new Point(10, 20);
  System.Console.WriteLine("Point: {0}, {1}", p.x, p.y);
}

```

With getters:

```csharp
public class Point {
  public Point(int x, int y) {
    _x = x;
    _y = y;
  }
  private int _x, _y;
  public int x {
    get { return _x; }
  }
  public int y {
    get { return _y; }
  }
}

public static void main(String args[]) {
  Point p = new Point(10, 20);
  System.Console.WriteLine("Point: {0}, {1}", p.x, p.y);
}
```

## Perl 6 [2000]

```perl
class Point {
  has Int $.x is r = 0;
  has Int $.y is r = 0;
}
my $p = Point.new(x=>10, y=>20);
say "Point: ",  $p.x, " ", $p.y;
```

## Visual Basic .NET [2001]

```basic
Class Point
  Private m_X As Integer
  Private m_Y As Integer

  Public Sub New(ByVal x As Integer, ByVal y As Integer)
    m_X = x
    m_Y = y
  End Sub

  Public Property X() As Integer
    Get
      Return m_X
    End Get
    Set(ByVal v As Integer)
      m_X = v
    End Set
  End Property

  Public Property Y() As Integer
    Get
      Return m_Y
    End Get
    Set(ByVal v As Integer)
      m_Y = v
    End Set
  End Property
End Class

Dim p As Point
p = New Point(10, 20)
Console.WriteLine("Point: "+p.X.ToString()+" "+p.Y.ToString())
```

## Io [2002]

```io
Point := Object clone do(
  x := 0
  y := 0
)
p := Point clone
p x = 10
p y = 20

write("Point: ")
p x print
write(" ")
p y println
```

## Groovy [2003]

```groovy
class Point {
  public int x
  public int y
  Point(x, y) {
    this.x = x
    this.y = y
  }
}
p = new Point(10, 20)
println "Point: ${p.x} ${p.y}"
```

## Scala [2003]

```scala
class Point(xv: Int, yv:Int) {
  var x: Int = xv
  var y: Int = yv
}

object Main extends App {
  val p = new Point(10, 20)
  printf("Point: %d %d\n", p.x, p.y)
}
```

## Clojure [2007]

```clojure
(deftype Point [x y])

(let [p (Point. 10 20)]
  (prn "Point:" (.x p) (.y p)))
```

## CoffeeScript [2009]

```coffeescript
class Point
  constructor: (x, y) -> # constructor: (@x, @y) ->
    @x = x
    @y = y
  x: 0
  y: 0

p = new Point(10, 20)
console.log "Point: #{p.x} #{p.y}"
```

## Go [2009]

```go
type point struct {
  x int
  y int
}

func newPoint(x int, y int) *point {
  return &point{x, y}
}

var p *point = newPoint(10, 20)
fmt.Println("Point:", p.x, p.y)
Julia [2012] ☑
type Point
  x::Int
  y::Int
end

p = Point(10, 20)
println("Point: $(p.x) $(p.y)")
Rust [2012] ☑
struct Point {
  x: int,
  y: int,
}

fn main() {
  let p = Point{ x: 10, y: 20 };
  println!("Point: {} {}", p.x, p.y);
}
```

## Swift [2014]

```swift
class Point{
  var x : Int
  var y : Int

  init(x: Int, y: Int) {
    self.x = x
    self.y = y
  }

}
let p = Point(x: 10, y: 20)
println("Point: \(p.x) \(p.y)")
```
