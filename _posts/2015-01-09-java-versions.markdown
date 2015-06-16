---
layout: post
title:  "Java Major Releases Cheatsheet"
date:   2015-01-09 17:49:00
categories: languages
---

| 1995
|| Alpha and Beta releases (unstable)
|
| 1996-01-23
|| **Java 1.0** [JDK 1.0]
|
| 1997-02-19
||  **Java 1.1** [JDK 1.1]
||  inner classes
| *libs:* | AWT event; JDBC, RMI; reflection
|
| 1998-12-08
|| **Java 1.2** [J2SE 1.2] *Playground*
||  strictfp
| *libs:* | unified collections; Swing; Event Listeners; Thread synchronizations changed
|
| 2000-05-08
||  **Java 1.3** [J2SE 1.3] *Kestrel*
||  platform: HotSpot JVM
|
| 2002-02-06
|| **Java 1.4** [J2SE 1.3] *Merlin*
||  assert
||  chained exception
| *libs:* | regular expressions; NIO; integrated XML
|
| 2004-09-30
|| **Java 5** (1.5) [J2SE 5.0] *Tiger*
||  generics
||  annotations
||  enum types
||  varags echanced for loops
||  static import
||  autoboxing/unboxing (primitive/wrapper types)
||  enhanced for (collections)
||  covariant return types
| *libs:* | concurrency utilities
|
| 2006-12-11
||  **Java 6** (1.6) [Java SE 6] *Mustang*
||  `@override`
||  pluggable annotations
| *libs:* | JDBC 4.0
||  VM scripting language support
||  *Updates 1-81*
|
| 2011-07-28
|| **Java 7** (1.7) [Java SE 7] *Dolphin*
||  switch on Strings
||  try with resources
||  diamond operator `<>`
||  type inference for generic instance creation
||  multiple exception handling
||  automatic null handling
| *libs:* | NIO.2
|| VM: dynamic languages support
|| *Updates 1-72*
|
| 2014-03-18
|| **Java 8** (1.8) [Java SE 8]
||  lambda expressions
||  annotations on Java types
||  default methods
||  type annotations
||  static methods in interfaces
||  functional interfaces
||  generic type inference improvements
| *libs:* | improved Date Time API java.time; java.util.function; java.util.stream
||  *Updates 5-25*
|
| 2016-xx-xx
|| **Java 9** (1.9) [Java SE 10]

## The Java Programming Language book editions:

1996 | 1st edition | TJPL  | Java 1
1997 | 2nd edition | TJPL2 | Java 1.1
2000 | 3rd edition | TJPL3 | J2SE 1.3
2005 | 4th edition | TJPL4 | J2SE 5.0

## The Java Language Specification editions:

2014-03 | Java SE 8 Edition
2011-07 | Java SE 7 Edition
2005-05 | Third Edition     | (Java SE 5.0 release)
2000-06 | Second Edition    | (Java 2 release)
1996-08 | First Edition

## Java Ecosystem

**Java SE** - Java Platform, Standard Edition

|  1998 | J2SE 1.2
|  2000 | J2SE 1.3
|  2002 | J2SE 1.4
|  2005 | J2SE 1.5
|  2006 | Java SE 6
|  2011 | Java SE 7
|  2014 | Java SE 8

**Java EE** - Java Platform, Enterprise Edition

|  1999 | J2EE 1.2
|  2001 | J2EE 1.3
|  2003 | J2EE 1.4
|  2005 | Java EE 5
|  2009 | Java EE 6
|  2013 | Java EE 7

**JVM**: Java Virtual Machine

### Standards / APIs:

| **CDI** | Contexts and Dependency Injection (for Java EE)
| **EJB** | Enterprise Java Beans - server side component architecture (Java EE)
| **Java Servlet**
| **JSP** | JavaServer Pages
| **JPA** | Java Persistence API
| **JSF** | JavaServer Faces (component-based user interfaces) [v1: JSP views; v2: Facelets]
| **JMS** | Java Message Service
| **JDBC**|  Java Database Connectivity
| **JTA** | Java Transaction API
| **JNI** | Java Native Interface

### Graphics / UI

| 1995
|| **AWT** Abstract Window Toolkit
|| original UI widget toolkit; part of JFC
| 1996
|| **IFC** Netscape Internet Foundation Classes
| 1997
|| **Swing**  GUI widget toolkit - MVC (from IFC)
| 1997
|| **JFC** Java Foundation Classes
|| Graphical framework for GUIs (AWT, Swing, Java 2D)
| 2001
|| **SWT** Standard Widget Toolkit
|| used in Eclipse IDE (native system use through JNI)
| 2004
|| **JFace** UI Toolkit (Eclipse)
