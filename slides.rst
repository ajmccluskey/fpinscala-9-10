Where are we?
=============

Part 1: Intro to FP
-------------------

.. role:: latex(raw)
   :format: latex


- What is it
- Datastructures
- Error handling
- Strict vs lazy
- Pure state

Part 2: Functional design and combinator libraries
--------------------------------------------------

- Parallelism
- Poperty-based testing
- **Parser combinators**

Part 3: Common structures in functional design
----------------------------------------------

- **Monoids**
- Monad
- Applicative and traversable functors

Part 4: Effects and I/O
-----------------------

- External effects and I/O
- Local effects and mutable state
- Stream processing and incremental I/O

A note on abstraction
=====================

----


    The purpose of abstraction is not to be vague, but to create a new semantic level in which one can be absolutely precise.
    -- Edsger Dijkstra

Chapter 9: Parsers
==================


It's the journey maaaaan
------------------------

.. image:: manaslu.jpg
     :align: center

.. raw:: latex
    \note[itemize] {
        - Focus is on *algebraic design*
        - Parser combinators used to show off design methodology
    }

What's a parser?
----------------

- Unstructured data to structured
- e.g. ``String`` to ``Sale`` data type

----

    A parser for things is a function from strings to lists of pairs of things and strings.
    -- Conor McBride [#]_

.. code:: scala

    def parse[Thing](s: String): List[(Thing, String)]

.. [#] `via @hackuador <https://twitter.com/hackuador/status/72567583412035993>`_

.. ----
.. 
.. - Difference between parser *combinators* and *generators*
.. - *combinators* for defining parsers by hand
.. - *generators* produce a parser from a grammar

Algebra first design
--------------------

Algebra loosely defined as data types, functions over them, and laws that specify
how they must work

How should our parser work?
---------------------------

- ``String`` as input
- Bake good error values in from the beginning
- Build up from primitive combinators

Candidate primitive
-------------------

.. code:: scala

   def char(c: Char): Parser[Char]

Running
-------

.. code:: scala

   def run[A](p: Parser[A])(s: String):
           Either[ParseError, A]

Compiling our algebra
---------------------

Given Scala's static typing, we can start to compile our algebra to ensure that
it makes sense at the type level.

.. code:: scala

  trait Parsers[Parser[+_]] {
    def run[A](p: Parser[A])(input: String):
      Either[ParseError,A]
    def char(c: Char): Parser[Char]
  }

Don't forget the laws
---------------------

Not only can we type check the algebra, we can also start writing down laws.

.. code:: scala

  def charLaw: Prop =
    Prop.forAll(Gen.stringN(1))(s =>
      run(char(s.charAt(0)))(s) == Right(s.charAt(0)))


Higher kindedness
-----------------

What's with ``Parsers[Parser[+_]]``?

Higher kindedness - type constructors
-------------------------------------

.. ReST didn't handle the second indent - fall back to latex

.. raw:: latex

  \begin{itemize}
    \item A \emph{proper} type is one that classifies values
    \begin{itemize}
      \item \texttt{String} classifies values but \texttt{List} does not
    \end{itemize}
    \item \texttt{List} is a \emph{type constructor} and not a proper type
    \item Type constructors are like functions at the type level
  \end{itemize}

Higher kindedness - kinds and order
-----------------------------------

- *kinds* are sometimes referred to as the types of types
- A type's kind captures the type arguments, if any, that are required to produce a proper type
- ``Int`` has kind ``*``, and ``List`` has kind ``* -> *``
- ``Parsers`` has kind ``(* -> *) -> *``
  - Takes a type constructor as a type argument
  - It's a *higher order* type constructor, or *higher kinded type* [#]_

.. [#] See http://stackoverflow.com/questions/6246719/what-is-a-higher-kinded-type-in-scala for more detail

Primitives
----------

Now we can go nuts adding all of the combinators we need.

.. code:: scala

  trait Parsers[Parser[+_]] {
    ...
    def string(s: String): Parser[String]
    def orString(s1: String, s2: String): Parser[String]
  }

Is ``orString`` really primitive?
-------------------------------------

- ``orString`` doesn't seem primitive enough
- Alternation should work for parsers of any type

.. code:: scala
 
  def or[A](p1: Parser[A], p2: Parser[A]): Parser[A]

.. Sugar
.. -----
.. 
.. :latex:`\fontsize{8pt}{10}\selectfont`
.. 
.. .. code:: scala
.. 
..   trait Parsers[ParseError, Parser[+_]] { self => ...
.. 
..     def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]
..     implicit def string(s: String): Parser[String]
..     implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
..     implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]):
..       ParserOps[String] = ParserOps(f(a))
.. 
..     case class ParserOps[A](p: Parser[A]) {
..       def |[B>:A](p2: Parser[B]): Parser[B] = self.or(p,p2)
..       def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p,p2)
..     }
..   }

.. Let's fix Vim's syntax highlighting... ||

Another primitive: repetition
-----------------------------

- It seems very likely we'll want to capture repetitions of ``Parser``
- e.g. We want to parse 10 'a' characters in a row, or 5 instances of some string

.. code:: scala

    def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]

Refinement
----------

- At this point we have the flavour of the process
- Keep adding combinators and refining them back to the simplest and most general
  forms that are still practical
- Ask the questions
  + Should this work for other types?
  + What are the properties/laws I expect to hold?
  + What should the semantics of a combinator be? Are there alternatives that would be more useful?

Fleshing out our algebra
------------------------

- An exercise for the reader
- Still no implementation of ``Parser`` or ``ParseError``
- Algebra specifies information available to implementations
  - Smaller surface area for users
  - Restricts possible implementations

Context sensitive grammar
-------------------------

- Context sensitivity is an important characterisitc of grammars
- Input dictates how subsequent input is parsed
- ``"1a", "2bb", "3ccc", ...``
- ``def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B]``

Implementing a JSON parser and ``Parser`` type
--------------------------------------------------

- Another exercise for the reader

Summary
--------

- Algebraic design
  + Remove distraction of implementation details
  + Use compiler as a sanity check
  + Laws/properties give us useful checks for implementation
  + Challenging to work in the abstract if you're not used to it

Part 3: Common structures
=========================

What's coming up
----------------

- We've developed a number of libraries in earlier parts
- Now we abstract some patterns seen across them
- Start with ``Monoid`` and ``Foldable``

It's more than a theory
-----------------------

- Avoid duplicated code
- Less cognitive load when dealing in well known abstractions 
- Common language to talk about these structures
- Overlap with mathematics means we can steal

Chapter 10: Monoids
===================

It starts with ``Monoid``
-----------------------------

- Simple and ubiquitous
- Useful for parallelisation
- Can compose simple pieces to build more complex calculations 

Looking for the pattern
-----------------------

.. code:: scala

  val stringExample = "" + "foo" + "bar" + ""
  val intExample    = 1 * 2 * 3 * 4 * 1
  val andExample    = true && true && false && true
  val orExample     = false || true || false || false || false

----

Two things in common

#. a *binary* *associative* operation
#. an *identity* element

Abstraction from the pattern
----------------------------

.. code:: scala

  trait Monoid[A] {
    def op(a1: A, a2: A): A
    def zero: A
  }

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }

Anarchy is overrated
--------------------

.. code:: scala

  // Associativity
  op(a1, op(a2, a3)) == op(op(a1, a2), a3)

  // Left and right identity
  op(zero, a) == op(a, zero) == a

We can use property based testing to ensure each ``Monoid`` instance obeys the laws

.. That's what makes it a ``Monoid``!
.. --------------------------------------
.. 
.. - Algebraic abstraction
.. - Binary associative operation with an identity
.. - Obeys laws of associativity, and left and right identity
.. - Instances are technically *not* ``Monoids`` - the abstraction is

Folding ``Monoid``s
-----------------------

.. code:: scala

  foldLeft[B](z: B)(f: (A, B) => B): B
  foldRight[B](z: B)(f: (B, A) => B): B

----

What if ``A == B``

.. code:: scala

   foldLeft[B](z: B)(f: (B, B) => B): B
   foldRight[B](z: B)(f: (B, B) => B): B

----

Both ``foldLeft`` and ``foldRight`` give the same result because laws

.. code:: scala

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  // This one is homework
  def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B

Associativity + parallelism
---------------------------

Associativity of ``Monoid`` means we can fold in either direction

.. code:: scala

  op(a, op(b, op(c, d))) == op(op(op(a, b), c), d)

We can also do a balanced fold

.. code:: scala

  op(op(a, b), op(c, d))

----

The unbalanced fold concatenates each element in sequence

.. code:: scala

  List("a", "b", "c", "d", "e", "f").foldLeft("")(_ + _)
  List("b", "c", "d", "e", "f").foldLeft("a")(_ + _)
  List("c", "d", "e", "f").foldLeft("ab")(_ + _)
  List("d", "e", "f").foldLeft("abc")(_ + _)
  List("e", "f").foldLeft("abcd")(_ + _)
  List("f").foldLeft("abcde")(_ + _)
  List().foldLeft("abcdef")(_ + _)
  "abcdef"

----

The balanced structure means we can form a tree of work

.. code:: scala

  op(op(op("a", "b"), "c"), op(op("d", "e"), "f"))
  op(op("ab", "c"), op("de", "f"))
  op("abc", "def")
  "abcdef"

Monoid homomorphisms
--------------------

``length`` is a *monoid homomorphism* between the string
concatenation and integer addition monoids.

.. code:: scala

  length(S.op("foo", "bar"))
  I.op(length("foo"), length("bar"))
  // We can use our homomorphism with foldMap
  foldMap(List("foo", "bar"), I)(length)

In general, functions between types that preserve ``Monoid`` structure

.. code:: scala

  f(M.op(a1, a2)) == N.op(f(b1), f(b2))

Monoid isomorphisms
-------------------

- Two homomorphisms between types: ``f`` and ``g``
- e.g. ``f: A => B`` and ``g: B => A``
- ``f andThen g`` and ``g andThen f`` are both the identity function
- Monoids for concatenation of ``String`` and ``List[Char]``

Foldable data structures
------------------------

.. code:: scala

  trait Foldable[F[_]] {
    def foldRight[A,B](as: F[A])(z: B)(f: (A,B) => B): B
    def foldLeft[A,B](as: F[A])(z: B)(f: (B,A) => B): B
    def foldMap[A,B](as: F[A])(f: A => B)(mb: Monoid[B]): B
    def concatenate[A](as: F[A])(m: Monoid[A]): A =
            foldLeft(as)(m.zero)(m.op)
  }

Composing ``Monoid``s
-------------------------

- ``Monoid`` instances on their own aren't that compelling
- Their composability makes them more powerful

.. code:: scala

  // Implementing this is an exercise
  def productMonoid[A,B](A: Monoid[A], B: Monoid[B]):
    Monoid[(A,B)]

----

:latex:`\fontsize{10pt}{12}\selectfont`

.. code:: scala

  def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K, V]] =
    new Monoid[Map[K, V]] {
      def zero = Map[K,V]()
      def op(a: Map[K, V], b: Map[K, V]) =
        (a.keySet ++ b.keySet).foldLeft(zero) { (acc,k) =>
          acc.updated(k, V.op(a.getOrElse(k, V.zero),
                              b.getOrElse(k, V.zero)))
        }
    }

----

.. code:: scala

  val M: Monoid[Map[String, Map[String, Int]]] =
    mapMergeMonoid(mapMergeMonoid(intAddition))
  
  val m1 = Map("o1" -> Map("i1" -> 1, "i2" -> 2))
  val m2 = Map("o1" -> Map("i2" -> 3))
  val m3 = M.op(m1, m2)

  // => Map(o1 -> Map(i1 -> 1, i2 -> 5))

Fusing traversals
-----------------

We can compose ``Monoid`` instances to perform multiple calculations in one pass.

.. code:: raw

  scala> val m = productMonoid(intAddition, intAddition)
  scala> val l = List(1,2,3,4)
  scala> val p = listFoldable.foldMap(l)(a => (1, a))(m)
  p: (Int, Int) = (4, 10)
  scala> val mean = p._1 / p._2.toDouble
  mean: Double = 2.5

We can develop combinators for doing this more easily - see chapter notes

Summary
-------

- Abstractions for common patterns have multiple benefits
- Obey the laws!
- ``Monoid`` is particularly good for parallel computation and fusing traversals
