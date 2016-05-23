Where are we?
=============

Part 1: Intro to FP
-------------------

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

Chapter 9: Parsers
==================

It's the journey man
--------------------

.. image:: journey.jpg

- Focus is on *algebraic design*



What's a parser?
----------------

    A parser for things is a function from strings to lists of pairs of things and strings.
    -- Conor McBride [#]_

.. [#] `via @hackuador on twitter <https://twitter.com/hackuador/status/72567583412035993>`_
    
Really?
-------

- Kind of - the book defines it differently
  
- Unstructured data to structured
- e.g. `String` to `Sale` data type

