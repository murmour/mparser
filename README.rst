===================================================
MParser, a simple monadic parser combinator library
===================================================

This library implements a rather complete and efficient monadic parser
combinator library similar to the Parsec library for Haskell by Daan Leijen
and the FParsec library for FSharp by Stephan Tolksdorf.

See LICENSE.txt for copying conditions (LGPL with static linking exception).

Home page: https://github.com/murmour/mparser.

MParser used to be a part of ocaml-base, a collection of useful OCaml
libraries by Holger Arnold [1]_.

The monadic interface of MParser is compatible with pa_monad [2]_.


Dependencies
------------

To build this package, you need:

* OCaml (>= 4.02).
* Dune (>= 1.11) [3]_.
* Findlib [4]_.
* Optionally: ``re`` [5]_ for ``mparser-re``.
* Optionally: ``pcre-ocaml`` [6]_ for ``mparser-pcre``.


Installing
----------

Either use OPAM [7]_ or build manually with Dune [3]_.

Installing from OPAM: `opam install [sub-library name]`.

To build manually, `cd` to this folder and run `dune build -p [sub-library name]`. Add `@install` for installation. Add `@doc` to produce API reference in `_build/default/_doc/_html`. Consult Dune manual for more options.

Available sub-libraries:

- ``mparser``: base library;
- ``mparser-re``: a plugin that adds support for regular expressions based on
  ``re`` [5]_ (``MParser_RE`` module, ``mparser-re`` findlib package);
- ``mparser-pcre``: a plugin that adds support for regular expressions based on
  ``pcre-ocaml`` [6]_ (``MParser_PCRE`` module, ``mparser-pcre`` findlib package).


Usage example
-------------

Let's implement a simple expression evaluator.

To save the typing effort, it is often handy to open the ``MParser`` module:

.. sourcecode:: ocaml

  open MParser


First, we define a parsing combinator ``expr``, which handles expression
parsing, taking care of the operator precedence issues:

.. sourcecode:: ocaml

  let infix p op =
    Infix (p |>> (fun _ a b -> (`Binop (op, a, b))), Assoc_left)

  let operators =
    [
      [
        infix (char '*') `Mul;
        infix (char '/') `Div;
      ];
      [
        infix (char '+') `Add;
        infix (char '-') `Sub;
      ];
    ]

  let decimal =
    many1_chars digit |>> int_of_string

  let expr =
    expression operators (decimal |>> fun i -> `Int i)


Next, we implement an interpreter for our expression tree:

.. sourcecode:: ocaml

  let rec calc = function
    | `Int i -> i
    | `Binop (op, a, b) ->
        match op with
          | `Add -> calc a + calc b
          | `Sub -> calc a - calc b
          | `Mul -> calc a * calc b
          | `Div -> calc a / calc b


The evaluator function:

.. sourcecode:: ocaml

  let eval (s: string) : int =
    match MParser.parse_string expr s () with
      | Success e ->
          calc e
      | Failed (msg, e) ->
          failwith msg


Using it:

.. sourcecode:: ocaml

  eval "4*4+10/2"  ->  21


Have fun!


References
----------

.. [1] http://www.holgerarnold.net/software
.. [2] https://www.cas.mcmaster.ca/~carette/pa_monad
.. [3] https://github.com/ocaml/dune
.. [4] http://projects.camlcity.org/projects/findlib.html
.. [5] https://github.com/ocaml/ocaml-re
.. [6] https://mmottl.github.io/pcre-ocaml
.. [7] https://opam.ocaml.org
