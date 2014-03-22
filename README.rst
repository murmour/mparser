===================================================
MParser, a simple monadic parser combinator library
===================================================

This library implements a rather complete and efficient monadic parser
combinator library similar to the Parsec library for Haskell by Daan Leijen
and the FParsec library for FSharp by Stephan Tolksdorf.

See the file INSTALL.txt for building and installation instructions.
See the file LICENSE.txt for copying conditions.

Home page: https://bitbucket.org/cakeplus/mparser


MParser used to be a part of ocaml-base, a collection of useful OCaml
libraries by Holger Arnold [1]_.

The monadic interface of MParser is compatible with pa_monad [2]_.

The only dependency is the PCRE-OCaml library [3]_.


Usage example
-------------

Let's implement a simple expression evaluator.

To save the typing effort, it is often handy to open the ``MParser`` module:

.. sourcecode:: ocaml

  open MParser


First, we define a parsing combinator ``expr``, which handles expression
parsing, taking care of the operator precedence issues:

.. sourcecode:: ocaml

  let infix p o =
    Infix (p |>> (fun _ a b -> (`Binop (o, a, b))), Assoc_left)

  let operators =
    [ [ infix (char '*') `Mul;
        infix (char '/') `Div ];
      [ infix (char '+') `Add;
        infix (char '-') `Sub ] ]

  let expr =
    expression operators (Tokens.decimal |>> fun i -> `Int i)


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
      | Success e -> calc e
      | Failed (msg, e) -> failwith msg


Using it:

.. sourcecode:: ocaml

  eval "4*4+10/2"  ->  21


Have fun!


References
----------

.. [1] http://www.holgerarnold.net/software
.. [2] http://www.cas.mcmaster.ca/~carette/pa_monad
.. [3] http://www.ocaml.info/home/ocaml_sources.html
