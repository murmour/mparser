(* OASIS_START *)
(* DO NOT EDIT (digest: 7790f9a28e5e40fca1c874c0c0fa244f) *)
This is the README file for the mParser distribution.

A simple monadic parser combinator library

This library implements a rather complete and efficient monadic parser
combinator library similar to the Parsec library for Haskell by Daan Leijen
and the FParsec library for FSharp by Stephan Tolksdorf.

See the files INSTALL.txt for building and installation instructions. See the
file LICENSE.txt for copying conditions. 

Home page: https://bitbucket.org/mrm/mparser


(* OASIS_STOP *)


mParser used to be a part of ocaml-base, a collection of useful OCaml
libraries by Holger Arnold [1].

It is possible to use mParser with pa_monad [2].

mParser depends on the PCRE-OCaml library [3].


References
----------

[1] ocaml-base Homepage
    http://www.holgerarnold.net/software

[2] Syntax extension for Monads in OCaml
    http://www.cas.mcmaster.ca/~carette/pa_monad/

[3] Markus Mottl's OCaml software (PCRE-OCaml)
    http://www.ocaml.info/home/ocaml_sources.html
