(lang dune 1.11)

(license "LGPL-2.1 with OCaml linking exception")
(name mparser)
(version 1.3)

(authors "Holger Arnold <holger@harnold.org>" "Max Mouratov <mmouratov@gmail.com>")
(maintainers "Max Mouratov <mmouratov@gmail.com>")
(source (github murmour/mparser))

(package
 (name mparser)
 (synopsis "A simple monadic parser combinator library")
 (description "
This library implements a rather complete and efficient monadic parser
combinator library similar to the Parsec library for Haskell by Daan Leijen and
the FParsec library for FSharp by Stephan Tolksdorf.")
 (depends
  (ocaml (>= 4.02))))

(package
 (name mparser-pcre)
 (synopsis "MParser plugin: PCRE-based regular expressions")
 (depends
  (ocaml (>= 4.02))
  mparser
  pcre))

(package
 (name mparser-re)
 (synopsis "MParser plugin: RE-based regular expressions")
 (depends
  (ocaml (>= 4.02))
  mparser
  (re (>= 1.7.2))))

(generate_opam_files true)
