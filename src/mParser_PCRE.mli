
(* MParser, a simple monadic parser combinator library
   -----------------------------------------------------------------------------
   Copyright (C) 2008, Holger Arnold
                 2014, Max Mouratov

   License:
     This library is free software; you can redistribute it and/or
     modify it under the terms of the GNU Library General Public
     License version 2.1, as published by the Free Software Foundation.

     This library is distributed in the hope that it will be useful,
     but WITHOUT ANY WARRANTY; without even the implied warranty of
     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

     See the GNU Library General Public License version 2.1 for more details
     (enclosed in the file LICENSE.txt).

   Module MParser_PCRE:
     PCRE-based regular expression parsers.
*)


module Regexp: MParser_Regexp.Sig

include module type of MParser.MakeRx (Regexp)


(** Predefined tokens parsers.

    This module provides parsers for tokens that are commonly used in parsing
    computer languages.  All parsers in this module skip the spaces (as
    defined by the {!MParser.spaces} parser) that occur after a token.  Where
    they are applied to a user-defined parser [p], however, they do not skip
    the spaces occurring after the characters parsed by [p].  For example,
    [parens p] is equivalent to [char '(' >> spaces >> p << char ')' << spaces].
*)
module Tokens:
sig
  open MParser

  val symbol: string -> (string, 's) parser
  (** [symbol sym] parses the literal string [sym] and returns it. *)

  val skip_symbol: string -> (unit, 's) parser
  (** [skip_symbol sym] parses the literal string [sym] and returns [()]. *)

  val parens: ('a, 's) parser -> ('a, 's) parser
  (** [parens p] parses [p] between parentheses ['('] and [')']. *)

  val braces: ('a, 's) parser -> ('a, 's) parser
  (** [braces p] parses [p] between curly braces ['{'] and ['}']. *)

  val brackets: ('a, 's) parser -> ('a, 's) parser
  (** [brackets p] parses [p] between angle brackets ['<'] and ['>']. *)

  val squares: ('a, 's) parser -> ('a, 's) parser
  (** [squares p] parses [p] between square brackets ['\['] and ['\]']. *)

  val semi: (char, 's) parser
  (** Parses a semicolon [';']. *)

  val comma: (char, 's) parser
  (** Parses a comma [',']. *)

  val colon: (char, 's) parser
  (** Parses a colon [':']. *)

  val dot: (char, 's) parser
  (** Parses a dot ['.']. *)

  val semi_sep: ('a, 's) parser -> ('a list, 's) parser
  (** [semi_sep p] parses zero or more occurrences of [p], separated by [';'].
      It returns a list of the results returned by [p]. *)

  val semi_sep1: ('a, 's) parser -> ('a list, 's) parser
  (** [semi_sep1 p] parses one or more occurrences of [p], separated by [';'].
      It returns a list of the results returned by [p]. *)

  val semi_sep_end: ('a, 's) parser -> ('a list, 's) parser
  (** [semi_sep_end p] parses zero or more occurrences of [p], separated and
      optionally ended by [';'].  It returns a list of the results returned by
      [p]. *)

  val semi_sep_end1: ('a, 's) parser -> ('a list, 's) parser
  (** [semi_sep_end1 p] parses one or more occurrences of [p], separated and
      optionally ended by [';'].  It returns a list of the results returned by
      [p]. *)

  val semi_end: ('a, 's) parser -> ('a list, 's) parser
  (** [semi_end p] parses zero or more occurrences of [p], separated and ended
      by [';'].  It returns a list of the results returned by [p]. *)

  val semi_end1: ('a, 's) parser -> ('a list, 's) parser
  (** [semi_sep_end1 p] parses one or more occurrences of [p], separated and
      ended by [';'].  It returns a list of the results returned by [p]. *)

  val comma_sep: ('a, 's) parser -> ('a list, 's) parser
  (** [comma_sep p] parses zero or more occurrences of [p], separated by
      [','].  It returns a list of the results returned by [p]. *)

  val comma_sep1: ('a, 's) parser -> ('a list, 's) parser
  (** [comma_sep1 p] parses one or more occurrences of [p], separated by
      [','].  It returns a list of the results returned by [p]. *)

  val char_literal: (char, 's) parser
  (** Parses a character literal as defined in the OCaml language and returns
      the character.  The literal may contain an escape sequence. *)

  val string_literal: (string, 's) parser
  (** Parses a string literal as defined in the OCaml language and returns the
      string.  The literal may contain escape sequences. *)

  val decimal: (int, 's) parser
  (** Parses a decimal natural number and returns it as an integer value.
      Fails with a [Message_error] if the parsed number is larger than
      [max_int]. *)

  val hexadecimal: (int, 's) parser
  (** Parses a hexadecimal natural number as defined in the OCaml language
      (prefixed with ["0x"] or ["0X"]) and returns it as an integer value.
      Fails with a [Message_error] if the parsed number is larger than
      [max_int]. *)

  val octal: (int, 's) parser
  (** Parses an octal natural number as defined in the OCaml language
      (prefixed with ["0o"] or ["0O"]) and returns it as an integer value.
      Fails with a [Message_error] if the parsed number is larger than
      [max_int]. *)

  val binary: (int, 's) parser
  (** Parses a binary natural number as defined in the OCaml language
      (prefixed with ["0b"] or ["0B"]) and returns it as an integer value.
      Fails with a [Message_error] if the parsed number is larger than
      [max_int]. *)

  val integer: (int, 's) parser
  (** Parses a decimal integer number and returns its value.  Fails with a
      [Message_error] if the parsed number is smaller than [min_int] or larger
      than [max_int]. *)

  val float: (float, 's) parser
  (** Parses floating-point literal as defined in the OCaml language and
      returns its value.  Fails with a [Message_error] if the parsed number is
      not a valid representation of a [float] value. *)

end
