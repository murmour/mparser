
(* MParser, a simple monadic parser combinator library
   -----------------------------------------------------------------------------
   Copyright (C) 2008, Holger Arnold
                 2014-2016, Max Mouratov

   License:
     This library is free software; you can redistribute it and/or
     modify it under the terms of the GNU Library General Public
     License version 2.1, as published by the Free Software Foundation.

     This library is distributed in the hope that it will be useful,
     but WITHOUT ANY WARRANTY; without even the implied warranty of
     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

     See the GNU Library General Public License version 2.1 for more details
     (enclosed in the file LICENSE.txt).

   Module MParser_Sig:
     Common module signatures.
*)


(** A pluggable regular expression engine. *)
module type Regexp = sig

  type t
  (** A compiled regular expression. *)

  type substrings
  (** Substrings matched by a regular expression. *)


  val make: string -> t
  (** Compiles a regular expression. *)

  val get_substring: substrings -> int -> string option
  (** Extracts a single substring.
      Returns None if the group did not match. *)

  val get_all_substrings: substrings -> string array
  (** Extracts all the matched substrings.
      Includes the full match at index 0.
      If a subpattern did not capture a substring, the empty
      string is returned in the corresponding position instead. *)

  val exec: rex: t -> pos: int -> string -> substrings option
  (** Attemts to match the string with a regular expression, starting
      from the position [pos]. Returns the matched substrings or [None]
      on failure. *)

end
