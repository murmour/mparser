
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

   Module MParser_Sig:
     Common module signatures.
*)


(* The interface of a pluggable regular expression engine. *)
module type Regexp = sig

  type t
  (** The type of a regular expression *)

  type substrings
  (** Substrings matched by a regular expression *)

  val make: string -> t
  (** Compiles a regular expression *)

  val get_substring: substrings -> int -> string option
  (** Extracts a single substring.
      Returns None if the group did not match *)

  val get_all_substrings: substrings -> string array
  (** Extracts all the matched substrings.
      Includes the full match at index 0.
      If a subpattern did not capture a substring, the empty
      string is returned in the corresponding position instead *)

  val exec: rex: t -> pos: int -> string -> substrings option
  (** Attemts to match the string with a regular expression,
      starting from the position [pos]. Returns [None] on failure *)

end


(* The interface of a pluggable channel data structure. *)
module type Channel = sig
  module Monad: MParser_Monad.Complete

  type t
  (** Channel type *)

  val length: t -> int Monad.t
  (** Return the size (number of characters) of the regular file
      on which the given channel is opened.  If the channel is opened
      on a file that is not a regular file, the result is meaningless.
      The returned size does not take into account the end-of-line
      translations that can be performed when reading from a channel
      opened in text mode. *)

  val position: t -> int Monad.t
  (** Return the current reading position for the given channel. *)

  val set_position: t -> int -> unit Monad.t
  (** Return the current reading position for the given channel. *)

  val read: t -> string -> int -> int -> int Monad.t
  (** [input ic buf pos len] reads up to [len] characters from
      the given channel [ic], storing them in string [buf], starting at
      character number [pos].
      It returns the actual number of characters read, between 0 and
      [len] (inclusive).
      A return value of 0 means that the end of file was reached.
      A return value between 0 and [len] exclusive means that
      not all requested [len] characters were read, either because
      no more characters were available at that time, or because
      the implementation found it convenient to do a partial read;
      [input] must be called again to read the remaining characters,
      if desired.  (See also {!Pervasives.really_input} for reading
      exactly [len] characters.)
      Exception [Invalid_argument "input"] is raised if [pos] and [len]
      do not designate a valid substring of [buf]. *)

end
