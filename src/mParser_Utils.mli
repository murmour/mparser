
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

   Module MParser_Utils:
     General purpose functions.
*)


val ( |> ): 'a -> ('a -> 'b) -> 'b
(** Function application. [x |> f] is equivalent to [f x]. *)


module IO: sig

  val input: in_channel -> string -> int -> int -> int
  (** [input chn buffer pos length] reads up to [length] characters from the
      channel [chn] and stores them in the string [buffer], starting at position
      [pos].  It returns the actual number of characters read.  A value less
      than [length] is only returned if there are less than [length] characters
      available from [chn] (the [input] function in the [Pervasives] module is
      allowed to read less than [length] characters if it "finds it convenient
      to do a partial read").

      @raise Invalid_argument if [pos] and [length] do not specify a valid
      substring of [buffer]. *)

end


module String: sig
  include module type of String

  val match_sub: string -> int -> string -> bool
  (** [match_sub s start pat] returns [true] if the string [s] contains the
      string [pat] as a substring starting at position [start], and [false]
      otherwise.

      @raise Invalid_argument if [start] is no valid index in [s]. *)

  val match_sub2: string -> int -> string -> int -> int -> bool
  (** [match_sub2 s1 start1 s2 start2 len] returns [true] if [sub s1 start1
      len = sub s2 start2 len], and [false] otherwise.

      @raise Invalid_argument if [start1], [start2], and [len] do not specify
      valid substrings of [s1] and [s2]. *)

  val unique: string list -> string list
  (** Returns the sorted list of unique elements in the list of strings [l] *)

  val for_all: (char -> bool) -> string -> bool
  (** [for_all p s] returns [true] if [p c = true] for all characters [c] of
      [s], and [false] otherwise. *)

end


module Char: sig
  include module type of Char

  val is_lowercase: t -> bool
  (** Returns [true] if the given character is an english lowercase letter. *)

  val is_uppercase: t -> bool
  (** Returns [true] if the given character is an english uppercase letter. *)

  val is_letter: t -> bool
  (** Returns [true] if the given character is an english letter. *)

  val is_digit: t -> bool
  (** Returns [true] if the given character is a decimal digit. *)

  val is_hex_digit: t -> bool
  (** Returns [true] if the given character is a hexadecimal digit. *)

  val is_oct_digit: t -> bool
  (** Returns [true] if the given character is an octal digit. *)

  val is_alphanum: t -> bool
  (** Returns [true] if the given character is a letter or a digit. *)

  val is_blank: t -> bool
  (** Returns [true] if the given character is a space or a tab
      ([' '] or ['\t']). *)

end
