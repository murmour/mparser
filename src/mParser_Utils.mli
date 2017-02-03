
(* MParser, a simple monadic parser combinator library
   -----------------------------------------------------------------------------
   Copyright (C) 2008, Holger Arnold
                 2014-2017, Max Mouratov

   License:
     This library is free software; you can redistribute it and/or
     modify it under the terms of the GNU Library General Public
     License version 2.1, as published by the Free Software Foundation.

     This library is distributed in the hope that it will be useful,
     but WITHOUT ANY WARRANTY; without even the implied warranty of
     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

     See the GNU Library General Public License version 2.1 for more details
     (enclosed in the file LICENSE.txt).
*)

(** General purpose utilities. *)


val ( |> ): 'a -> ('a -> 'b) -> 'b
(** Function application. [x |> f] is equivalent to [f x]. *)


module IO: sig

  val input: in_channel -> Bytes.t -> int -> int -> int
  (** [input chn b pos length] reads up to [length] characters from the
      channel [chn] and stores them in the byte-buffer [b], starting at position
      [pos]. It returns the actual number of characters read. A value less
      than [length] is only returned if there are less than [length] characters
      available from [chn] (the [input] function in the [Pervasives] module is
      allowed to read less than [length] characters if it "finds it convenient
      to do a partial read").

      @raise Invalid_argument if [pos] and [length] do not specify a valid
      substring of [b]. *)

end


module String: sig
  include module type of String

  val unique: string list -> string list
  (** Returns the sorted list of unique elements in the list of strings [l] *)

  val for_all: (char -> bool) -> string -> bool
  (** [for_all p s] returns [true] if [p c = true] for all characters [c] of
      [s], and [false] otherwise. *)

end


module Bytes: sig
  include module type of Bytes

  val match_sub: Bytes.t -> int -> string -> bool
  (** [match_sub b start pat] returns [true] if the byte-buffer [b] contains
      the string [pat] as a substring starting at position [start], and
      [false] otherwise.

      @raise Invalid_argument if [start] is no valid index in [b]. *)

  val match_sub2: Bytes.t -> int -> string -> int -> int -> bool
  (** [match_sub2 b1 start1 s2 start2 len] returns [true] if [Bytes.sub b1
      start1 len = String.sub s2 start2 len], and [false] otherwise.

      @raise Invalid_argument if [start1], [start2], and [len] do not specify
      valid substrings of [b1] and [s2]. *)

end


module Char: sig
  include module type of Char

  val is_lowercase: t -> bool
  (** Returns [true] if the given character is an English lowercase letter. *)

  val is_uppercase: t -> bool
  (** Returns [true] if the given character is an English uppercase letter. *)

  val is_letter: t -> bool
  (** Returns [true] if the given character is an English letter. *)

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
