
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

   Module MParser_Utils:
     General purpose functions.
*)


let ( |> ) f x = x f


module IO = struct

  let input chn buffer pos length =
    if pos < 0 || pos + length > String.length buffer then
      invalid_arg "MParser_Utils.IO.input: invalid substring"
    else
      let rec iter chars_read =
        let pos' = pos + chars_read in
        let length' = length - chars_read in
        let chars = Pervasives.input chn buffer pos' length' in
        if chars > 0 then
          iter (chars_read + chars)
        else
          chars_read
      in
      iter 0

end


module String = struct
  include String

  let match_sub s start pat =
    let len_s = String.length s in
    let len_pat = String.length pat in
    if not (start >= 0 && start <= len_s) then
      invalid_arg "MParser_Utils.String.match_sub: invalid index"
    else if start + len_pat > len_s then
      false
    else
      let rec iter i =
        if i >= len_pat then
          true
        else if String.unsafe_get pat i <>
                String.unsafe_get s (start + i) then
          false
        else
          iter (i + 1)
      in
      iter 0

  let match_sub2 s1 i1 s2 i2 n =
    if not (i1 >= 0 && i1 + n <= String.length s1 &&
            i2 >= 0 && i2 + n <= String.length s2) then
      invalid_arg "MParser_Utils.String.match_sub2: invalid index"
    else
      let rec iter i =
        if i >= n then
          true
        else if String.unsafe_get s1 (i1 + i) <>
                String.unsafe_get s2 (i2 + i) then
          false
        else
          iter (i + 1)
      in
      iter 0

  let unique =
    let module SSet = Set.Make (String) in
    fun l -> SSet.elements (List.fold_right SSet.add l SSet.empty)

  let for_all p a =
    let rec iter i =
      if i >= String.length a then
        true
      else if p (String.unsafe_get a i) then
        iter (i + 1)
      else
        false
    in
    iter 0

end


module Char = struct
  include Char

  let is_lowercase c =
    'a' <= c && c <= 'z'

  let is_uppercase c =
    'A' <= c && c <= 'Z'

  let is_letter c =
    is_lowercase c || is_uppercase c

  let is_digit c =
    '0' <= c && c <= '9'

  let is_hex_digit c =
       ('0' <= c && c <= '9')
    || ('a' <= c && c <= 'f')
    || ('A' <= c && c <= 'F')

  let is_oct_digit c =
    ('0' <= c && c <= '7')

  let is_alphanum c =
    is_letter c || is_digit c

  let is_blank c =
    c = ' ' || c = '\t'

end
