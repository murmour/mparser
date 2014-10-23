
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


let ( |> ) f x = x f


module String = struct
  include String

  let match_sub s start pat =
    let len_s = String.length s in
    let len_pat = String.length pat in
    if 0 <= start && start < len_s then
      if start + len_pat > len_s then
        false
      else
        let rec compare i =
          if i >= len_pat then true
          else if String.unsafe_get pat i <>
                  String.unsafe_get s (start + i) then
            false
          else
            compare (i + 1)
        in
        compare 0
    else
      invalid_arg "MParser_Utils.String.match_sub: Invalid index"

  let match_sub2 s1 i1 s2 i2 n =
    if 0 <= i1 && i1 + n <= String.length s1 &&
       0 <= i2 && i2 + n <= String.length s2 then
      let rec compare i =
        if i >= n then
          true
        else if String.unsafe_get s1 (i1 + i) <>
                String.unsafe_get s2 (i2 + i) then
          false
        else
          compare (i + 1)
      in
      compare 0
    else
      invalid_arg "MParser_Utils.String.match_sub2: Invalid index"

  let unique =
    let module SSet = Set.Make (String) in
    fun l -> SSet.elements (List.fold_right SSet.add l SSet.empty)

  let for_all p a =
    let rec for_all i =
      if i >= String.length a then
        true
      else if p (String.unsafe_get a i) then
        for_all (i + 1)
      else
        false
    in
    for_all 0

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
