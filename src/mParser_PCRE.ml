
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


module Regexp = struct

  type t = Pcre.regexp
  type substrings = Pcre.substrings


  let make pattern =
    Pcre.regexp pattern

  let get_substring s idx =
    try
      Some (Pcre.get_substring s idx)
    with Not_found | Invalid_argument _ ->
      None

  let get_all_substrings s =
    Pcre.get_substrings s

  let default_flags =
    Pcre.rflags [ `ANCHORED ]

  let exec ~rex ~pos s =
    try
      Some (Pcre.exec ~pos ~iflags:default_flags ~rex s)
    with Not_found ->
      None

end

include MParser.MakeRx (Regexp)


(* Token parsers
   -------------------------------------------------------------------------- *)

module Tokens = struct
  open MParser

  let symbol s =
    string s << spaces

  let skip_symbol s =
    skip_string s << spaces

  let char_sp c =
    char c << spaces

  let parens p =
    between (char_sp '(') (char_sp ')') p

  let braces p =
    between (char_sp '{') (char_sp '}') p

  let brackets p =
    between (char_sp '<') (char_sp '>') p

  let squares p =
    between (char_sp '[') (char_sp ']') p

  let semi s =
    char_sp ';' s

  let comma s =
    char_sp ',' s

  let colon s =
    char_sp ':' s

  let dot s =
    char_sp '.' s

  let semi_sep p =
    sep_by p semi

  let semi_sep1 p =
    sep_by1 p semi

  let semi_sep_end p =
    sep_end_by p semi

  let semi_sep_end1 p =
    sep_end_by1 p semi

  let semi_end p =
    end_by p semi

  let semi_end1 p =
    end_by1 p semi

  let comma_sep p =
    sep_by p comma

  let comma_sep1 p =
    sep_by1 p comma

  let escaped_char s =
    (any_of "nrtb\\\"\'" |>> (function
       | 'n' -> '\n'
       | 'r' -> '\r'
       | 't' -> '\t'
       | 'b' -> '\b'
       | c   -> c)) s

  let escape_sequence_dec =
    let int_of_dec c =
      (Char.code c) - (Char.code '0') in
    let char_of_digits d2 d1 d0 =
      char_of_int (100 * (int_of_dec d2) + 10 * (int_of_dec d1)
                   + (int_of_dec d0))
    in
      fun s ->
        (digit >>= fun d2 ->
         digit >>= fun d1 ->
         digit >>= fun d0 ->
         try_return3 char_of_digits d2 d1 d0
           "Escape sequence is no valid character code" s) s

  let escape_sequence_hex =
    let int_of_hex c =
      if      '0' <= c && c <= '9' then (Char.code c) - (Char.code '0')
      else if 'a' <= c && c <= 'f' then (Char.code c) - (Char.code 'a') + 10
      else if 'A' <= c && c <= 'F' then (Char.code c) - (Char.code 'A') + 10
      else failwith "MParser.int_of_hex: no hex digit" in
    let char_of_digits h1 h0 =
      char_of_int (16 * (int_of_hex h1) + (int_of_hex h0))
    in
      fun s ->
        (char 'x'  >>
         hex_digit >>= fun h1 ->
         hex_digit >>= fun h0 ->
         try_return2 char_of_digits h1 h0
           "Escape sequence is no valid character code" s) s

  let escape_sequence s =
       (escape_sequence_dec
    <|> escape_sequence_hex) s

  let char_token s =
       ((char '\\' >> (escaped_char <|> escape_sequence))
    <|>  any_char) s

  let char_literal s =
    (char '\'' >> char_token << char_sp '\''
     <?> "character literal") s

  let string_literal s =
    (char '"' >> (many_chars_until char_token (char_sp '"'))
     <?> "string literal") s

  let decimal_r =
    make_regexp "\\d+"

  let hexadecimal_r =
    make_regexp "0(x|X)[0-9a-fA-F]+"

  let octal_r =
    make_regexp "0(o|O)[0-7]+"

  let binary_r =
    make_regexp "0(b|B)[01]+"

  let integer_r =
    make_regexp "-?\\d+"

  let float_r =
    make_regexp "-?\\d+(\\.\\d*)?((e|E)?(\\+|-)?\\d+)?"

  let decimal s =
    (regexp decimal_r >>= fun digits ->
     spaces >>
     try_return int_of_string digits "Decimal value out of range" s
     <?> "decimal value") s

  let hexadecimal s =
    (regexp hexadecimal_r >>= fun digits ->
     spaces >>
     try_return int_of_string digits "Hexadecimal value out of range" s
     <?> "hexadecimal value") s

  let octal s =
    (regexp octal_r >>= fun digits ->
     spaces >>
     try_return int_of_string digits "Octal value out of range" s
     <?> "octal value") s

  let binary s =
    (regexp binary_r >>= fun digits ->
     spaces >>
     try_return int_of_string digits "Binary value out of range" s
     <?> "binary value") s

  let integer s =
    (regexp integer_r >>= fun digits ->
     spaces >>
     try_return int_of_string digits "Integer value out of range" s
     <?> "integer value") s

  let float s =
    (regexp float_r >>= fun digits ->
     spaces >>
     try_return float_of_string digits "Not a valid float value" s
     <?> "float value") s

end
