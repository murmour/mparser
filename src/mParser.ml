
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

   Module MParser:
     The parser combinator library.
*)

(** For an introduction to monadic parser combinators see the following paper:

    - Graham Hutton and Erik Meijer, "Monadic Parser Combinators", Technical
      report NOTTCS-TR-96-4, Department of Computer Science, University of
      Nottingham, 1996.

    The internal design of this module is based on the one used in the FParsec
    library for F# by Stephan Tolksdorf and the Parsec library for Haskell by
    Daan Leijen, which is described in the following paper:

    - Daan Leijen and Erik Meijer, "Parsec: Direct-Style Monadic Parser
      Combinators For The Real World", Technical Report UU-CS-2001-35,
      Departement of Computer Science, Universiteit Utrecht, 2001.
*)


open Printf
open MParser_Utils


(* Parser state
   -------------------------------------------------------------------------- *)

type 's state = {
  input:      CharStream.t;
  length:     int;
  index:      int;
  line:       int;
  line_begin: int;
  user:       's;
}

let init input user = {
  input      = input;
  length     = CharStream.length input;
  index      = 0;
  line       = 1;
  line_begin = 0;
  user       = user;
}

let is_valid_index s =
  0 <= s.index && s.index < s.length

let is_eof s =
  not (is_valid_index s)

let next_state s =
  if is_valid_index s then
    { s with index = s.index + 1 }
  else
    s

let advance_state s n =
  if is_valid_index s then
    { s with index = s.index + n }
  else
    s

let advance_state_nl s n =
  if is_valid_index s then
    { s with
        index = s.index + n;
        line = s.line + 1;
        line_begin = s.index + n }
  else
    s

let read_char s =
  CharStream.read_char s.input s.index

let read_index s i =
  CharStream.read_char s.input i

let next_char s =
  CharStream.read_char s.input (s.index + 1)

let prev_char s =
  CharStream.read_char s.input (s.index - 1)

let read_string s n =
  CharStream.read_string s.input s.index n

let match_char s c =
  CharStream.match_char s.input s.index c

let match_string s str =
  CharStream.match_string s.input s.index str


(* Error handling
   -------------------------------------------------------------------------- *)

type pos = int * int * int

let pos_of_state s =
  (s.index, s.line, s.index - s.line_begin + 1)

type error_message =
  | Unexpected_error of string
  | Expected_error of string
  | Message_error of string
  | Compound_error of string * error
  | Backtrack_error of error
  | Unknown_error

and error =
  | Parse_error of pos * error_message list
  | No_error

let unexpected_error s msg =
  Parse_error (pos_of_state s, [Unexpected_error msg])

let expected_error s msg =
  Parse_error (pos_of_state s, [Expected_error msg])

let message_error s msg =
  Parse_error (pos_of_state s, [Message_error msg])

let compound_error s msg e =
  Parse_error (pos_of_state s, [Compound_error (msg, e)])

let backtrack_error s e =
  Parse_error (pos_of_state s, [Backtrack_error e])

let unknown_error s =
  Parse_error (pos_of_state s, [Unknown_error])

let merge_errors e1 e2 =
  match e1, e2 with
    | No_error, _ -> e2
    | _, No_error -> e1
    | Parse_error (s, msg1), Parse_error (_, msg2) ->
        Parse_error (s, List.append msg1 msg2)


(* Error reporting
   -------------------------------------------------------------------------- *)

let error_line input pos width indent =
  let rec find_nl i stop =
    if i >= stop then i
    else match CharStream.read_char input i with
      | None | Some '\n' | Some '\r' -> i
      | _ -> find_nl (i + 1) stop
  in
  let space = width - indent in
    if space > 10 then
      let index, _, column = pos in
      let start   = index - (min (column - 1) (space / 2)) in
      let stop    = min (start + space) (CharStream.length input) in
      let length  = (find_nl start stop) - start in
      let offset  = index - start in
        if length > 0 then
            (String.make indent ' ')
          ^ (CharStream.read_string input start length) ^ "\n"
          ^ (String.make (indent + offset) ' ') ^ "^\n"
        else "\n"
    else "\n"

(** [concat_conj sep conj strings] concatenates the elements of [strings]
    using the separator [sep] and the conjunction [conj] according to the
    rules of the English language.  For example, [concat_conj "and"
    \["A"; "B"; "C"\]] results in the string ["A, B, and C"]. *)
let rec concat_conj conj = function
  |      [ ] -> ""
  | x :: [ ] -> x
  | x :: [y] -> sprintf "%s %s %s" x conj y
  | x :: xs  -> sprintf "%s, %s" x (concat_conj conj xs)

let rec error_message input pos messages width indent =
  let index, line, column = pos in

  let unexp, exp, msg, comp, back, unknowns =
    List.fold_left
      (fun ((u, e, m, c, b, k) as msgs) msg ->
         match msg with
           | Unexpected_error s when s <> "" ->
               s :: u, e, m, c, b, k
           | Expected_error s when s <> "" ->
               u, s :: e, m, c, b, k
           | Message_error s when s <> "" ->
               u, e, s :: m, c, b, k
           | Compound_error (s, Parse_error (pos1, msg1)) ->
               u, s :: e, m, (s, pos1, msg1) :: c, b, k
           | Backtrack_error (Parse_error (pos1, msg1)) ->
               u, e, m, c, (pos1, msg1) :: b, k
           | Unknown_error ->
               u, e, m, c, b, (k + 1)
           | _ -> msgs)
      ([], [], [], [], [], 0) messages in

  let ind = String.make indent ' ' in
  let buf = Buffer.create 160 in

  bprintf buf "%sError in line %d, column %d:\n%s"
    ind line column (error_line input pos width indent);

  if unexp <> [] then
    bprintf buf "%sUnexpected %s\n"
      ind (concat_conj "and" (String.unique unexp));

  if exp <> [] then
    bprintf buf "%sExpecting %s\n"
      ind (concat_conj "or" (String.unique exp));

  if msg <> [] then
    if unexp <> [] || exp <> [] then
      (bprintf buf "%sOther errors:\n" ind;
       msg |> List.iter (fun m ->
         bprintf buf "%s  %s\n" ind m))
    else
      msg |> List.iter (fun m ->
        bprintf buf "%s%s\n" ind m);

  comp |> List.iter (fun (s, p, m) ->
    bprintf buf "%s%s could not be parsed because:\n%s"
      ind s (error_message input p m width (indent + 2)));

  back |> List.iter (fun (p, m) ->
    bprintf buf "%sBacktracking occurred after:\n%s"
      ind (error_message input p m width (indent + 2)));

  Buffer.contents buf


(** Parser type *)

type ('a, 's) reply =
  | Empty_failed of error
  | Empty_ok of 'a * 's state * error
  | Consumed_failed of error
  | Consumed_ok of 'a * 's state * error

type ('a, 's) parser = 's state -> ('a, 's) reply

type ('a, 's) t = ('a, 's) parser

let make_ok consumed r s e =
  if consumed then Consumed_ok (r, s, e)
  else Empty_ok (r, s, e)

let make_failed consumed e =
  if consumed then Consumed_failed e
  else Empty_failed e

let is_consumed r =
  match r with
    | Consumed_failed _ | Consumed_ok _ -> true
    | Empty_failed _ | Empty_ok _ -> false

let is_empty r =
  match r with
    | Consumed_failed _ | Consumed_ok _ -> false
    | Empty_failed _ | Empty_ok _ -> true

let is_error r =
  match r with
    | Empty_failed _ | Consumed_failed _ -> true
    | Empty_ok _ | Consumed_ok _ -> false

let is_ok r =
  match r with
    | Empty_failed _ | Consumed_failed _ -> false
    | Empty_ok _ | Consumed_ok _ -> true

let get_error reply =
  match reply with
    | Empty_failed e | Empty_ok (_, _, e)
    | Consumed_failed e | Consumed_ok (_, _, e) -> e

let set_error reply error =
  match reply with
    | Empty_failed _ -> Empty_failed error
    | Empty_ok (r, s, _) -> Empty_ok (r, s, error)
    | Consumed_failed _ -> Consumed_failed error
    | Consumed_ok (r, s, _) -> Consumed_ok (r, s, error)

type 'a result =
  | Success of 'a
  | Failed of string * error

let parse p input user =
  match p (init input user) with
    | Empty_ok (x, _, _) | Consumed_ok (x, _, _) ->
        Success x
    | Empty_failed e | Consumed_failed e ->
        ( match e with
            | Parse_error (pos, messages) ->
                Failed (error_message input pos messages 78 0, e)
            | No_error ->
                Failed ("", e) )

let parse_string p str user =
  let input = CharStream.from_string str in
    parse p input user

let parse_channel p chn user =
  let input = CharStream.from_channel chn in
    parse p input user


(* Parser combinators
   -------------------------------------------------------------------------- *)

let return x s =
  Empty_ok (x, s, No_error)

let try_return f x msg s0 s1 =
  try Empty_ok (f x, s1, No_error)
  with _ -> Empty_failed (message_error s0 msg)

let try_return2 f x1 x2 msg s0 s1 =
  try Empty_ok (f x1 x2, s1, No_error)
  with _ -> Empty_failed (message_error s0 msg)

let try_return3 f x1 x2 x3 msg s0 s1 =
  try Empty_ok (f x1 x2 x3, s1, No_error)
  with _ -> Empty_failed (message_error s0 msg)

let fail msg s =
  Consumed_failed (message_error s msg)

let message msg s =
  Empty_failed (message_error s msg)

let zero s =
  Empty_failed (unknown_error s)

let bind p f s =
  match p s with
    | Empty_failed e1 -> Empty_failed e1
    | Consumed_failed e1 -> Consumed_failed e1
    | Empty_ok (r1, s1, e1) ->
        ( match f r1 s1 with
            | Empty_failed e2 -> Empty_failed (merge_errors e2 e1)
            | Empty_ok (r2, s2, e2) -> Empty_ok (r2, s2, merge_errors e2 e1)
            | consumed -> consumed )
    | Consumed_ok (r1, s1, e1) ->
        ( match f r1 s1 with
            | Empty_failed e2 -> Consumed_failed (merge_errors e2 e1)
            | Empty_ok (r2, s2, e2) -> Consumed_ok (r2, s2, merge_errors e2 e1)
            | consumed -> consumed )

let (>>=) = bind

let (>>) p q =
  p >>= fun _ -> q

let (<<) p q =
  p >>= fun x -> q >> return x

let (>>$) p x =
  p >> return x

let (>>?) p q s =
  match p s with
    | Empty_failed e1 -> Empty_failed e1
    | Consumed_failed e1 -> Consumed_failed e1
    | Empty_ok (_, s1, e1) ->
        ( match q s1 with
            | Empty_failed e2 -> Empty_failed (merge_errors e2 e1)
            | Empty_ok (r2, s2, e2) -> Empty_ok (r2, s2, merge_errors e2 e1)
            | consumed -> consumed )
    | Consumed_ok (_, s1, e1) ->
        ( match q s1 with
            | Empty_failed e2 ->
                Empty_failed (backtrack_error s (merge_errors e2 e1))
            | Empty_ok (r2, s2, e2) -> Consumed_ok (r2, s2, merge_errors e2 e1)
            | consumed -> consumed )

let (|>>) p f =
  p >>= fun x -> return (f x)

let pipe2 p1 p2 f =
  p1 >>= fun x1 ->
  p2 >>= fun x2 ->
  return (f x1 x2)

let pipe3 p1 p2 p3 f =
  p1 >>= fun x1 ->
  p2 >>= fun x2 ->
  p3 >>= fun x3 ->
  return (f x1 x2 x3)

let pipe4 p1 p2 p3 p4 f =
  p1 >>= fun x1 ->
  p2 >>= fun x2 ->
  p3 >>= fun x3 ->
  p4 >>= fun x4 ->
  return (f x1 x2 x3 x4)

let (<|>) p1 p2 s =
  match p1 s with
    | Empty_failed e1 ->
        ( match p2 s with
            | Empty_failed e2 -> Empty_failed (merge_errors e2 e1)
            | Empty_ok (r2, s2, e2) -> Empty_ok (r2, s2, (merge_errors e2 e1))
            | consumed -> consumed )
    | other -> other

let (<|>$) p x =
  p <|> return x

let choice ps =
  List.fold_left (<|>) zero ps

let attempt p s =
  match p s with
    | Consumed_failed e -> Empty_failed (backtrack_error s e)
    | other -> other

let (<?>) p label s =
  let reply = p s in
    if is_empty reply then
      set_error reply (expected_error s label)
    else
      reply

let (<??>) p label s =
  let reply = p s in
    if is_empty reply then
      if is_error reply then
        match get_error reply with
          | Parse_error (pos, [Backtrack_error error]) ->
              set_error reply (compound_error s label error)
          | _ ->
              set_error reply (expected_error s label)
      else
        set_error reply (expected_error s label)
    else
      if is_error reply then
        set_error reply (compound_error s label (get_error reply))
      else
        reply

let look_ahead p s =
  match p s with
    | Empty_ok (r, _, _)
    | Consumed_ok (r, _, _) -> Empty_ok (r, s, No_error)
    | Empty_failed e -> Empty_failed e
    | Consumed_failed e -> Empty_failed (backtrack_error s e)

let followed_by p msg s =
  match p s with
    | Empty_ok _
    | Consumed_ok _ -> Empty_ok ((), s, No_error)
    | Empty_failed e
    | Consumed_failed e -> Empty_failed (expected_error s msg)

let not_followed_by p msg s =
  match p s with
    | Empty_ok _
    | Consumed_ok _ -> Empty_failed (unexpected_error s msg)
    | Empty_failed e
    | Consumed_failed e -> Empty_ok ((), s, No_error)

let opt x p =
  p <|>$ x

let option p =
  (p >>= fun r -> return (Some r)) <|> return None

let optional p =
  p >>$ () <|>$ ()

let try_skip p =
  p >>$ true <|>$ false

let pair p q =
  p >>= fun x ->
  q >>= fun y ->
  return (x, y)

let many_fold_apply f a g p =
  let rec loop consumed a s e =
    match p s with
      | Consumed_ok (r, s1, e1) -> loop true (f a r) s1 e1
      | Consumed_failed e1 -> Consumed_failed e1
      | Empty_failed e1 -> make_ok consumed (g a) s (merge_errors e1 e)
      | Empty_ok _ ->
          failwith "FParser.many_fold_apply: parser accepts the empty string"
  in
    fun s -> loop false a s No_error

let many1_fold_apply f a g p s =
  match many_fold_apply f a g p s with
    | Empty_ok (_, _, e) -> Empty_failed e
    | other -> other

let many p =
  many_fold_apply (fun xs x -> x :: xs) [] List.rev p

let many1 p =
  many1_fold_apply (fun xs x -> x :: xs) [] List.rev p

let many_rev p =
  many_fold_apply (fun xs x -> x :: xs) [] (fun x -> x) p

let many1_rev p =
  many1_fold_apply (fun xs x -> x :: xs) [] (fun x -> x) p

let skip_many p =
  many_fold_apply (fun _ _ -> ()) () (fun _ -> ()) p

let skip_many1 p =
  many1_fold_apply (fun _ _ -> ()) () (fun _ -> ()) p

let many_fold_left f a =
  many_fold_apply f a (fun x -> x)

let many1_fold_left f a =
  many1_fold_apply f a (fun x -> x)

let many_rev_fold_left f a =
  many_fold_apply (fun xs x -> x :: xs) [] (List.fold_left f a)

let many1_rev_fold_left f a =
  many1_fold_apply (fun xs x -> x :: xs) [] (List.fold_left f a)

let chain_left1 p op =
  p >>= fun x ->
  many_fold_left (fun x (f, y) -> f x y) x (pair op p)

let chain_left p op x =
  chain_left1 p op <|>$ x

let chain_right1 p op =
  let rec make_op a f y l =
    match l with
      | (g, x) :: r -> make_op a g (f x y) r
      | [] -> f a y
  in
    pipe2 p (many_rev (pair op p))
      (fun x l -> match l with [] -> x | (f, y) :: r -> make_op x f y r)

let chain_right p op x =
  chain_right1 p op <|>$ x

let count n p =
  let rec loop consumed n l s e =
    if n > 0 then
      match p s with
        | Empty_ok (r, s1, e1) ->
            loop consumed (n - 1) (r :: l) s1 (merge_errors e1 e)
        | Consumed_ok (r, s1, e1) ->
            loop true (n - 1) (r :: l) s1 e1
        | Empty_failed e1 -> make_failed consumed (merge_errors e1 e)
        | Consumed_failed e1 -> Consumed_failed e1
    else
      if consumed then Consumed_ok (List.rev l, s, e)
      else Empty_ok (List.rev l, s, e)
  in
    fun s -> loop false n [] s No_error

let skip_count n p =
  let rec loop consumed n s e =
    if n > 0 then
      match p s with
        | Empty_ok (_, s1, e1) ->
            loop consumed (n - 1) s1 (merge_errors e1 e)
        | Consumed_ok (_, s1, e1) ->
            loop true (n - 1) s1 e1
        | Empty_failed e1 -> make_failed consumed (merge_errors e1 e)
        | Consumed_failed e1 -> Consumed_failed e1
    else
      make_ok consumed () s e
  in
    fun s -> loop false n s No_error

let between left right p =
  left >> p << right

let sep_by1 p sep =
  p               >>= fun x ->
  many (sep >> p) >>= fun xs ->
  return (x :: xs)

let sep_by p sep =
  sep_by1 p sep <|>$ []

let sep_end_by1 p sep =
  p                >>= fun x ->
  many (sep >>? p) >>= fun xs ->
  optional sep     >>
  return (x :: xs)

let sep_end_by p sep =
  sep_end_by1 p sep <|>$ []

let end_by p sep =
  many (p << sep)

let end_by1 p sep =
  many1 (p << sep)

let many_until p q =
  many (not_followed_by q "" >> p) << q

let skip_many_until p q =
  skip_many (not_followed_by q "" >> p) << q


(* Accessing state
   -------------------------------------------------------------------------- *)

let get_user_state s =
  Empty_ok (s.user, s, No_error)

let set_user_state st s =
  Empty_ok ((), { s with user = st }, No_error)

let update_user_state f s =
  Empty_ok ((), { s with user = f (s.user) }, No_error)

let get_input s =
  Empty_ok (s.input, s, No_error)

let get_index s =
  Empty_ok (s.index, s, No_error)

let get_pos s =
  Empty_ok (pos_of_state s, s, No_error)

let register_nl lines chars_after_nl s =
  let s1 = {
    s with
      line = s.line + lines;
      line_begin = s.index - chars_after_nl; }
  in
    Empty_ok ((), s1, No_error)

let set_pos (index, line, column) s =
  let s' =
    { s with
        line;
        line_begin = s.index - (column - 1);
    }
  in
  Empty_ok ((), s', No_error)


(* Character parsers
   -------------------------------------------------------------------------- *)

let skip n s =
  if n >= 0 then
    let s1 = advance_state s n in
      if s1.index <> s.index then Consumed_ok ((), s1, No_error)
      else Empty_ok ((), s, No_error)
  else
    invalid_arg "FParser.skip: negative offset"

let eof s =
  match read_char s with
    | Some c -> Empty_failed (expected_error s "end of input")
    | None -> Empty_ok ((), s, No_error)

let char c s =
  if match_char s c then Consumed_ok (c, next_state s, No_error)
  else Empty_failed (expected_error s ("'" ^ (String.make 1 c) ^ "'"))

let skip_char c s =
  if match_char s c then Consumed_ok ((), next_state s, No_error)
  else Empty_failed (expected_error s ("'" ^ (String.make 1 c) ^ "'"))

let any_char s =
  match read_char s with
    | Some c -> Consumed_ok (c, next_state s, No_error)
    | None -> Empty_failed (expected_error s "any character")

let skip_any_char s =
  match read_char s with
    | Some c -> Consumed_ok ((), next_state s, No_error)
    | None -> Empty_failed (expected_error s "any character")

let any_char_or_nl s =
  match read_char s with
    | Some c ->
        if c <> '\n' && c <> '\r' then
          Consumed_ok (c, next_state s, No_error)
        else
          let n = (if c = '\r' && next_char s = Some '\n' then 2 else 1)
          in Consumed_ok ('\n', advance_state_nl s n, No_error)
    | None ->
        Empty_failed (expected_error s "any character")

let skip_any_char_or_nl s =
  match read_char s with
    | Some c ->
        if c <> '\n' && c <> '\r' then
          Consumed_ok ((), next_state s, No_error)
        else
          let n = if c = '\r' && next_char s = Some '\n' then 2 else 1
          in Consumed_ok ((), advance_state_nl s n, No_error)
    | None ->
        Empty_failed (expected_error s "any character")

let peek_char s =
  match next_char s with
    | Some c -> Empty_ok (c, s, No_error)
    | None -> Empty_failed (unexpected_error s "end of input")

let string str s =
  if match_string s str then
    Consumed_ok (str, advance_state s (String.length str), No_error)
  else
    Empty_failed (expected_error s ("\"" ^ str ^ "\""))

let skip_string str s =
  if match_string s str then
    Consumed_ok ((), advance_state s (String.length str), No_error)
  else
    Empty_failed (expected_error s ("\"" ^ str ^ "\""))

let any_string n s =
  if n = 0 then
    Empty_ok ("", s, No_error)
  else
    let r = read_string s n in
      if String.length r = n then
        Consumed_ok (r, advance_state s n, No_error)
      else
        Empty_failed
          (expected_error s
             ("any sequence of " ^ (string_of_int n) ^ " characters"))

let many_chars p s =
  many_fold_apply
    (fun b c -> Buffer.add_char b c; b) (Buffer.create 16)
    (Buffer.contents) p s

let many1_chars p s =
  many1_fold_apply
    (fun b c -> Buffer.add_char b c; b) (Buffer.create 16)
    (Buffer.contents) p s

let skip_many_chars =
  skip_many

let skip_many1_chars =
  skip_many1

let many_chars_until p q =
  many_chars (not_followed_by q "" >> p) << q

let skip_many_chars_until p q =
  skip_many_chars (not_followed_by q "" >> p) << q

let satisfy p s =
  match read_char s with
    | Some c ->
        if p c then Consumed_ok (c, next_state s, No_error)
        else Empty_failed (unknown_error s)
    | None ->
        Empty_failed (unexpected_error s "end of input")

let satisfy_l p label s =
  match read_char s with
    | Some c when p c -> Consumed_ok (c, next_state s, No_error)
    | _ -> Empty_failed (expected_error s label)

let skip_satisfy p =
  satisfy p |>> ignore

let skip_satisfy_l p label=
  satisfy_l p label |>> ignore

let nsatisfy n p s =
  if n = 0 then
    Empty_ok ("", s, No_error)
  else
    let r = read_string s n in
      if String.length r = n && String.for_all p r then
        Consumed_ok (r, advance_state s n, No_error)
      else
        Empty_failed (unknown_error s)

let many_satisfy_loop p =
  let rec loop i s =
    match read_index s i with
      | Some c when p c -> loop (i + 1) s
      | _ -> i - s.index
  in
    fun s -> loop s.index s

let many_satisfy p s =
  let n = many_satisfy_loop p s in
    if n > 0 then Consumed_ok (read_string s n, advance_state s n, No_error)
    else Empty_ok ("", s, No_error)

let many1_satisfy p s =
  match many_satisfy p s with
    | Consumed_ok _ as result -> result
    | _ -> Empty_failed (unknown_error s)

let skip_many_satisfy p s =
  let n = many_satisfy_loop p s in
    if n > 0 then Consumed_ok ((), advance_state s n, No_error)
    else Empty_ok ((), s, No_error)

let skip_many1_satisfy p s =
  match skip_many_satisfy p s with
    | Consumed_ok _ as result -> result
    | _ -> Empty_failed (unknown_error s)

let next_char_satisfies p s =
  match next_char s with
    | Some c when p c -> Empty_ok ((), s, No_error)
    | _ -> Empty_failed (unknown_error s)

let prev_char_satisfies p s =
  match prev_char s with
    | Some c when p c -> Empty_ok ((), s, No_error)
    | _ -> Empty_failed (unknown_error s)

let any_of str =
  satisfy (String.contains str)

let none_of str =
  satisfy (fun x -> not (String.contains str x))

let uppercase s =
  satisfy_l Char.is_uppercase "uppercase letter" s

let lowercase s =
  satisfy_l Char.is_lowercase "lowercase letter" s

let letter s =
  satisfy_l Char.is_letter "letter" s

let digit s =
  satisfy_l Char.is_digit "digit" s

let hex_digit s =
  satisfy_l Char.is_hex_digit "hex digit" s

let oct_digit s =
  satisfy_l Char.is_oct_digit "octal digit" s

let alphanum s =
  satisfy_l Char.is_alphanum "letter or digit" s

let tab s =
  satisfy_l (fun c -> c = '\t') "tab" s

let blank s =
  satisfy_l Char.is_blank "space or tab" s

let newline s =
  match read_char s with
    | Some c when c = '\n' || c = '\r' ->
        let k = if c = '\r' && next_char s = Some '\n' then 2 else 1
        in Consumed_ok ('\n', advance_state_nl s k, No_error)
    | _ -> Empty_failed (expected_error s "newline")

let space s =
  match read_char s with
    | Some c when c = ' ' || c = '\t' ->
        Consumed_ok (c, next_state s, No_error)
    | Some c when c = '\n' || c = '\r' ->
        let k = if c = '\r' && next_char s = Some '\n' then 2 else 1
        in Consumed_ok ('\n', advance_state_nl s k, No_error)
    | _ -> Empty_failed (expected_error s "whitespace")

let spaces s =
  let lines = ref 0 in
  let line_begin = ref 0 in
  let rec loop i =
    match read_index s i with
      | Some c when c = ' ' || c = '\t' -> loop (i + 1)
      | Some c when c = '\n' || c = '\r' ->
          let k =
            if c = '\r' && read_index s (i + 1) = Some '\n' then 2 else 1
          in
            lines := !lines + 1;
            line_begin := i + k;
            loop (i + k)
      | _ -> i - s.index
  in let n = loop s.index in
    if !lines > 0 then
      let s1 =
        { s with
            index      = s.index + n;
            line       = s.line + !lines;
            line_begin = !line_begin; }
      in Consumed_ok ((), s1, No_error)
    else if n > 0 then
      Consumed_ok ((), advance_state s n, No_error)
    else
      Empty_ok ((), s, No_error)

let spaces1 s =
  match spaces s with
    | Consumed_ok _ as result -> result
    | _ -> Empty_failed (expected_error s "whitespace")


(* Expressions
   -------------------------------------------------------------------------- *)

type assoc =
  | Assoc_none
  | Assoc_left
  | Assoc_right

type ('a, 's) operator =
  | Infix of (('a -> 'a -> 'a, 's) parser * assoc)
  | Prefix of ('a -> 'a, 's) parser
  | Postfix of ('a -> 'a, 's) parser

let expression operators term =

  let make_parser term ops =

    let split_op (rassoc, lassoc, nassoc, prefix, postfix) op =
      match op with
          Infix (p, assoc) ->
            ( match assoc with
                  Assoc_none  -> (rassoc, lassoc, p::nassoc, prefix, postfix)
                | Assoc_left  -> (rassoc, p::lassoc, nassoc, prefix, postfix)
                | Assoc_right -> (p::rassoc, lassoc, nassoc, prefix, postfix) )
        | Prefix p ->
            (rassoc, lassoc, nassoc, p::prefix, postfix)
        | Postfix p ->
            (rassoc, lassoc, nassoc, prefix, p::postfix) in

    let (rassoc, lassoc, nassoc, prefix, postfix) =
      List.fold_left split_op ([], [], [], [], []) ops in

    let rassoc_op  = choice rassoc in
    let lassoc_op  = choice lassoc in
    let nassoc_op  = choice nassoc in
    let prefix_op  = choice prefix in
    let postfix_op = choice postfix in

    let prefix_p  = opt (fun x -> x) prefix_op in
    let postfix_p = opt (fun x -> x) postfix_op in

    let term_p =
      prefix_p  >>= fun pre ->
      term      >>= fun x ->
      postfix_p >>= fun post ->
      return (post (pre x)) in

    let rec rassoc_p x =
      rassoc_op                           >>= fun f ->
      (term_p >>= (fun z -> rassoc_p' z)) >>= fun y ->
      return (f x y)

    and rassoc_p' x =
      opt x (rassoc_p x) in

    let rec lassoc_p x =
      lassoc_op >>= fun f ->
      term_p    >>= fun y ->
      lassoc_p' (f x y)

    and lassoc_p' x =
      opt x (lassoc_p x) in

    let nassoc_p x =
      nassoc_op >>= fun f ->
      term_p    >>= fun y ->
      return (f x y)

    in
      term_p >>= fun x ->
     (rassoc_p x <|> lassoc_p x <|> nassoc_p x <|>$ x)

  in
    List.fold_left make_parser term operators


(* Regexp-related features
   -------------------------------------------------------------------------- *)

module MakeRx = functor (Rx: MParser_Regexp.Sig) -> struct
  module CharStreamRx = CharStream.MakeRx (Rx)

  let match_regexp s rex =
    CharStreamRx.match_regexp s.input s.index rex

  let make_regexp pat =
    Rx.make pat

  let regexp r s =
    match match_regexp s r with
      | Some substrings ->
          (match Rx.get_substring substrings 0 with
            | Some result ->
                let n = String.length result in
                if n > 0 then
                  Consumed_ok (result, advance_state s n, No_error)
                else
                  Empty_ok (result, s, No_error)
            | None ->
                zero s)
      | None ->
          zero s

  let regexp_substrings r s =
    match match_regexp s r with
      | Some substrings ->
          let result = Rx.get_all_substrings substrings in
          let n = String.length (Array.get result 0) in
            if n > 0 then
              Consumed_ok (result, advance_state s n, No_error)
            else
              Empty_ok (result, s, No_error)
      | None ->
          zero s


  (* Token parsers
     ------------------------------------------------------------------------ *)

  module Tokens = struct

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

end
