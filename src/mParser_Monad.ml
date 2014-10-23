
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

   Module MParser_Monad:
     Monadic interfaces.
*)


module type Minimal = sig
  type 'a t
  val return: 'a -> 'a t
  val bind: 'a t -> ('a -> 'b t) -> 'b t
end


module type Complete = sig
  include Minimal
  val (>>=): 'a t -> ('a -> 'b t) -> 'b t
  val (>>): 'a t -> 'b t -> 'b t
  val (<<): 'a t -> 'b t -> 'a t
  val iter_s: ('a -> unit t) -> 'a list -> unit t
end


module Extend (M: Minimal) = struct
  include M

  let (>>=) = bind

  let (>>) p q =
    p >>= fun _ -> q

  let (<<) p q =
    p >>= fun x -> q >> return x

  let rec iter_s f l =
    match l with
      | [] ->
          return ()
      | x :: l ->
          f x >>= fun () ->
          iter_s f l

end
