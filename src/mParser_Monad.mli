
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


(* Minimal monadic interface *)
module type Minimal = sig
  type 'a t

  val return: 'a -> 'a t
  (** [return x] always succeeds with the result [x]
      without consuming any input.  *)

  val bind: 'a t -> ('a -> 'b t) -> 'b t
  (** [p >>= f] first applies the monad [p], then applies [f] to the
      resulting value, and finally applies the resulting monad. *)

end


(* Extended monadic interface, which can be built on top of the minimal one *)
module type Complete = sig
  include Minimal

  val (>>=): 'a t -> ('a -> 'b t) -> 'b t
  (** [p >>= f] is equivalent to [bind p f] *)

  val (>>): 'a t -> 'b t -> 'b t
  (** [p >> q] is equivalent to [p >>= (fun _ -> q)]. *)

  val (<<): 'a t -> 'b t -> 'a t
  (** [p << q] is equivalent to [p >>= (fun x -> q >> return x)]. *)

  val iter_s: ('a -> unit t) -> 'a list -> unit t
  (** [iter_s f list] applies the function [f] sequentially to each element
      of the list, returning a [unit] monad. Useful for side effects. *)

end


module Extend: functor (M: Minimal) -> Complete with type 'a t = 'a M.t
