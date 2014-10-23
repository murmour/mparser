
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

   Module MParser_Channel_Lwt:
     A channel abstraction built on top of Lwt_io.(input channel).
*)


open Lwt


module Monad = MParser_Monad.Extend (struct
  type 'a t = 'a Lwt.t
  let return = Lwt.return
  let bind = Lwt.bind
end)


type t = Lwt_io.input Lwt_io.channel


let length ch =
  Lwt_io.length ch >>= fun len ->
  Lwt.return (Int64.to_int len)

let position ch =
  let pos = Lwt_io.position ch in
  Lwt.return (Int64.to_int pos)

let set_position ch pos =
  Lwt_io.set_position ch (Int64.of_int pos)

let read ch buf pos len =
  Lwt_io.read_into ch buf pos len
