
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


(* todo *)
type t = in_channel (* Lwt_io.input Lwt_io.channel *)


(* todo *)
let length ch =
  Pervasives.in_channel_length ch

(* todo *)
let position ch =
  Pervasives.pos_in ch

(* todo *)
let set_position ch pos =
  Pervasives.seek_in ch pos

(* todo *)
let read ch buf pos len =
  Pervasives.input ch buf pos len
