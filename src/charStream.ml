
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

   Module CharStream:
     Character streams.
*)


open MParser_Utils


module Make (Ch: MParser_Sig.Channel) (Rx: MParser_Sig.Regexp) = struct

  open Ch.Monad

  type t = {
    block_size:    int;        (** Size of a block in chars. *)
    block_overlap: int;        (** Overlap between blocks in chars. *)
    min_rspace:    int;        (** Minimum space for regexp matching. *)
    length:        int;        (** Length of the stream in chars. *)
    input:         Ch.t;       (** Input if created from a channel. *)
    buffer:        string;     (** The block buffer. *)
    mutable buffer_pos: int;   (** Stream position of the current block. *)
  }

  (** [io_input chn buffer pos length] reads up to [length] characters from the
      channel [chn] and stores them in the string [buffer], starting at position
      [pos].  It returns the actual number of characters read.  A value less
      than [length] is only returned if there are less than [length] characters
      available from [chn] (the [input] function in the [Pervasives] module is
      allowed to read less than [length] characters if it "finds it convenient
      to do a partial read").

      @raise Invalid_argument if [pos] and [length] do not specify a valid
      substring of [buffer]. *)
  let io_input chn buffer pos length : int Ch.Monad.t =
    if pos < 0 || pos + length > String.length buffer then
      invalid_arg "io_input: invalid substring"
    else
      let rec iter ~stop ~chars_read =
        if stop then return chars_read else
          let pos = pos + chars_read in
          let len = length - chars_read in
          Ch.read chn buffer pos len >>= fun chars ->
          if chars > 0 then
            iter ~chars_read:(chars_read + chars) ~stop:false
          else
            iter ~chars_read ~stop:true
      in
      iter ~stop:false ~chars_read:0

  (** [read_block s pos length] reads a block of [length] characters from
      current position in the input channel and writes it to the block buffer
      starting at [pos].

      The functions in this module use [read_block] exclusively to fill the
      block buffer.  If the input channel is not modified after creating the
      char stream from it, there will be at least [length] characters left in
      the channel when [read_block] is called, but this condition cannot be
      enforced by this module.  Therefore, an exception is raised if less than
      [length] characters are available.

      @raise Failure if less than [length] characters could be read.
  *)
  let read_block s pos length : unit Ch.Monad.t =
    io_input s.input s.buffer pos length >>= fun block_length ->
    if block_length <> length then
      failwith "CharStream.read_block: I/O error"
    else
      return ()

  let from_string str =
    let len = String.length str in
    {
      block_size    = len;
      block_overlap = 0;
      min_rspace    = 0;
      length        = len;
      input         = Obj.magic 0;
      buffer        = str;
      buffer_pos    = 0;
    }

  let from_channel ?(block_size = 1048576) ?block_overlap ?min_rspace input =

    let block_overlap =
      match block_overlap with
        | Some x -> x
        | None -> block_size / 16 in

    let min_rspace =
      match min_rspace with
        | Some x -> x
        | None -> block_size / 64 in

    if block_size < 1 || block_size > Sys.max_string_length then
      invalid_arg "CharStream.from_channel: invalid block size";

    if block_overlap < 1 || block_overlap > block_size / 2 then
      invalid_arg "CharStream.from_channel: invalid block overlap";

    if min_rspace < 1 || min_rspace > block_overlap then
      invalid_arg "CharStream.from_channel: invalid minimum rspace";

    Ch.length input >>= fun length ->
    Ch.length input >>= fun buffer_pos ->
    let block_size = min block_size length in
    let buffer = String.create block_size in
    let s = {
      block_size;
      block_overlap;
      min_rspace;
      length;
      input;
      buffer;
      buffer_pos;
    }
    in
    read_block s 0 block_size >> return s

  let length s =
    s.length

  let is_valid_pos s pos =
    0 <= pos && pos < s.length

  let is_visible s pos =
    s.buffer_pos <= pos && pos < s.buffer_pos + s.block_size

  (** [perform_unsafe_seek s pos] sets the position in the input stream to
      [pos], unconditionally reads the corresponding block from the input
      channel, and writes it to the block buffer.  [pos] must be a valid
      position in the input channel.  The function ensures that the block buffer
      contains at least [(max 0 (min (pos - 1) s.block_overlap))] characters
      from the input before [pos] and at least [(max 0 (min (length s - (pos +
      1)) s.block_overlap))] characters from the input after [pos].
  *)
  let perform_unsafe_seek s pos : unit Ch.Monad.t =
    let new_buffer_pos =
      min (s.length - s.block_size) (max 0 (pos - s.block_overlap))
    in
    let offset = new_buffer_pos - s.buffer_pos in

    if 0 < offset && offset < s.block_size then
      let overlap = s.block_size - offset in
      String.blit s.buffer (s.block_size - overlap) s.buffer 0 overlap;
      Ch.set_position s.input (new_buffer_pos + overlap) >>
      read_block s overlap (s.block_size - overlap) >>
      return (s.buffer_pos <- new_buffer_pos)

    else if offset < 0 && (- offset) < s.block_size then
      let overlap = s.block_size + offset in
      String.blit s.buffer 0 s.buffer (s.block_size - overlap) overlap;
      Ch.set_position s.input new_buffer_pos >>
      read_block s 0 (s.block_size - overlap) >>
      return (s.buffer_pos <- new_buffer_pos)

    else
      Ch.set_position s.input new_buffer_pos >>
      read_block s 0 s.block_size >>
      return (s.buffer_pos <- new_buffer_pos)

  (** [unsafe_seek s pos] sets the position in the input stream to [pos].  If
      this position is currently not visible, i.e., not covered by the block
      buffer, it reads the corresponding block from the input channel and writes
      it to the block buffer using [perform_unsafe_seek s pos].  [pos] must be a
      valid position in the input channel.
  *)
  let unsafe_seek s pos =
    if not (is_visible s pos) then
      perform_unsafe_seek s pos
    else
      return ()

  let seek s pos =
    if is_valid_pos s pos then
      unsafe_seek s pos
    else
      invalid_arg "CharStream.seek: invalid stream position"

  let chars_left s pos =
    if is_valid_pos s pos then
      length s - pos
    else
      0

  let read_char s pos =
    if is_valid_pos s pos then
      (unsafe_seek s pos >>
       return (Some (String.unsafe_get s.buffer (pos - s.buffer_pos))))
    else
      return None

  let read_string s pos maxlen : string Ch.Monad.t =
    if not (is_valid_pos s pos) then
      return ""
    else
      let len = min maxlen (chars_left s pos) in
      if is_visible s pos && is_visible s (pos + len - 1) then
        return (String.sub s.buffer (pos - s.buffer_pos) len)
      else if len <= s.block_overlap then
        (perform_unsafe_seek s pos >>
         return (String.sub s.buffer (pos - s.buffer_pos) len))
      else
        let result = String.create len in

        let rec iter chars_left chars_read =
          if chars_left > 0 then
            let nchars = min s.block_size chars_left in
            read_block s 0 nchars >>
            (String.blit s.buffer 0 result chars_read nchars;
             iter (chars_left - nchars) (chars_read + nchars))
          else
            return result
        in
        Ch.set_position s.input pos >>
        iter len 0

  let match_char s pos c =
    let open Ch.Monad in
    read_char s pos >>= fun c2 ->
    return (c2 = Some c)

  let match_string s pos str =
    if not (is_valid_pos s pos) then
      return (str = "")
    else
      let len = String.length str in
      if len > chars_left s pos then
        return false
      else if is_visible s pos && is_visible s (pos + len - 1) then
        return (String.match_sub s.buffer (pos - s.buffer_pos) str)
      else if len <= s.block_overlap then
        (perform_unsafe_seek s pos >>
         return (String.match_sub s.buffer (pos - s.buffer_pos) str))
      else
        let rec iter chars_left chars_read ~result =
          if chars_left > 0 then
            (let nchars = min s.block_size chars_left in
             read_block s 0 nchars >>= fun () ->
             if String.match_sub2 str chars_read s.buffer 0 nchars then
               iter (chars_left - nchars) (chars_read + nchars) ~result
             else
               return false)
          else
            return result
        in
        Ch.set_position s.input pos >>
        iter len 0 ~result:true

  let match_regexp s pos rex =
    if is_valid_pos s pos then
      (if not (is_visible s pos && is_visible s (pos + s.min_rspace)) then
         perform_unsafe_seek s pos
       else
         return ()) >>
      return (Rx.exec ~rex ~pos:(pos - s.buffer_pos) s.buffer)
    else
      return None

end
