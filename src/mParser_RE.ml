
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

   Module MParser_RE:
     RE-based regular expression parsers.
*)


module Perl = struct

  module Regexp: MParser_Regexp.Sig = struct

    type t = Re.re
    type substrings = Re.substrings


    let default_flags =
      [ `Anchored ]

    let make pattern =
      Re_perl.(compile (re ~opts:default_flags pattern))

    let get_substring s idx =
      try
        Some (Re.get s idx)
      with Not_found ->
        None

    let get_all_substrings s =
      Re.get_all s

    let exec ~rex ~pos s =
      try
        Some (Re.exec ~pos rex s)
      with Not_found ->
        None

  end

  include MParser.MakeRx (Regexp)

end


module Emacs = struct

  module Regexp: MParser_Regexp.Sig = struct

    type t = Re.re
    type substrings = Re.substrings


    let make pattern =
      let anchored = Re.seq [ Re.start; Re_emacs.re pattern ] in
      Re_emacs.(compile anchored)

    let get_substring s idx =
      try
        Some (Re.get s idx)
      with Not_found ->
        None

    let get_all_substrings s =
      Re.get_all s

    let exec ~rex ~pos s =
      try
        Some (Re.exec ~pos rex s)
      with Not_found ->
        None

  end

  include MParser.MakeRx (Regexp)

end


module POSIX = struct

  module Regexp: MParser_Regexp.Sig = struct

    type t = Re.re
    type substrings = Re.substrings


    let make pattern =
      let anchored = Re.seq [ Re.start; Re_posix.re pattern ] in
      Re_posix.compile anchored

    let get_substring s idx =
      try
        Some (Re.get s idx)
      with Not_found ->
        None

    let get_all_substrings s =
      Re.get_all s

    let exec ~rex ~pos s =
      try
        Some (Re.exec ~pos rex s)
      with Not_found ->
        None

  end

  include MParser.MakeRx (Regexp)

end


module Glob = struct

  module Regexp: MParser_Regexp.Sig = struct

    type t = Re.re
    type substrings = Re.substrings


    let make pattern =
      let anchored = Re.seq [ Re.start; Re_glob.glob pattern ] in
      Re.compile anchored

    let get_substring s idx =
      try
        Some (Re.get s idx)
      with Not_found ->
        None

    let get_all_substrings s =
      Re.get_all s

    let exec ~rex ~pos s =
      try
        Some (Re.exec ~pos rex s)
      with Not_found ->
        None

  end

  include MParser.MakeRx (Regexp)

end
