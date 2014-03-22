
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


(* Perl-style (PCRE-alike) *)
module Perl: sig
  module Regexp: MParser_Regexp.Sig
  include module type of MParser.MakeRx (Regexp)
end

(* Emacs-style *)
module Emacs: sig
  module Regexp: MParser_Regexp.Sig
  include module type of MParser.MakeRx (Regexp)
end

(* POSIX-style
   (http://www.opengroup.org/onlinepubs/007908799/xbd/re.html,
    http://www.opengroup.org/onlinepubs/007908799/xsh/regcomp.html) *)
module POSIX: sig
  module Regexp: MParser_Regexp.Sig
  include module type of MParser.MakeRx (Regexp)
end

(* Shell-style *)
module Glob: sig
  module Regexp: MParser_Regexp.Sig
  include module type of MParser.MakeRx (Regexp)
end
