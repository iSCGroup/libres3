(**************************************************************************)
(*  LibreS3 server                                                        *)
(*  Copyright (C) 2012-2014 Skylable Ltd. <info-copyright@skylable.com>   *)
(*                                                                        *)
(*  This program is free software; you can redistribute it and/or modify  *)
(*  it under the terms of the GNU General Public License version 2 as     *)
(*  published by the Free Software Foundation.                            *)
(*                                                                        *)
(*  This program is distributed in the hope that it will be useful,       *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU General Public License for more details.                          *)
(*                                                                        *)
(*  You should have received a copy of the GNU General Public License     *)
(*  along with this program; if not, write to the Free Software           *)
(*  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,            *)
(*  MA 02110-1301 USA.                                                    *)
(*                                                                        *)
(*  Special exception for linking this software with OpenSSL:             *)
(*                                                                        *)
(*  In addition, as a special exception, Skylable Ltd. gives permission   *)
(*  to link the code of this program with the OpenSSL library and         *)
(*  distribute linked combinations including the two. You must obey the   *)
(*  GNU General Public License in all respects for all of the code used   *)
(*  other than OpenSSL. You may extend this exception to your version     *)
(*  of the program, but you are not obligated to do so. If you do not     *)
(*  wish to do so, delete this exception statement from your version.     *)
(**************************************************************************)

open OUnit
module Make
  (M:Sigs.Monad)
  (W:Sigs.ThreadMonad with type 'a t = 'a M.t)
  (OS:EventIO.OSMonad with type 'a t = 'a M.t):
sig
  val tests: string -> test
end = struct
  module MonadTest = MonadTest.MakeTests(M)
  module OSTest = OSMonadTest.Make(M)(OS)
  module IO = EventIO.Make(M)(OS)
  module IOTest = EventIOTest.Make(M)(IO)
  module Default = SXDefaultIO.Make(M)(OS)(W)
  let _ =
    Default.register ();
  module SXIO =  Default.IO
  module ServerTest = ServerTest.Make(SXIO)(IO)

  let tests name =
    name>:::[
      MonadTest.tests;
      OSTest.tests;
      IOTest.tests;
      ServerTest.tests
    ]
end
