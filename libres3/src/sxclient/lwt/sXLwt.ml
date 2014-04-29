(**************************************************************************)
(*  SX client                                                             *)
(*  Copyright (C) 2012-2014 Skylable Ltd. <info-copyright@skylable.com>   *)
(*                                                                        *)
(*  This library is free software; you can redistribute it and/or         *)
(*  modify it under the terms of the GNU Lesser General Public            *)
(*  License as published by the Free Software Foundation; either          *)
(*  version 2.1 of the License, or (at your option) any later version.    *)
(*                                                                        *)
(*  This library is distributed in the hope that it will be useful,       *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *)
(*  Lesser General Public License for more details.                       *)
(*                                                                        *)
(*  You should have received a copy of the GNU Lesser General Public      *)
(*  License along with this library; if not, write to the Free Software   *)
(*  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,            *)
(*  MA  02110-1301  USA                                                   *)
(*                                                                        *)
(*  As a special exception to the GNU Library General Public License,     *)
(*  you may link, statically or dynamically, a "work that uses the        *)
(*  Library" with a publicly distributed version of the Library to        *)
(*  produce an executable file containing portions of the Library, and    *)
(*  distribute that executable file under terms of your choice, without   *)
(*  any of the additional requirements listed in clause 6 of the GNU      *)
(*  Library General Public License. By "a publicly distributed version    *)
(*  of the Library", we mean either the unmodified Library, or a          *)
(*  modified version of the Library that is distributed under the         *)
(*  conditions defined in clause 3 of the GNU Library General Public      *)
(*  License. This exception does not however invalidate any other         *)
(*  reasons why the executable file might be covered by the GNU Library   *)
(*  General Public License.                                               *)
(**************************************************************************)

module Monad = struct
  include Lwt
  let try_catch f g v =
    Lwt.catch (fun () ->
      try
        f v
      with e ->
        fail e) g;;
  let run = Lwt_main.run
end

module Thread = struct
  include Lwt
  type 'a wakener = 'a result -> unit
  let bad_result = Lwt.make_error (Failure "http dispatch")

  let result f =
    try
      Lwt.make_value (f ())
    with e ->
      Lwt.make_error e
    ;;

  let wait () =
    let waiter, wakener = Lwt.wait () in
    let result = ref bad_result in
    let notif_id = Lwt_unix.make_notification ~once:true (fun () ->
      Lwt.wakeup_result wakener !result) in
    waiter, (fun r ->
      result := r;
      Lwt_unix.send_notification notif_id
    )
  ;;
end

module OS = struct
  type 'a t = 'a Lwt.t
  include Lwt_unix
end
module IO = EventIO.Make(Monad)(OS)
module Default = SXDefaultIO.Make(Monad)(OS)(Thread)
module SXIO = Default.IO
