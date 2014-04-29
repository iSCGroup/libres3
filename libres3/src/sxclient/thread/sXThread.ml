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
  type 'a t = unit -> 'a
  let return value = fun () -> value
  let (>>=) v f = f (v ())
  let fail e = raise e
  let try_catch f g v = fun () ->
    try
      (f v) ()
    with e ->
      (g e) ();;
  let run x = x ()

  type 'a result = Result of 'a | Error of exn
  type 'a wakener = 'a result -> unit

  let masterlock = Mutex.create ()

  let result f =
    try
      Result (f ())
    with e ->
      Error e
  ;;

  let wait_result (cond,res) () =
    Mutex.lock masterlock;
    while !res = None do
      Condition.wait cond masterlock;
    done;
    Mutex.unlock masterlock;
    match !res with
    | None -> assert false
    | Some (Result r) ->
        r
    | Some (Error e) ->
        raise e
  ;;

  let send_result (cond,res) r =
    Mutex.lock masterlock;
    res := Some r;
    Condition.signal cond;
    Mutex.unlock masterlock;
  ;;

  let wait () =
    let cond = Condition.create () in
    let res = ref None in
    wait_result (cond, res), send_result (cond,res)
  ;;
end

module OS = struct
  type 'a t = 'a Monad.t
  let rec restart_eintr f v =
    try
      Monad.return (f v)
    with Unix.Unix_error(Unix.EINTR, _, _) ->
      restart_eintr f v
  ;;
  let mkdir name = restart_eintr (Unix.mkdir name)
  let rmdir = restart_eintr Unix.rmdir
  type dir_handle = Unix.dir_handle
  let opendir = restart_eintr Unix.opendir
  let readdir = restart_eintr Unix.readdir
  let closedir = restart_eintr Unix.closedir
  type file_descr = Unix.file_descr
  let openfile name flags = restart_eintr (Unix.openfile name flags)
  let close = restart_eintr Unix.close
  let read fd buf pos = restart_eintr (Unix.read fd buf pos)
  let write fd buf pos = restart_eintr (Unix.write fd buf pos)
  let rename src = restart_eintr (Unix.rename src)
  let unlink = restart_eintr Unix.unlink
  let access name perms = restart_eintr (Unix.access name) perms

  let sleep t =
    let finish = Unix.gettimeofday () +. t in
    let rec loop t =
      begin try ignore (Unix.select [] [] [] t) with _ -> () end;
      let now = Unix.gettimeofday () in
      if now < finish then
        loop (finish -. now)
      else
        Monad.return () in
    loop t
  ;;

  module LargeFile = struct
    let lseek fd pos = restart_eintr (Unix.LargeFile.lseek fd pos)
    let lstat = restart_eintr Unix.LargeFile.lstat
    let fstat = restart_eintr Unix.LargeFile.fstat
  end
end

module IO = EventIO.Make(Monad)(OS)
module Default = SXDefaultIO.Make(Monad)(OS)(Monad)
module SXIO = Default.IO
