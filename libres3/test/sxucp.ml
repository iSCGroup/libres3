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

open SXThread

let () =
  Default.register ();
  if (Array.length Sys.argv) <> 4 then begin
    Printf.eprintf "Usage: %s /path/to/admin.key <source URL> <destination URL>\n" Sys.argv.(0);
    exit 1;
  end;
  let adminkey = Sys.argv.(1) in
  let src = Sys.argv.(2) and dst = Sys.argv.(3) in
  Printexc.record_backtrace true;
  let f = open_in adminkey in
  Config.secret_access_key := input_line f;
  close_in f;
  try
    let size = SXIO.with_url_source (SXIO.of_url src) (fun source () ->
      source.SXIO.meta.SXIO.size) () in
    Printf.printf "Data size: %Ld\n" size;
    let t0 = Unix.gettimeofday () in
    SXIO.copy (SXIO.of_url src) ~srcpos:0L (SXIO.of_url dst) ();
    let t1 = Unix.gettimeofday () in
    let delta = t1 -. t0 in
    let mb = (Int64.to_float size) /. 1048576.0 in
    Printf.printf "Transferred %.3f MiB bytes in %.3fs: %f MiB/s\n"
      mb delta (mb /. delta);
  with e ->
    Printf.eprintf "%s: %s\n%!" Sys.argv.(0) (Printexc.to_string e);
    Printexc.print_backtrace stderr;
    Printf.eprintf "%s: %s\n%!" Sys.argv.(0) (Printexc.to_string e);;

