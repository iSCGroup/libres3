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
let fmt_time t =
  Netdate.format ~fmt:"%Y-%m-%d %H:%M:%S UTC" (Netdate.create t);;
let print_entry () entry () =
  Printf.printf "%s %20Ld %s\n"
    (fmt_time entry.SXIO.mtime)
    entry.SXIO.size
    entry.SXIO.name;;

let recurse dir =
  Printf.printf "DIR: %s\n" dir;
  true;;

let () =
  Default.register ();
  Printexc.record_backtrace true;
  if (Array.length Sys.argv) <> 3 then begin
    Printf.eprintf "Usage: %s /path/to/admin.key <URL>\n" Sys.argv.(0);
    exit 1;
  end;
  let adminkey = Sys.argv.(1) in
  let url = Sys.argv.(2) in
  let f = open_in adminkey in
  Config.secret_access_key := input_line f;
  close_in f;
  try
    SXIO.fold_list ~base:(SXIO.of_url "/") (SXIO.of_url url) ~entry:print_entry ~recurse () ()
  with e ->
    Printexc.print_backtrace stderr;
    Printf.eprintf "Error: %s\n" (Printexc.to_string e);;
