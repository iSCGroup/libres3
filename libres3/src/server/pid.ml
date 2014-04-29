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

let write_pid name =
  let pid = Unix.getpid () in
  try
    let pidfile = open_out name in
    Printf.fprintf pidfile "%d\n" pid;
    close_out pidfile
  with Sys_error e ->
      Printf.eprintf "Failed to create PIDfile %s: %s\n%!" name e;
      raise Exit;;

let kill_pid name =
  begin try
    let f = open_in name in
    let pid = int_of_string (input_line f) in
    Printf.printf "Sending TERM to PID %d ... %!" pid;
    begin try
      Unix.kill (-pid) 15;
      Printf.printf "\n%!";
    with Unix.Unix_error(e,_,_) ->
      Printf.eprintf "Kill failed: %s!\n%!" (Unix.error_message e);
    end;
    close_in f
  with
  | Sys_error e | Failure e ->
    Printf.printf "PIDfile %s cannot be opened: %s\n%!" name e
  | End_of_file -> ();
  end;
  try Sys.remove name with Sys_error _ -> ();;

