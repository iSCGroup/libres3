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

module StringMap = Map.Make(String)
module StringSet = Set.Make(String)

let open_file name =
  try
    Some (open_in name)
  with Sys_error msg ->
    Printf.printf "Cannot open file %s\n" msg;
    None;;

let with_file ch f ~close =
  let result = try f ch with e -> close ch; raise e in
  close ch;
  result;;

let readlocked f ch =
  Unix.lockf (Unix.descr_of_in_channel ch) Unix.F_RLOCK 0;
  f ch;;

let rec parse_full parse accum ch =
  try
    parse_full parse (parse ch accum) ch
  with End_of_file ->
    accum;;

type 'a configuration = 'a Map.Make(String).t
let parse_configuration path parse default =
  match open_file path with
  | None -> default
  | Some ch ->
      Printf.printf "Loading configuration from %s\n" path;
      try
        with_file ch (readlocked (parse_full parse default)) ~close:close_in
      with
      | Scanf.Scan_failure msg | Failure msg ->
          Printf.eprintf "Failed to parse configuration: %s\n" msg;
          raise Exit;
      | e ->
        Printf.eprintf "Failed to parse configuration: %s\n"
          (Printexc.to_string e);
        raise Exit;;

let config_file = Filename.concat Configure.sysconfdir "libres3/libres3.conf"
let process_configuration name ~parse ~print =
  parse_configuration config_file parse StringMap.empty

let log_dir = Filename.concat Configure.localstatedir "log/libres3"
let var_lib_dir = Filename.concat Configure.localstatedir "lib/libres3"
let generated_config_file = Filename.concat var_lib_dir "libres3_ocsigen.xml"
