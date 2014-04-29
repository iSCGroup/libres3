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

(* Sxreport *)
open Printf

let print_wrap out name f =
  fprintf out "--- %s: ---\n" name;
  f ();
  fprintf out "---=---\n"

let dump_channel out ch =
  try
    while true; do
      let line = input_line ch in
      fprintf out "\t%s\n" line;
    done
  with End_of_file -> ()

let dump_file out name =
  print_wrap out name (fun () ->
      let buf = String.make Config.buffer_size ' ' in
      try
        let f = open_in name in
        dump_channel out f;
        close_in f;
      with Sys_error e ->
        fprintf out "\t%s\n" e
  )

let print_section out name =
  Printf.fprintf out "\n%s" name;
    Printf.fprintf out "\n%s\n" (String.make (String.length name) '-')

let dump_command out cmd =
  print_wrap out cmd (fun () ->
    try
      let f = Unix.open_process_in cmd in
      dump_channel out f;
      match Unix.close_process_in f with
      | Unix.WEXITED 0 -> ();
      | Unix.WEXITED n ->
        fprintf out "Error executing '%s': exited with code %d\n" cmd n
      | Unix.WSIGNALED n | Unix.WSTOPPED n ->
        fprintf out "Error executing '%s': exited due to signal %d\n" cmd n
    with Unix.Unix_error (err,fn,_) ->
      fprintf out "Error executing '%s' in %s: %s\n" cmd fn (Unix.error_message err)
  )

type 'a result = OK of 'a | Error of exn

let print_str_opt fmt = function
  | Some s -> Printf.fprintf fmt "%s" s
  | None -> Printf.fprintf fmt "N/A"

let print_str_list fmt lst =
  Printf.fprintf fmt "%s"
    (String.concat ":" (List.map Filename.quote lst))

let secret_re = Netstring_str.regexp ".*secret_key.*"
let rec dump_cfg out ch =
  let line = input_line ch in
  begin match Netstring_str.string_match secret_re line 0 with
  | None ->
    Printf.fprintf out "%s\n" line
  | Some _ ->
    Printf.fprintf out "<secret_key not printed>\n"
  end;
  dump_cfg out ch

let dump_cfg_file out name dir =
  let path = Filename.concat dir name in
  try
    let ch = open_in path in
    print_wrap out path (fun () ->
      try
        dump_cfg out ch;
      with End_of_file ->
        close_in ch
    )
  with _ -> ()

let run out config result =
  print_section out "Build configuration";
  fprintf out "Source code version: %s\n" Version.version;
  fprintf out "sysconfdir: %s\n" Configure.sysconfdir;
  print_wrap out "Package versions" (fun () ->
      output_string out Version.package_info
  );
  print_wrap out "Build environment" (fun () ->
    output_string out Version.env_info
  );
    fprintf out "OCaml compiler version: %s\n" Sys.ocaml_version;

  (* TODO: bindir, sysconfdir, localstatedir *)
  print_section out "System information";
  fprintf out "OS type: %s\n" Sys.os_type;
  fprintf out "Word size: %d\n" Sys.word_size;
  fprintf out "Executable: %s\n" Sys.argv.(0);
  dump_command out "uname -mrsv";
  dump_command out "lsb_release -ds 2>/dev/null";
  dump_file out "/proc/cpuinfo";
  dump_command out "/sbin/ifconfig 2>/dev/null";
  dump_command out "free 2>/dev/null";

  print_section out "Runtime configuration";
  fprintf out "Buckets dir: %s\n" !Config.buckets_dir;
  fprintf out "Cache dir: %s\n" !Config.cache_dir;
  fprintf out "Syslog_facility: %s\n" !Config.syslog_facility;
  fprintf out "Run as: %a:%a\n"
    print_str_opt !Config.user print_str_opt !Config.group;
  fprintf out "Pid file: %s\n" !Config.pidfile;
  fprintf out "SSL certificate file: %a\n" print_str_opt !Config.ssl_certificate_file;
  fprintf out "SSL private key file: %a\n" print_str_opt !Config.ssl_privatekey_file;
  fprintf out "Base host: %s, port: %d, SSL port: %d\n"
    !Config.base_hostname !Config.base_port !Config.base_ssl_port;
  fprintf out "Access key id: %s\n" !Config.key_id;
  fprintf out "Secret access key present: %d bytes\n"
    (String.length !Config.secret_access_key);
  fprintf out "SX host: %a\n" print_str_opt !Config.sx_host;
  fprintf out "Max connected: %d\n" !Config.max_connected;
  fprintf out "Daemonize: %b\n" !Config.daemonize;
  fprintf out "Verbose: %b\n" !Config.verbose;
  fprintf out "Initialization: ";
  begin match result with
  | OK _ -> fprintf out "OK\n";
  | Error e -> fprintf out "Error: %s\n" (Printexc.to_string e)
  end;
  dump_cfg_file out (Filename.concat Configure.sysconfdir "libres3/libres3.conf");
  dump_file out (Ocsigen_config.get_config_file ());
  let dir = Paths.log_dir in
  dump_file out (Filename.concat dir "errors.log");
  dump_file out (Filename.concat dir "warnings.log");
  dump_file out (Filename.concat dir "info.log");
;;

