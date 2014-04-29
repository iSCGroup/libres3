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

let print_version () =
  Printf.printf "libres3 setup version %s\n%!" Version.version;
  exit 0

let sxsetup_conf = ref ""
let open_errmsg = ref false
let s3_host = ref ""
let spec = Arg.align [
  "--s3-host", Arg.Set_string s3_host,
    " Base hostname to use (equivalent to; s3.amazonaws.com, host_base in .s3cfg)";
  "--sxsetup-conf", Arg.Set_string sxsetup_conf, " Path to sxsetup.conf";
  "--version", Arg.Unit print_version, " Print version";
  "-V", Arg.Unit print_version, " Print version";
]

let anon_fail flag =
  raise (Arg.Bad ("invalid option: " ^ flag))

let read_value msg =
  Printf.printf "\n%s: %!" msg;
  let reply = input_line stdin in
  reply

let ask_arg (_, spec, doc) =
  match spec with
  | Arg.Set_string str ->
      if !str = "" then str := read_value doc
  | _ -> ()

(* cmdline parsing *)
let () =
  let usage = Printf.sprintf "Usage: %s [options]\n" Sys.argv.(0) in
  Arg.parse spec anon_fail usage;
  if !sxsetup_conf = "" then
    sxsetup_conf := Filename.concat Configure.sysconfdir "sxserver/sxsetup.conf"
  else
    open_errmsg := true
(*  List.iter ask_arg spec*)

let parse_value v =
  try
    Scanf.sscanf v "%S" (fun v -> v)
  with Scanf.Scan_failure _ ->
    v

let load_file name =
  let f = open_in name in
  let size = in_channel_length f in
  let str = String.make size ' ' in
  really_input f str 0 size;
  close_in f;
  str

type line = KV of string * string | Other of string

let load_config file =
  try
    let f = open_in file in
    let rec loop lst =
      try
        let line = input_line f in
        let entry =
          try
            Scanf.sscanf line "%[^= ] = %s@\n" (fun k v -> KV(k, parse_value v))
          with Scanf.Scan_failure _ | End_of_file ->
            Other line in
        loop (entry :: lst)
      with End_of_file ->
        close_in f;
        Printf.printf "Successfully loaded SX configuration from '%s'\n%!" file;
        List.rev lst in
    loop []
  with Sys_error msg ->
    if !open_errmsg then
      Printf.eprintf "Cannot open configuration file: %s\n%!" msg;
    []

let rec find lst key = match lst with
| [] -> raise Not_found
| KV (k, v) :: _ when k = key ->
    v
| _ :: tl ->
  find tl key

let rec replace add lst key value out_list = match lst with
| [] ->
    let out_list = List.rev out_list in
    if add then (KV (key, value)) :: out_list else out_list
| KV (k, _) :: tl when k = key ->
    replace false tl key value (KV (key, value) :: out_list)
| hd :: tl ->
    replace add tl key value (hd :: out_list)

let load_admin_key lst =
  let dir = find lst "data-dir" in
  load_file (Filename.concat dir "admin.key")

let libres3_conf () =
  Filename.concat Configure.sysconfdir "libres3/libres3.conf"

let fallback_read msg f x =
  try
    let result = f x in
    if result = "" then
      raise Not_found;
    Printf.printf "Successfully obtained %s\n%!" (String.lowercase msg);
    result
  with e ->
    (*Printf.eprintf "Warning (%s): %s\n%!" msg (Printexc.to_string e);*)
    flush stderr;
    read_value msg

let rec read_yes_no default msg =
  let choice = match default with
  | true -> "[Y/n]"
  | false -> "[y/N]" in
  Printf.eprintf "%s %s " msg choice;
  flush stdout;
  flush stderr;
  match (String.lowercase (input_line stdin)) with
  | "y" -> true
  | "n" -> false
  | "" -> default
  | _ -> read_yes_no default msg

let open_out_ask name =
  try
    open_out_gen [Open_wronly;Open_creat;Open_excl] 0o600 name
  with Sys_error _ ->
    Printf.printf "File '%s' already exists, overwriting\n%!" name;
    open_out name

let update_s3cfg is_https host port key name =
  Printf.printf "Updating '%s'\n" name;
  let lst = load_config name in
  let f = open_out (name ^ ".tmp") in
  (* restrict access to the file because it contains auth tokens *)
  Unix.fchmod (Unix.descr_of_out_channel f) 0o600;
  let hostport = host ^ ":" ^ (string_of_int port) in
  let lst = replace true lst "access_key" "admin" [] in
  let lst = replace true lst "secret_key" key [] in
  let lst = replace true lst "host_base" hostport [] in
  let lst = replace true lst "host_bucket" ("%(bucket)s." ^ hostport) [] in

  let use_https = if is_https then "True" else "False" in
  let lst = replace true lst "use_https" use_https [] in

  let lst = replace false lst "cloudfront_host" host [] in
  let lst = replace false lst "simpledb_host" host [] in
  let scheme = if is_https then "https" else "http" in
  let lst = replace false lst "website_endpoint" (scheme ^ "://" ^ hostport) [] in
  List.iter (function
    | KV (k,v) ->
      Printf.fprintf f "%s = %s\n" k v
    | Other s ->
      Printf.fprintf f "%s\n" s
  ) lst;
  close_out f;
  Sys.rename (name ^ ".tmp") name

let () =
  try
    let config = load_config !sxsetup_conf in
    let load = find config in
    let admin_key = fallback_read "Admin auth token" load "SX_ADMIN_KEY" in
    let this_ip = fallback_read "SX server IP/DNS name" load "SX_NODE_IP"
    and rundir = Filename.concat Configure.localstatedir "run"
    and webuser = fallback_read "Run as user" load "SX_SERVER_USER"
    and webgroup = fallback_read "Run as group" load "SX_SERVER_GROUP"
    and ssl_key = fallback_read "SSL key file" load "SX_SSL_KEY_FILE"
    and ssl_cert = fallback_read "SSL certificate file" load "SX_SSL_CERT_FILE"
    and replica_count = fallback_read "Default volume replica count" load "LIBRES3_REPLICA"
    and volume_size = "10G"
    in
    s3_host := fallback_read "S3 (DNS) name" load "LIBRES3_HOST";
    let name = libres3_conf () in
    Printf.printf "\nGenerating '%s'\n" name;
    let outfile = open_out_ask name in
    (* restrict access to the file because it contains auth tokens *)
    Unix.fchmod (Unix.descr_of_out_channel outfile) 0o600;
    Printf.fprintf outfile "# LibreS3 configuration file\n";
    Printf.fprintf outfile "secret_key=%S\n" admin_key;
    Printf.fprintf outfile "sx_host=%S\n" this_ip;
    Printf.fprintf outfile "s3_host=%S\n" !s3_host;
    Printf.fprintf outfile "pid_file=%S\n" (Filename.concat rundir "libres3/libres3.pid");
    Printf.fprintf outfile "user=%S\n" webuser;
    Printf.fprintf outfile "group=%S\n" webgroup;
    Printf.fprintf outfile "replica_count=%s\n" replica_count;
    Printf.fprintf outfile "volume_size=%s\n" volume_size;
    if Sys.file_exists ssl_cert && Sys.file_exists ssl_key then begin
      Printf.fprintf outfile "s3_ssl_certificate_file=%S\n" ssl_cert;
      Printf.fprintf outfile "s3_ssl_privatekey_file=%S\n" ssl_key;
    end;
    Printf.fprintf outfile "allow_volume_create_any_user=true";
    close_out outfile;
    update_s3cfg false !s3_host 8008 admin_key (Filename.concat Configure.sysconfdir "libres3/libres3-insecure.sample.s3cfg");
    update_s3cfg true !s3_host 8443 admin_key (Filename.concat Configure.sysconfdir "libres3/libres3.sample.s3cfg");
  with Sys_error msg ->
    Printf.eprintf "Error: %s\n" msg;
    exit 1
