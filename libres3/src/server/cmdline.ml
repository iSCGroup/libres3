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

let parse_value v =
  try `Int (Int64.of_string v)
  with Failure _ ->
    try
      Scanf.sscanf v "%S" (fun v -> `String v)
    with Scanf.Scan_failure _ ->
      Scanf.sscanf v "%s" (fun v -> `String v);;

let format_value = function
  | `String s -> Printf.sprintf "%S" s
  | `Int i -> Int64.to_string i;;

let parse spec line =
  if String.length line = 0 then None
  else if line.[0] = '#' then None
  else
    try Some (Scanf.sscanf line "%[^= ] = %s@\n" (fun k v ->
      let k = String.lowercase k in
      if not (StringSet.mem k spec) then
        raise (Failure ("Unknown configuration key: " ^ k));
      k, parse_value v
    ))
    with End_of_file -> None;;

let parse_configuration_line spec ch accum =
  match parse spec (input_line ch) with
  | Some (key, v) -> StringMap.add key v accum
  | None -> accum;;

let save spec ch defaults =
  Printf.fprintf ch "# LibreS3 configuration file\n\
    #\n# Uncomment the lines that you want to change.\n\
    # Format is key=\"value\" (or key=value for integers) with optional whitespace around the equal sign.\n\n";
  List.iter (fun (key, _, doc) ->
    Printf.fprintf ch "\n# %s\n#%s=" doc key;
    try
      let v = StringMap.find key defaults in
      Printf.fprintf ch "%s\n" (format_value v)
    with Not_found ->
      Printf.fprintf ch "\n"
  ) spec;;

let print_config key v =
  if key <> "secret_key" then
    Printf.printf "\t%s=%s\n" key (format_value v);;

let load_configuration spec =
  let keys = List.fold_left (fun accum (key, _, _) ->
    StringSet.add (String.lowercase key) accum) StringSet.empty spec in
  let config = Paths.process_configuration Config.config_file
    ~parse:(parse_configuration_line keys)
    ~print:(save spec) in
  List.iter (fun (key, fn, _) ->
    try
      fn (StringMap.find key config)
    with
    | Not_found -> ()
    | Failure msg ->
        raise (Failure (Printf.sprintf "Failed to parse configuration key %s: %s"
          key msg))
  ) spec;
  Printf.printf "Configuration:\n";
  StringMap.iter print_config config;
  config;;

let print_version () =
  Printf.printf "libres3 version %s\n%!" Version.version;
  exit 0
;;

let expect_string f = function
  | `String s -> f s
  | `Int _ ->
      failwith "expected string value, got integer";;

let expect_int f = function
  | `Int i -> f (Int64.to_int i)
  | `String _ ->
      failwith "expected integer value, got string";;

let set_string_opt ref = expect_string (fun s -> ref := Some s);;
let set_string ref = expect_string (fun s -> ref := s);;
let set_int ref = expect_int (fun i -> ref := i);;
let set_bool ref = function
  | `String s ->
    begin match String.lowercase s with
    | "true" | "yes" | "on" -> ref := true
    | "false" | "no" | "off" -> ref := false
    | _ -> failwith "expected boolean value, but got '%s'" s
    end
  | `Int _ ->
      failwith "expected boolean value, got integer";;

let set_size ref = function
  | `String s ->
    begin try
      Scanf.sscanf s "%g%c" (fun size suffix ->
          let size = match suffix with
            | 'T' | 't' -> size *. (2. ** 40.)
            | 'G' | 'g' -> size *. (2. ** 30.)
            | 'M' | 'm' -> size *. (2. ** 20.)
            | 'K' | 'k' -> size *. (2. ** 10.)
            | c -> failwith "Unknown suffix in '%s'" s in
          ref := size)
    with _ ->
      failwith "expected size with suffix: '%s'" s
    end
  | `Int i -> ref := Int64.to_float i
;;

let parse_cmdline additional_args =
  let stop = ref false in
  let spec = (Arg.align [
    "--stop", Arg.Set stop, " Stop running process (based on PIDfile)";
    "--nofork", Arg.Clear Config.daemonize, " Run in foreground mode (default:\
      daemonize)";
    "--verbose", Arg.Unit (fun () -> Config.verbose := true), " Run in verbose mode";
    "--version", Arg.Unit print_version, " Print version";
    "-V", Arg.Unit print_version, " Print version";
    "--fs", Arg.Set_string Config.buckets_dir, "Filesystem path to use (FS mode)";
  ] @ additional_args) in
  let usage = (Printf.sprintf "Usage: %s [options]\n" Sys.argv.(0)) in
  Arg.parse spec (fun anon ->
    raise (Arg.Bad ("invalid option " ^ anon))
  ) usage;
  let tmpdir_base = ref None in
  ignore (load_configuration [
    "secret_key", set_string Config.secret_access_key,
      " SX secret access key";
    "s3_host", set_string Config.base_hostname,
      " Base hostname to use (equivalent of s3.amazonaws.com, host_base in .s3cfg)";
    "s3_port",set_int Config.base_port,
      " Bind to specified port";
    "s3_ssl_port", set_int Config.base_ssl_port,
      " Bind to specified port for HTTPS";
    "s3_ssl_certificate_file", set_string_opt Config.ssl_certificate_file,
      " The path to the SSL certificate";
    "s3_ssl_privatekey_file", set_string_opt Config.ssl_privatekey_file,
      " The path to the SSL certificate's private key";
    "sx_host", set_string_opt Config.sx_host,
      " Hostname of an SX cluster node";
    "replica_count", set_int Config.replica_count,
      " Default volume replica count";
    "volume_size", set_size Config.volume_size,
      " Default volume size [K,M,G,T suffixes accepted]";
    "tmpdir_base", set_string_opt tmpdir_base,
      " Base temporary directory";
    "pid_file", set_string Config.pidfile,
      " Specify the file where to write the pid of the server";
    "user", set_string_opt Config.user,
      " User to use when dropping privileges";
    "group", set_string_opt Config.group,
      " Group to use when  dropping privileges";
    "max_parallel", set_int Config.max_connected,
        " Maximum number of connections to handle in parallel";
    "allow_volume_create_any_user", set_bool Config.volume_create_elevate_to_admin,
        " Allow creating volumes as any user (elevate to admin privileges)"

  ]);
  if !Config.pidfile = "" then begin
    Printf.eprintf "pid_file is not set!\n";
    raise Exit
  end;
  if !stop then begin
    Pid.kill_pid !Config.pidfile;
    exit 0
  end;
  try
    if !Config.base_hostname = "" then
      raise (Arg.Bad "s3_host must be set");
    let mode_sx = !Config.sx_host <> None
    and mode_fs = !Config.buckets_dir <> "/" in
    if mode_sx && mode_fs then
      raise (Arg.Bad "You cannot use both SX and FS mode");
    if not mode_sx && not mode_fs then
      raise (Arg.Bad "SX host is not set");
    if !Config.ssl_certificate_file = Some "" then
      Config.ssl_certificate_file := None;
    if !Config.ssl_privatekey_file = Some "" then
      Config.ssl_privatekey_file := None;
    begin match !Config.ssl_certificate_file, !Config.ssl_privatekey_file with
    | Some _, Some _ -> ()
    | None, None -> ()
    | _ -> raise (Arg.Bad "You must specifiy both s3_ssl_certificate_file and \
    s3_ssl_privatekey_file")
    end;
    if Unix.geteuid () = 0 then begin
      if !Config.user = None then
        raise (Arg.Bad "When running as root you must set user");
      if Unix.getegid () = 0 && !Config.group = None then
        raise (Arg.Bad "When running as root you must set group");
    end else if Unix.getgid() = 0 then
      raise (Arg.Bad "Running as root group, but not as root user: cannot change groups")
    else begin
      begin match !Config.user with
      | Some u ->
          Printf.eprintf "Ignoring user %s directive: not running as root\n" u;
      | None -> ()
      end;
      begin match !Config.group with
      | Some g ->
          Printf.eprintf "Ignoring group %s directive: not running as root\n" g;
      | None -> ();
      end;
      Config.user := None;
      Config.group := None;
    end;
    Printf.printf "Starting an HTTP S3 server on %s:%d\n"
      !Config.base_hostname !Config.base_port;
    if !Config.ssl_privatekey_file <> None then
      Printf.printf "Starting an HTTPS S3 server on %s:%d\n"
        !Config.base_hostname !Config.base_ssl_port;
    match !Config.sx_host with
    | Some sx ->
      Printf.printf "Connecting to SX backend at %s\n"
        sx
    | None ->
      Printf.printf "Using the FS backend at %s\n"
        !Config.buckets_dir
  with Arg.Bad msg ->
    Printf.eprintf "Command-line error: %s!\n" msg;
    Arg.usage spec usage;
    raise Exit;;

