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

open Site
open Ocsigen_config
open CodedIO
open UnixLabels

let name = "libres3"
type config = {
  logdir: string ref;
  datadir: string ref;
  uploaddir: string ref;
  commandpipe: string ref;
  maxrequestbodysize_mb: int ref;
  timeout: int ref;
  keepalivetimeout: int ref;
}

let not_empty s = String.length !s > 0

let str_entry ?ns name ?attrs value =
  Xml.tag ?ns name ?attrs [Xml.d !value];;

let str_entry_opt ?ns name ?attrs value =
  match !value with
  | Some s ->
    Xml.tag ?ns name ?attrs [Xml.d s]
  | None ->
    Xml.d "";;

let int_entry ?ns name ?attrs value =
  Xml.tag ?ns name ?attrs [Xml.d (string_of_int !value)];;

let set_default stref default =
  if !stref = "" then
    stref := default;;

let build_ssl_config () =
  match !Config.ssl_certificate_file, !Config.ssl_privatekey_file with
  | Some cert, Some key ->
    [
      int_entry "port" ~attrs:[Xml.attr "protocol" "HTTPS"] Config.base_ssl_port;
      Xml.tag "ssl" [
        str_entry "certificate" (ref cert);
        str_entry "privatekey" (ref key)
      ];
    ]
  | _ -> [];;

let build_config conf =
  Xml.tag "ocsigen" [
    Xml.tag "server" (List.rev_append (build_ssl_config ()) [
      int_entry "port" Config.base_port;
(*      str_entry "syslog" Config.syslog_facility;*)
      str_entry "logdir" conf.logdir;
      str_entry "datadir" conf.datadir;
      str_entry "uploaddir" conf.uploaddir;
      str_entry_opt "user" Config.user;
      str_entry_opt "group" Config.group;
      str_entry "commandpipe" conf.commandpipe;
      str_entry "charset" (ref "utf-8");
      Xml.tag "maxrequestbodysize" [
        Xml.d (Printf.sprintf "%dMiB" !(conf.maxrequestbodysize_mb))
      ];
      str_entry "mimefile" (ref "/etc/mime.types");
      int_entry "maxconnected" Config.max_connected;
      int_entry "clienttimeout" conf.timeout;
      int_entry "servertimeout" conf.keepalivetimeout;
      Xml.tag "extension" ~attrs:[Xml.attr "name" "libres3"] [];
      Xml.tag "host" ~attrs:[Xml.attr "defaulthostname" !(Config.base_hostname)] [
        Xml.tag "libres3" [];
      ];
    ])
  ];;

let write_config filech xml =
  let output = Xmlm.make_output ~nl:true (`Channel filech) in
  Xmlm.output_doc_tree (fun (x:Xml.t) -> x) output (None,xml);;

let try_chown dirname =
  match !Config.user with
  | Some u ->
    begin try
      let pw = getpwnam u in
      chown dirname pw.pw_uid pw.pw_gid
    with
    | Not_found | Unix_error(EPERM,_,_) -> ()
    end
  | None -> ()
;;

let rec mkdir_p dir ~perm =
  begin try
    mkdir dir ~perm;
  with
  | Unix_error(ENOENT,_,_) ->
    mkdir_p (Filename.dirname dir) ~perm;
    mkdir dir ~perm;
  | Unix_error(EEXIST,_,_) -> ()
  end;
  try_chown dir
;;

(* this lacks some error-handling code *)
let rec rmdirs dir =
    let d = opendir dir in
    try
        while true; do
            let dirent = readdir d in
            if dirent <> "." && dirent <> ".." then
            let entry = Filename.concat dir dirent in
            if (stat entry).st_kind = S_DIR then
                rmdirs entry
            else
                unlink entry
        done
    with
    | End_of_file ->
            closedir d;
            rmdir dir;;

let print_version () =
  Printf.printf "libres3 version %s\n%!" Version.version;
  exit 0
;;

let handle_error f () =
  try
    f ()
  with
  | Unix_error (err, fn, param) ->
      Printf.eprintf "Error in %s(%s): %s\n%!" fn param (error_message err)
  | Sys_error msg ->
      Printf.eprintf "Error: %s\n%!" msg
  | Failure msg ->
      Printf.eprintf "Error: %s\n%!" msg
  | e ->
      Printf.eprintf "Unexpected error: %s\n%!" (Printexc.to_string e)
;;

let handle_signal s msg =
  ignore (
    Sys.signal s (Sys.Signal_handle (fun _ ->
      prerr_endline msg;
      exit 3;
    ))
  );;

let rec wait_pipe file delay =
  if delay <= 0. then false
  else try
    ignore (Unix.lstat file);
    true
  with Unix.Unix_error(Unix.ENOENT,_,_) ->
    Netsys.sleep 0.1;
    wait_pipe file (delay -. 1.);;

let initialize config extra_spec =
  Cmdline.parse_cmdline extra_spec;

  let program = Sys.argv.(0) in
  let bindir = if Filename.is_relative program then
    Filename.dirname (Filename.concat Filename.current_dir_name program)
  else
    Filename.dirname program in
  set_default config.logdir Paths.log_dir;
  let dir = Paths.var_lib_dir in
  set_default config.datadir (Filename.concat dir "datadir");
  set_default config.uploaddir (Filename.concat dir "uploaddir");
  set_default config.commandpipe (Filename.concat dir "command.pipe");
  set_disablepartialrequests true;(* we handle it ourselves *)
  set_maxuploadfilesize (Some  5368709120L);
  set_respect_pipeline ();
  set_filebuffersize Config.buffer_size;
  set_netbuffersize Config.buffer_size;
  set_pidfile !Config.pidfile;
  (* remove stale PIDfile *)
  begin try unlink !Config.pidfile with _ -> () end;
  List.iter (fun d -> mkdir_p ~perm:0o770 !d) [
    config.logdir; config.datadir; config.uploaddir
  ];
  let configfile = Paths.generated_config_file in
  let ch = open_out configfile in
  write_config ch (build_config config);
  close_out ch;
  set_configfile configfile;

  register_all ()
;;

let command_pipe = Filename.concat Paths.var_lib_dir "command.pipe"
let reopen_logs _ =
  try
    let f = open_out command_pipe in
    begin try output_string f "reopen_logs\n" with _ -> () end;
    close_out f
  with _ ->
    ()

let run () =
  Config.max_connected := 350;
  handle_signal Sys.sigint "Exiting due to user interrupt";
  handle_signal Sys.sigterm "Exiting due to TERM signal";
  ignore (Sys.signal Sys.sighup Sys.Signal_ignore);

  Arg.current := 0;
  (* FIXME: these two should be separate executables *)
  let run_sxreport = ref false in
  let config = {
    logdir = ref "";
    datadir = ref "";
    uploaddir = ref "";
    commandpipe = ref "";
    maxrequestbodysize_mb = ref 5120;
    timeout = ref 30;
    keepalivetimeout = ref 30;
  } in
  let extra_spec = [
    "--uploaddir", Arg.Set_string config.uploaddir, " Upload temporary directory";
   (* Hidden options *)
    "--minthreads", Arg.Int set_minthreads, "";
    "--maxthreads", Arg.Int set_maxthreads, "";
    "--verbose", Arg.Unit set_verbose, "";
    "--veryverbose", Arg.Unit set_veryverbose, "";
    "--debug", Arg.Unit (fun () -> set_debugmode true), "";
    "--sxreport", Arg.Set run_sxreport, "";
    "--no-ssl", Arg.Clear Config.sx_ssl, "Disable SSL connection to SX nodes"
  ] in

  let result =
    try Sxreport.OK (initialize config extra_spec)
    with e -> Sxreport.Error e in

  if run_sxreport.contents then
    Sxreport.run Pervasives.stdout config result
  else match result with
  | Sxreport.Error e -> raise e
  | Sxreport.OK () ->
    flush_all ();
    Sys.set_signal Sys.sigchld (Sys.Signal_handle (fun _ ->
        Printf.eprintf "Failed to start server (check logfile: %s/errors.log)\n%!" (Paths.log_dir);
        exit 1
    ));
    ignore (Lwt_unix.on_signal Sys.sigusr1 reopen_logs);
    let ok = ref false in
    at_exit (fun () ->
      if not !ok then begin
        Printf.eprintf "Killing all children\n%!";
        (* kill self&all children *)
        Unix.kill 0 15
      end
    );
    if !Config.daemonize then begin
      Unix.chdir "/";
      ignore (Unix.setsid ());
      if Lwt_unix.fork () > 0 then begin
        (* Do not run exit hooks in the parent. *)
        Lwt_sequence.iter_node_l Lwt_sequence.remove Lwt_main.exit_hooks;
      end else begin
        ignore (Unix.setsid ());
        Ocsigen_server.start_server ();
        exit 0;
      end
    end;
    Printf.printf "Waiting for server to start (5s) ... %!";
    if wait_pipe !(config.commandpipe) 5. then begin
      Printf.printf "OK\n%!";
      ok := true;
    end else begin
      Printf.printf "still didn't start!\n%!";
      exit 1
    end
  ;;

let () =
  Printexc.record_backtrace true;
  Printexc.register_printer (function
    | Ocsigen_stream.Interrupted e ->
        Some ("Ocsigen_stream.Interrupted: " ^ (Printexc.to_string e))
    | _ -> None
  );
  handle_error run ();;
