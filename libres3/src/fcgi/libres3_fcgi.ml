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

open Netcgi_fcgip
let had_error = ref false
open SXThread
module Server = struct
  type t = Netcgi.cgi
  type u = Netchannels.trans_out_obj_channel

  type 'a monad = 'a Monad.t
  let log cgi str = cgi#environment#log_error str

  open Dispatch
  let send_headers (cgi:t) h =
    let fields =
      ("Content-Length",[Int64.to_string h.content_length]) ::
      (List.map (fun (n,v) -> n,[v]) h.reply_headers) in
    let fields = match h.last_modified with
    | None -> fields
    | Some mtime ->
        ("Last-Modified", [Util.format_date mtime]) :: fields in
    let fields = match h.etag with
    | None -> fields
    | Some etag ->
        ("ETag", [etag]) :: fields in
    cgi#set_header
      ~status:h.status
      ?content_type:h.content_type
      ~fields ();
    Monad.return cgi#out_channel
  ;;

  let send_data out (str, pos, len) =
    out#really_output str pos len;
    Monad.return ()
end

module FcgiServer =
  Dispatch.Make(SXIO)(IO)(Server)

let exn_handler (env:Netcgi.cgi_environment) f =
  try f ()
  with e ->
    (* fallback exception handler *)
    let bt = Printexc.get_backtrace () in
    let str = Printexc.to_string e in
    env#set_output_header_fields [
      "Server", Dispatch.server_name;
      "Content-Type", "application/xml"
    ];
    env#set_status `Internal_server_error;
    env#send_output_header ();
    let output s = env#out_channel#output_string s in
    output "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n";
    output
    "<Error><Code>InternalError</Code><RequestId>rid</RequestId><Resource>";
    output env#cgi_script_name;
    output "</Resource><Message>Fatal internal server error</Message><Exception>";
    output str;
    output "</Exception>";
    if Printexc.backtrace_status () then begin
      output "<Backtrace>";
      output bt;
      output "</Backtrace>";
    end;
    output "</Error>";
    env#out_channel#close_out();;

let conv_method = function
  | (`DELETE | `GET | `HEAD | `POST ) as a -> a
  | `PUT _ -> `PUT

open FcgiServer
open Monad

let stream_of_arg arg pos =
  (* TODO: body should be an input_stream not a source,
   * but SXIO's copy takes a source not an input_stream *)
  if pos <> 0L then
    fail (Failure "non-zero read position!")
  else
    let buf = String.create Config.buffer_size in
    let ch = arg#open_value_rd () in
    return (fun () ->
      try
        return (buf, 0, ch#input buf 0 (String.length buf))
      with End_of_file | Netchannels.Closed_channel ->
        ch#close_in;
        arg#finalize (* TODO: ensure its always called *);
        return ("", 0, 0)
    );;

let source_of_arg arg = {
  SXIO.meta = {
    SXIO.name = "";
    SXIO.size = 0L;
    SXIO.mtime = 0.;
  };
  SXIO.seek = stream_of_arg arg
}

let with_request_source cgi f = match cgi#request_method with
  | (`DELETE | `GET | `HEAD) as m ->
      f m None
  | `POST ->
      let source = source_of_arg (cgi#argument "BODY") in
      f (`POST source) None
  | `PUT put ->
      let source = source_of_arg put in
      let body = match put#store with
      | `File file -> Some file
      | `Memory -> None in
      f (`PUT source) body
      ;;

let process_request dispatcher (cgi: cgi) =
  had_error := false;(* TODO: pass this parameter without using a global *)
  let env = cgi#environment in
  let orig_uri = env#cgi_property ~default:"" "REQUEST_URI" in
  run (with_request_source cgi (fun req_method body_file ->
    handle_request dispatcher {
      server = (cgi :> Netcgi.cgi);
      meth = req_method;
      body_file = body_file;
      info = {
        CanonRequest.req_headers = env#input_header_fields;
        undecoded_url = orig_uri
      };
    } >>= fun () ->
    cgi#out_channel#commit_work ();
    return ()
  ));;

let is_small header =
  try
    header#content_length () < 65536
  with Not_found -> false;;

let arg_store _env _ header =
  if is_small header then
    `Memory_max 65536.
  else
    `File_max 5368709120.;;

let run_handler (dispatcher, config) =
  try
    Netcgi_fcgip.run
    ~exn_handler
    ~config ~output_type:(`Direct "")
    ~arg_store
    (process_request dispatcher)
  with e ->
    Printf.eprintf "Error: %s\n" (Printexc.to_string e);
    raise e;;


let daemon () =
  (* !! must ensure that we don't start pipeline before that,
   * you can't fork threads! *)
  Unix.chdir "/";

  match Unix.fork () with
  | 0 ->
      Pid.write_pid !Config.pidfile;
      let _ = Unix.setsid () in
      Unix.dup2 (Unix.openfile "/dev/null" [ Unix.O_RDONLY ] 0) Unix.stdin;
      Unix.dup2 (Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0) Unix.stdout;
      (* leave stderr open *)
		  Netsys_posix.run_post_fork_handlers();
  | child_pid ->
      Printf.printf "Running daemonized\n";
      Netsys._exit 0;;

let variables dir sock = function
  | "socket" -> sock
  | "tmpdir" -> dir
  | "host" -> !Config.base_hostname
  | "port" -> string_of_int !Config.base_port
  | "ssl_listen_directives" ->
      begin match !Config.ssl_certificate_file, !Config.ssl_privatekey_file with
      | Some cert, Some keyfile ->
        Printf.sprintf "listen %s:%d default_server ssl;\n\
          ssl_certificate %s;\n\
          ssl_certificate_key %s;\n"
            !Config.base_hostname !Config.base_ssl_port (Filename.quote cert) (Filename.quote keyfile);
      | _ -> ""
      end;
  | s ->
      failwith ("Unknown variable " ^ s)
  ;;

let nginx_template =
  "\
  worker_processes 4;\n\
  error_log $(tmpdir)/error.log;\n\
  pid $(tmpdir)/libres3-nginx.pid;\n\
  daemon off;\n\
  events {\n\
  \tworker_connections 768;\n\
  }\n\
  \n\
  http {\n\
  \       tcp_nopush on;\n\
  \       tcp_nodelay off;\n\
  \       default_type application/octet-stream;\n\
  \       access_log $(tmpdir)/access.log;\n\
  \       client_max_body_size 5G;\n\
  \       client_body_temp_path $(tmpdir)/;\n\
  \       proxy_temp_path $(tmpdir)/;\n\
  \       fastcgi_temp_path $(tmpdir)/;\n\
  \       uwsgi_temp_path $(tmpdir)/;\n\
  \       scgi_temp_path $(tmpdir)/;\n\
  \       server {\n\
  \            access_log $(tmpdir)/access.log;\n\
  \            error_log $(tmpdir)/error.log;\n\
  \            ssl_ciphers ECDHE-RSA-AES256-SHA384:AES256-SHA256:RC4:HIGH:!MD5:!aNULL:!EDH:!AESGCM;\n\
  \            ssl_prefer_server_ciphers on;\n\
  \n\
  \            listen [::]:$(port);\n\
  \            $(ssl_listen_directives)\n\
  \            root $(tmpdir);\n\
  \            location /500.xml { internal; }\n\
  \            location /502.xml { internal; }\n\
  \            location /504.xml { internal; }\n\
  \            location / {\n\
  \                error_page 500 /500.xml;\n\
  \                error_page 502 /502.xml;\n\
  \                error_page 504 /502.xml;\n\
  \                fastcgi_pass unix:$(socket);\n\
  \                fastcgi_max_temp_file_size 0;\n\
  \                fastcgi_store off;\n\
  \                fastcgi_read_timeout 600;\n\
  \                fastcgi_param\tQUERY_STRING\t\t\\$query_string;\n\
  \                fastcgi_param\tREQUEST_METHOD\t\t\\$request_method;\n\
  \                fastcgi_param\tCONTENT_TYPE\t\t\\$content_type;\n\
  \                fastcgi_param\tCONTENT_LENGTH\t\t\\$content_length;\n\
  \n\
  \                fastcgi_param\tSCRIPT_FILENAME\t\t\\$request_filename;\n\
  \                fastcgi_param\tSCRIPT_NAME\t\t\\$fastcgi_script_name;\n\
  \                fastcgi_param\tREQUEST_URI\t\t\\$request_uri;\n\
  \                fastcgi_param\tDOCUMENT_URI\t\t\\$document_uri;\n\
  \                fastcgi_param\tDOCUMENT_ROOT\t\t\\$document_root;\n\
  \                fastcgi_param\tSERVER_PROTOCOL\t\t\\$server_protocol;\n\
  \n\
  \                fastcgi_param\tGATEWAY_INTERFACE\tCGI/1.1;\n\
  \                fastcgi_param\tSERVER_SOFTWARE\t\tnginx/\\$nginx_version;\n\
  \n\
  \                fastcgi_param\tREMOTE_ADDR\t\t\\$remote_addr;\n\
  \                fastcgi_param\tREMOTE_PORT\t\t\\$remote_port;\n\
  \                fastcgi_param\tSERVER_ADDR\t\t\\$server_addr;\n\
  \                fastcgi_param\tSERVER_PORT\t\t\\$server_port;\n\
  \                fastcgi_param\tSERVER_NAME\t\t\\$server_name;\n\
  \            }\n\
  \       }\n\
  }\n\
  "
;;

let xml_500 =
  "\
  <?xml version=\"1.0\" encoding=\"UTF-8\"?>\n\
  <Error>\n\
  \  <Code>InternalError</Code>\n\
  \  <Message>Nginx internal error (out of space?)</Message>\n\
  </Error>\n\
  "
;;

let xml_502 =
  "\
  <?xml version=\"1.0\" encoding=\"UTF-8\"?>\n\
  <Error>\n\
  \  <Code>ServiceUnavailable</Code>\n\
  \  <Message>LibreS3 cannot be contacted</Message>\n\
  </Error>\n\
  "
;;

let xml_504 =
  "\
  <?xml version=\"1.0\" encoding=\"UTF-8\"?>\n\
  <Error>\n\
  \  <Code>SlowDown</Code>\n\
  \  <Message>LibreS3 reply timeout</Message>\n\
  </Error>\n\
  "
;;

let write_file dir name contents =
  let out = open_out (Filename.concat dir name) in
  output_string out contents;
  close_out out;;

let write_nginx dir sock =
  let buf = Buffer.create 4096 in
  (* substitute $(var) *)
  Buffer.add_substitute buf (variables dir sock) nginx_template;
  write_file dir "libres3-nginx.conf" (Buffer.contents buf);
  write_file dir "500.xml" xml_500;
  write_file dir "502.xml" xml_502;
  write_file dir "504.xml" xml_504
;;

let start_nginx dir =
  flush_all ();
  (*  TODO: also search our very own $(bindir) *)
  let additional_paths="/usr/sbin:/usr/local/sbin:/opt/local/sbin" in
  let path =
    try (Sys.getenv "PATH") ^ ":" ^ additional_paths
    with Not_found -> additional_paths in
  Unix.putenv "PATH" path;
  let pid =
    Unix.create_process "nginx" [|
        "nginx";"-c";Filename.concat dir "libres3-nginx.conf"
      |] Unix.stdin Unix.stdout Unix.stderr  in
  let _, status = Unix.waitpid [Unix.WNOHANG] pid in
  match status with
  | Unix.WEXITED 0 -> ()
  | Unix.WEXITED n ->
      Printf.eprintf "nginx exited with code %d\n" n;
      exit 1
  | _ ->
      Printf.eprintf "nginx was signaled/stopped\n";
      exit 1;;

let run () =
  if (Unix.geteuid () = 0 || Unix.getegid() = 0) then begin
    Printf.eprintf "Running as root is not supported!\n";
    exit 1;
  end;
  Printf.printf "libres3 fcgi version %s\n%!" Version.version;
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  Sys.set_signal Sys.sighup Sys.Signal_ignore;
  Sys.set_signal Sys.sigusr1 (Sys.Signal_handle (fun _ -> raise Exit));
  ignore (Unix.umask 0o027); (* make sure sockets are not writable by other *)
  Config.max_connected := 2;
  let fcgi_sock = ref "" in
  Cmdline.parse_cmdline [];

  let dir = Netsys_tmp.tmp_directory () in
  if !fcgi_sock = "" then begin
    try
      ignore (Unix.getpeername Unix.stdin);
    with
    | _ ->
        fcgi_sock := Filename.concat dir "fcgi.socket"
  end;
  (*if (getpeername(sock, (struct sockaddr *&sa, &len) != 0 && errno ==     ENOTSOCK) { *)
  let sock = if !fcgi_sock <> "" then begin
    Printf.printf "Opening fastcgi socket %s\n" !fcgi_sock;
    begin try Unix.unlink !fcgi_sock with _ -> () end;
    let sockaddr = Unix.ADDR_UNIX !fcgi_sock in
	  let sock = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
    Unix.setsockopt sock Unix.SO_REUSEADDR true;
	  Unix.bind sock sockaddr;
    Unix.listen sock 5;
    let null = Unix.openfile "/dev/null" [Unix.O_RDONLY] 0 in
    Unix.dup2 null Unix.stdin;
    Unix.close null;
    sock
  end else Unix.stdin in

  write_nginx dir !fcgi_sock;
  flush_all ();
  if !Config.daemonize then
    daemon () (* must do it before spawning threads *)
  else
    Printf.printf "Running in the foreground\n";
  start_nginx dir;

  let dispatcher = run (FcgiServer.init ()) in
  let config = { Netcgi.default_config with
    Netcgi.input_content_length_limit = 5*1024*1024*1024;
(*    Netcgi.max_arguments = 16;*)
    Netcgi.permitted_http_methods = [`DELETE; `GET; `HEAD; `POST; `PUT];
    Netcgi.permitted_input_content_types = [];
    Netcgi.default_exn_handler = true;
  } in

  Default.register ();
  Unix.dup2 sock Unix.stdin;
  let threads = Array.init !Config.max_connected (fun _ ->
    Thread.create run_handler (dispatcher, config)) in
  Array.iter Thread.join threads
;;

let () =
  Printexc.record_backtrace true;
  try
    run ()
  with
  | Exit -> exit 4
  | e ->
    Printexc.print_backtrace stderr;
    Printf.eprintf "\nError: %s\n" (Printexc.to_string e);
    exit 3;;
