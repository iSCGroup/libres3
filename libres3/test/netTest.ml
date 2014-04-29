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

open Http_client
open HttpTest

let out = open_out "reply.log"

let map_reply call =
  output_string out "------- Request\n";
  Printf.fprintf out  "\tmethod: %s\n" call#request_method;
  Printf.fprintf out  "\teffective URI: %s\n" call#effective_request_uri;
  Printf.fprintf out  "--- Headers\n";
  let headers = (call#request_header `Effective)#fields in
  List.iter (fun (name,value) ->
    Printf.fprintf out  "\t%s: %s\n" name value) headers;
  let body = call#request_body#value in
  Printf.fprintf out  "---Body (%d bytes)\n" (String.length body);
  output_string out  body;
  output_string out "------- Reply\n";
  Printf.fprintf out  "\tStatus : %d (%s)\n"
    call#response_status_code
    call#response_status_text;
  Printf.fprintf out  "\tProtocol: %s\n" call#response_protocol;
  output_string out "--- Headers\n";
  let headers = call#response_header#fields in
  List.iter (fun (name,value) ->
    Printf.fprintf out  "\t%s: %s\n" name value) headers;
  let body = call#response_body#value in
  Printf.fprintf out  "--- Body (%d bytes)\n" (String.length body);
  output_string out  body;
  output_string out  "\n--------\n";
  {
    code = call#response_status_code;
    body = call#response_body#value;
    headers = call#response_header#fields
  };;

let perform_http_queries lst =
  let pipeline = new pipeline in
  (* without this we get an 'EOF on message' error *)
  pipeline#set_options { pipeline#get_options with
      Http_client.connection_timeout = 10.; };
  let calls = List.rev_map (fun req ->
    let call = match req.meth with
    | `GET -> new get_call
    | `POST -> new post_call
    | `HEAD -> new head_call
    | `PUT -> new put_call
    | `DELETE -> new delete_call
    | `TRACE -> new trace_call
    | `OPTIONS -> new options_call in
    let url =
      Printf.sprintf "http://%s:%d%s" req.host req.port req.relative_url in
    call#set_request_uri url;
    call#set_request_header (new Netmime.basic_mime_header req.req_headers);
    call#set_request_body (new Netmime.memory_mime_body req.req_body);
    pipeline#add call;
    call
  ) lst in
  (* TODO: set resolver to ensure that *.libres3.skylable.com is resolved to
   * 127.0.0.1 even if network is down *)
  pipeline#run ();
  List.rev (List.rev_map map_reply calls);;

let build_tests name testcases =
  HttpTest.generate_tests name perform_http_queries testcases;;

let print_version () =
  Printf.printf "libres3test version %s\n%!" Version.version;
  exit 0
;;

let parse_s3cfg s3cfg =
  try
    let f = open_in s3cfg in
    Printf.printf "Using s3cfg from '%s'\n" s3cfg;
    begin try
      while true; do
        let line = input_line f in
        try
        Scanf.sscanf line "%s = %s" (fun key value ->
          match key with
          | "access_key" ->
            S3Test.key_id := value;
          | "host_base" ->
              begin try
                Scanf.sscanf value "%s@:%d" (fun host port ->
                  Config.base_hostname := host;
                  Config.base_port := port)
              with Scanf.Scan_failure _ | End_of_file ->
                Config.base_hostname := value;
                Config.base_port := 80
              end
          | "secret_key" ->
            S3Test.secret_access_key := value
          | _ -> ()
        );
        with Scanf.Scan_failure _ | End_of_file -> ()
      done
    with End_of_file -> ();
    end;
    Printf.printf "Host: %s:%d\nAccess key id: %s\n"
      !Config.base_hostname !Config.base_port !S3Test.key_id;
    close_in f
  with
  | Sys_error e ->
      Printf.eprintf "Error: %s\n" e;
      exit 2;;

let set_backtrace () =
  Printexc.record_backtrace true;;

let noop () = () (* oUnit handles it *)

let arg_specs = Arg.align [
  "-verbose", Arg.Unit noop, " Run the test in verbose mode.";
  "-only-test", Arg.Unit noop, " path Run only the selected test";
  "--s3cfg", Arg.String parse_s3cfg,
    " Use S3 server, key id and secret access key from specified .s3cfg";
  "--backtrace", Arg.Unit set_backtrace,
    " Show backtrace on exceptions";
  "--version", Arg.Unit print_version, " Print version";
  "-V", Arg.Unit print_version, " Print version";
]

let _ =
    (* must parse before calling suite () to override the access key,
     * because suite () already uses it and the parsing callback
     * from oUnit would be too
     * late to change it *)
  Arg.parse arg_specs (fun _ -> ()) ("Usage: " ^ Sys.argv.(0) ^ " [options]");
(*  Arg.current := 0;*)
  OUnit.run_test_tt_main (build_tests "Http" (S3TestData.suite true));;
