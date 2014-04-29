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

open Ocsigen_extensions
open Ocsigen_http_frame
open Ocsigen_http_frame.Http_header

module Server = struct
  type 'a monad = 'a Lwt.t
  type t = {
    mutable headers: Dispatch.headers option;
    mutable woken: bool;
    headers_wait: unit Lwt.t;
    headers_wake: unit Lwt.u;
    mvar: (string * int * int) Lwt_mvar.t
  }
  type u = t
  let send_headers s h =
    s.headers <- Some h;
    s.woken <- true;
    Lwt.wakeup s.headers_wake ();
    Lwt.return s
  let send_data s data =
    Lwt_mvar.put s.mvar data
  let log _ str = Ocsigen_messages.warning str
end

open SXLwt
module OcsigenServer =
  Dispatch.Make(SXIO)(IO)(Server)
module D = Dispatch

let conv_method m src = match m with
  |  Http_header.GET -> `GET
  |  Http_header.POST -> `POST src
  |  Http_header.HEAD -> `HEAD
  |  Http_header.PUT -> `PUT src
  |  Http_header.DELETE -> `DELETE
  |  Http_header.TRACE -> `TRACE
  |  Http_header.OPTIONS -> `OPTIONS
  |  Http_header.CONNECT -> `CONNECT
  |  Http_header.LINK -> `LINK
  |  Http_header.UNLINK -> `UNLINK
  |  Http_header.PATCH -> `PATCH;;

open OcsigenServer

let convert_headers h =
  List.fold_left (fun a (name, value) ->
    Http_headers.add (Http_headers.name name) value a
  ) Http_headers.empty h;;

open Monad

let empty_read () =
  Ocsigen_stream.empty None

let return_eof server () =
  if not server.Server.woken then
    Lwt.wakeup server.Server.headers_wake ();
  Lwt_mvar.put server.Server.mvar ("EOF",0,0);;

let stream_of_reply wait_eof server =
  let eof = wait_eof >>= return_eof server in
  let rec read () =
    Lwt_mvar.take server.Server.mvar >>= fun (str, pos, len) ->
    if len = 0 then
      eof >>= fun () ->
      Ocsigen_stream.empty None
    else begin
      let substr =
        if pos = 0 && len = String.length str then str
      else String.sub str pos len in
      Ocsigen_stream.cont substr read
    end
  in
  Ocsigen_stream.make read
;;

let stream_of arg pos =
  if pos <> 0L then
    fail (Failure "position is not 0 in input")
  else
    let s = ref (Ocsigen_stream.get arg) in
    return (fun () ->
    Ocsigen_stream.next !s >>= function
    | Ocsigen_stream.Finished _ -> return ("",0,0)
    | Ocsigen_stream.Cont (buf, next_stream) ->
        s := next_stream;
        return (buf, 0, String.length buf)
    );;

let source_of arg size = {
  SXIO.meta = { SXIO.name = ""; SXIO.size = size; SXIO.mtime = 0.};
  SXIO.seek = stream_of arg
}

let empty_stream () = Ocsigen_stream.of_string ""

let stream_of_request ri =
  match ri.ri_method with
  | Http_header.POST | Http_header.PUT ->
      begin match ri.ri_http_frame.frame_content with
      | None -> empty_stream ()
      | Some input -> input
      end
  | _ -> empty_stream ();;

let process_request dispatcher ri () =
  let headers = Http_headers.fold (fun name values accum ->
    let namestr = Http_headers.name_to_string name in
    List.rev_append (List.map (fun v -> namestr, v) values) accum
  ) ri.ri_http_frame.frame_header.headers [] in
  let stream = stream_of_request ri in
  let w, u = Lwt.wait () in
  let server = {
    Server.headers = None;
    headers_wait = w; headers_wake = u;woken=false;
    mvar = Lwt_mvar.create_empty () } in
  let cl = match ri.ri_content_length with
  | Some l -> l
  | None -> 0L in
  let source = source_of stream cl in
  let req_method = conv_method ri.ri_method source in
  let undecoded_url = match ri.ri_http_frame.frame_header.mode with
  | Query (_, url) -> url
  | _ -> "/" ^ ri.ri_url_string in
  let wait_eof =
    handle_request dispatcher {
      server = server;
      meth = req_method;
      body_file = None;(* TODO: write to tmpfile *)
      info = {
        CanonRequest.req_headers = headers;
        undecoded_url = undecoded_url;
      };
    } in
  Ocsigen_senders.Stream_content.result_of_content
    (stream_of_reply wait_eof server) >>= fun res ->
      server.Server.headers_wait >|= fun () ->
      match server.Server.headers with
      | None -> res
      | Some h ->
        { res with
          res_code = Nethttp.int_of_http_status h.D.status;
          res_content_length = Some h.D.content_length;
          res_content_type = h.D.content_type;
          res_headers = convert_headers h.D.reply_headers;
          res_lastmodified = h.D.last_modified;
          res_etag = h.D.etag
      };;

let fun_site _ config_info _ _ _ _ =
  Config.base_hostname := config_info.default_hostname;
  Ocsigen_messages.console2 (
    Printf.sprintf "LibreS3 default hostname: %s"  !Config.base_hostname
  );
  begin try
    Ssl.set_cipher_list !Ocsigen_http_client.sslcontext
      "ECDHE-RSA-AES256-SHA384:AES256-SHA256:RC4:HIGH:!MD5:!aNULL:!EDH:!AESGCM";
  with _ -> () end;
  Default.register ();
  let dispatcher = Lwt_main.run (OcsigenServer.init ()) in
  Ocsigen_messages.console2 "Startup complete";
  function
    | Req_not_found (_, request) ->
      Lwt.return (Ext_found (process_request dispatcher request.request_info))
    | Req_found _ ->
      Lwt.return Ext_do_nothing
  ;;

let register_all () =
  Ocsigen_extensions.register_extension ~fun_site ~name:"libres3" ()
