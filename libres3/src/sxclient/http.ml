(**************************************************************************)
(*  SX client                                                             *)
(*  Copyright (C) 2012-2014 Skylable Ltd. <info-copyright@skylable.com>   *)
(*                                                                        *)
(*  This library is free software; you can redistribute it and/or         *)
(*  modify it under the terms of the GNU Lesser General Public            *)
(*  License as published by the Free Software Foundation; either          *)
(*  version 2.1 of the License, or (at your option) any later version.    *)
(*                                                                        *)
(*  This library is distributed in the hope that it will be useful,       *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *)
(*  Lesser General Public License for more details.                       *)
(*                                                                        *)
(*  You should have received a copy of the GNU Lesser General Public      *)
(*  License along with this library; if not, write to the Free Software   *)
(*  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,            *)
(*  MA  02110-1301  USA                                                   *)
(*                                                                        *)
(*  As a special exception to the GNU Library General Public License,     *)
(*  you may link, statically or dynamically, a "work that uses the        *)
(*  Library" with a publicly distributed version of the Library to        *)
(*  produce an executable file containing portions of the Library, and    *)
(*  distribute that executable file under terms of your choice, without   *)
(*  any of the additional requirements listed in clause 6 of the GNU      *)
(*  Library General Public License. By "a publicly distributed version    *)
(*  of the Library", we mean either the unmodified Library, or a          *)
(*  modified version of the Library that is distributed under the         *)
(*  conditions defined in clause 3 of the GNU Library General Public      *)
(*  License. This exception does not however invalidate any other         *)
(*  reasons why the executable file might be covered by the GNU Library   *)
(*  General Public License.                                               *)
(**************************************************************************)

type methods = [ `GET | `POST | `HEAD | `PUT | `DELETE | `TRACE | `OPTIONS ]
type request = {
  meth: methods;
  host: string;
  port: int;
  relative_url: string;
  req_headers: (string *string) list;
  req_body: string;
}

type reply = {
  headers: Netmime.mime_header_ro;
  body: string;
  code: int;
  status: Nethttp.http_status;
  req_host: string;
}

module type Pipeline = sig
  type 'a t
  type pipeline
  val start_pipeline : unit -> pipeline
  val stop_pipeline : pipeline -> unit
  val make_http_request : pipeline -> request -> reply t
  val make_cached_request : pipeline -> key:string -> request -> string
  t
  val input_of_async_channel: string -> (unit -> (string *int * int) t)
end

let () = Ssl.init ~thread_safe:true ();;
module MakePipeline (M:Sigs.Monad)(W:Sigs.ThreadMonad with type 'a t = 'a M.t)
: (Pipeline with type 'a t = 'a M.t) = struct
  module M = M
  type 'a t = 'a M.t
  open Http_client
  exception HTTP_Job_Callback of http_call * (http_call -> unit)
    (* This is not an exception in the usual sense, but simply a tagged
     * pair (call, f_done). This pair is pushed onto the event queue to
     * send another HTTP request [call] to the HTTP thread. When the
     * request is processed, the function [f_done] is called. Note that
     * [f_done] is called in the context of the HTTP thread, and it must
     * arrange some synchronisation with the calling thread to return
     * the result.
     *)
  type pipeline = Unixqueue.event_system * Unixqueue.group * Thread.t

  let https_setup pipeline =
    let ctx = Ssl.create_context Ssl.TLSv1 Ssl.Client_context in
    let tct = Https_client.https_transport_channel_type ctx in
    pipeline # configure_transport Http_client.https_cb_id tct

  let rec run esys =
    try
      Unixqueue.run esys
    with e ->
      (* catch exceptions escaping Uq, and restart the event loop *)
      (* debugging *)
      Printexc.print_backtrace stderr;
      Printf.eprintf "Uq error: %s\n" (Printexc.to_string e);
      run esys
  ;;

  let http_thread (esys, keep_alive_group, handler_added) =
    let pipeline = new pipeline in
    let cache = create_aggressive_cache () in
(*    Http_client.Debug.enable := true;
    Uq_ssl.Debug.enable := true;
    Netlog.Debug.enable_all ();*)
    pipeline#set_event_system esys;
    pipeline#set_connection_cache cache;
    https_setup pipeline;
    pipeline#set_options { pipeline#get_options with
      Http_client.connection_timeout = 10.;
      synchronization = Sync (* disable pipelining, but keep persistence *)
};
    Unixqueue.add_handler esys keep_alive_group (fun _ _ event ->
      match event with
	    | Unixqueue.Extra (HTTP_Job_Callback (call, cb)) ->
        pipeline # add_with_callback call cb
	    | _ ->
	      raise Equeue.Reject  (* The event is not for us *)
    );
    handler_added (W.result (fun () -> ()));
    (* Now start the event queue. It returns when all jobs are done and
     * the keep_alive_group is cleared.
    *)
    run esys

  let stop_pipeline (esys, keep_alive_group,thread) =
    Unixqueue.clear esys keep_alive_group;
    Thread.join thread
  ;;

  let start_pipeline () =
    let esys = Unixqueue.create_unix_event_system() in
    (* from http_mt:ml
     * In order to keep the event system active when there are no HTTP requests
     * to process, we add an artificial timer that never times out (-1.0).
     * The timer is bound to a Unixqueue group, and by clearing this group
     * the timer can be deleted.
     *)
    let keep_alive_group = Unixqueue.new_group esys in
    let w = Unixqueue.new_wait_id esys in
    Unixqueue.add_resource esys keep_alive_group (Unixqueue.Wait w,(-1.0));
    Printf.printf "Starting http pipeline ... %!";
    let wait_handler_added, handler_added = W.wait () in
    let thread = Thread.create http_thread (esys, keep_alive_group, handler_added) in
    M.run wait_handler_added;
    Printf.printf " http pipeline up!\n%!";
    esys, keep_alive_group, thread
  ;;

  let http_call (esys,_,_) (call, host) =
    let waiter, wakener = W.wait () in
    let handle_reply = fun call ->
      (* this runs in http_thread *)
      match call#status with
      | `Http_protocol_error e ->
          wakener (W.result (fun () -> raise (Http_protocol e)))
      | _ ->
          let reply = {
            headers = (call#response_header :> Netmime.mime_header_ro);
            body = call#response_body#value;
            code = call#response_status_code;
            req_host = host;
            status = call#response_status
          } in
        wakener (W.result (fun () -> reply ))
      in

    (* the callback is needed when it fails to connect *)
    Unixqueue.add_event esys
      (Unixqueue.Extra (HTTP_Job_Callback (call, handle_reply)));
    waiter;;

  let call_of_request req =
    let call = match req.meth with
    | `GET -> new get_call
    | `POST -> new post_call
    | `HEAD -> new head_call
    | `PUT -> new put_call
    | `DELETE -> new delete_call
    | `TRACE -> new trace_call
    | `OPTIONS -> new options_call in
    let scheme = if !Config.sx_ssl then "https" else "http" in
    let url =
      Printf.sprintf "%s://%s:%d%s" scheme req.host req.port req.relative_url in
    call#set_request_uri url;
    let headers = new Netmime.basic_mime_header req.req_headers in
    if req.meth = `PUT then
    headers#update_field "Content-Length" (
        string_of_int (String.length req.req_body)
    );
    call#set_request_header headers;
    call#set_request_body (new Netmime.memory_mime_body req.req_body);
    call, req.host;;

  let make_http_request state req =
    http_call state (call_of_request req)
  ;;

  let return_http_error status =
    raise (Sigs.HttpCode status)

  module MCache = Cache.Make(M)(struct
    type t = string
    let compare a b = String.compare a b
    type data = string
    (* TODO: cache needs to be aware of data size,
     * and count in units of bytes instead of cached elements *)
    let cache_size = 1000
  end)

  let perform_cached_request esys request =
    let call, _ = call_of_request request in
    if not call#is_idempotent then
      M.fail (Invalid_argument "cached request only valid on GET and HEAD")
    else begin
      let waiter, wakener = W.wait () in
      let cb = (fun call ->
        wakener (W.result (fun () ->
          match call#response_status with
          | `Ok ->
              call#response_body#value
          | status ->
              return_http_error status
        ))) in
      Unixqueue.add_event esys (Unixqueue.Extra (HTTP_Job_Callback (call, cb)));
      waiter
    end
  ;;

  let make_cached_request (esys,_,_) ~key request =
    MCache.bind (M.return key) (fun _ ->
      perform_cached_request esys request
     )
  ;;

  let noop () = ()
  open M
  let input_of_async_channel str =
    let first = ref true in fun () ->
      if !first then begin
        first := false;
        return (str, 0, String.length str)
      end else
        return ("",0,0)
  ;;
end
