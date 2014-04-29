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

exception Detail of exn * (string * string) list
type metafn = unit -> (string * string) list
module Make(M:Sigs.Monad) = struct
  type 'a t = 'a M.t(* monad *)
  module M = M
  type output_data = string * int * int
  type input_stream = unit -> output_data t
  type output_stream = output_data -> unit t
  

  type entry = {
    name: string; (* absolute path *)
    size: int64;
    mtime: float
  }

  type source = {
    meta: entry;
    seek: int64 -> input_stream t
  }
  type sink = int64 -> output_stream t

  let read_string str pos =
    let eof = ref false in
    M.return (fun () ->
      if !eof then M.return ("",0,0)
      else begin
        eof := true;
        let len = Int64.sub (Int64.of_int (String.length str)) pos in
        if len < 0L then M.return ("",0,0)
        else M.return (str, Int64.to_int pos, Int64.to_int len)
      end
    );;

  open M
  let rec iter stream f =
    stream () >>= fun (str,pos,len) ->
    f (str,pos,len) >>= fun () ->
    if len = 0 then return ()
    else iter stream f
  ;;

  (* sources *)
  let of_string str = `Source {
    meta = {
      name = "";
      size = Int64.of_int (String.length str);
      mtime = Unix.gettimeofday ();
    };
    seek = read_string str;
  }
  let of_source src = `Source src
(*  let of_fd_in ch =
    `Source ch;;*)

  (* sinks *)
  let of_sink dst = `Sink dst
(*  let of_fd_out ch =
    `Sink ch;;*)

  (* URLs *)
  type url = Neturl.url

  let scheme_re = Netstring_str.regexp "^\\([a-zA-Z][a-zA-Z0-9+.-]*\\)://"
  let schemes = Hashtbl.create 16
  type op = {
    with_url_source : 'a. url -> (source -> 'a t) -> 'a t;
    with_urls_source : 'a. url list -> int64 -> (source -> 'a t) -> 'a t;
    fold_list: 'a. url -> ('a -> entry -> 'a t) -> (string -> bool) -> 'a -> 'a t;
    create: ?metafn:metafn -> ?replica:int -> url -> unit t;
    exists: url -> int64 option t;
    token_of_user: url -> string option t;
    check: url -> string option t;
    delete: url -> unit t;
    copy_same: url -> url -> bool t;
    get_meta: url -> (string*string) list t;
    put: ?metafn:metafn -> source -> int64 -> url -> unit t
  }
  let scheme_ops : (string,op) Hashtbl.t = Hashtbl.create 16

  let encode_8bits s =
    let b = Buffer.create (String.length s) in
    String.iter (fun c ->
      let n = Char.code c in
      if n >= 0x80 then
        Buffer.add_string b (Printf.sprintf "%%%02X" n)
      else
        Buffer.add_char b c
    ) s;
    Buffer.contents b;;

  open Neturl
  let path_norm_encode f components =
    norm_path (List.rev (List.rev_map f components));;

  let encode_unsafe str = Neturl.fixup_url_string (encode_8bits str);;

  let encode_opt f url =
    try Some (encode_8bits (f ?encoded:(Some true) url))
    with Not_found -> None;;

  let encode_opt_list f url =
    try
      Some (List.map encode_8bits (f ?encoded:(Some true) url))
    with Not_found ->
      None;;

  let normalize_url url =
    let syntax = url_syntax_of_url url in
    let path_encoder =
      if syntax.url_enable_query = Url_part_not_recognized then
        Netencoding.Url.encode ~plus:true (* fully encode, input is not encoded *)
      else
        encode_unsafe (* encode just unsafe and 8bits *) in
    let url = Neturl.remove_from_url ~fragment:true ~other:true url in
    Neturl.modify_url ~encoded:true
      ?user:(encode_opt url_user url)
      ?user_param:(encode_opt_list url_user_param url)
      ?password:(encode_opt url_password url)
      ~path:(path_norm_encode path_encoder (url_path ~encoded:true url))
      ?param:(encode_opt_list url_param url)
      ?query:(encode_opt url_query url)
      url;;

  let of_url str =
    `Url (match Netstring_str.string_match scheme_re str 0 with
    | Some result ->
        let scheme = Netstring_str.matched_group result 1 str in
        if Hashtbl.mem schemes scheme then
          let url =
            Neturl.parse_url ~schemes ~accept_8bits:true ~enable_fragment:true str in
          normalize_url (Neturl.ensure_absolute_url url)
        else
          failwith ("Unsupported URL scheme: " ^ scheme)
    | None ->
      (* URL without scheme: convert to file:// URL *)
        Neturl.file_url_of_local_path str);;

  (*let details = Hashtbl.find scheme_ops (url_scheme url) in*)
  let of_neturl url =
    `Url (Neturl.ensure_absolute_url url);;

  let ops_of_url url =
    let scheme = url_scheme url in
    try
      Hashtbl.find scheme_ops scheme
    with Not_found ->
      (* TODO: use fail *)
      raise (Failure ("Unregistered URL scheme: " ^ scheme));;

  let with_url_source (`Url url) f =
    (ops_of_url url).with_url_source url f;;

  let with_urls_source urls filesize f =
    match urls with
    | [] ->
        let `Source s = of_string "" in
        f s
    | (`Url url) :: tl ->
        (* TODO: check they all have same scheme *)
        let urls = List.rev (List.rev_map (fun (`Url url) -> url) urls) in
      (ops_of_url url).with_urls_source urls filesize f;;

  let remove_base base str =
    let n = String.length str in
    let baselen = String.length base in
    if String.sub str 0 baselen = base then
      String.sub str baselen (n - baselen)
    else
      "";;

  let fold_list ~base:(`Url base) (`Url url) ~entry ~recurse =
    let prefix = join_path (url_path base) in
    let fold_entry accum e =
      entry accum {
        e with
        name = remove_base prefix e.name
      } in
    let fold_recurse dir =
      recurse (remove_base prefix dir) in
    (ops_of_url url).fold_list url fold_entry fold_recurse;;

  (* TODO: move to common *)
  let is_prefix ~prefix str =
    let plen = String.length prefix in
    (String.length str) >= plen &&
    (String.sub str 0 plen) = prefix;;

  (* TODO: move to common *)
  let is_prefix str ~prefix =
    let plen = String.length prefix in
    let n = String.length str in
    n >= plen &&
    (String.sub str 0 plen) = prefix;;

  let prefix_re_match base re name =
    is_prefix ~prefix:base name &&
    match re with
    | Some regexp ->
      (Netstring_str.string_match regexp name (String.length base)) <> None
    | None -> true;;

  let fold_list_filter (`Url url) ~filter_re f accum =
    let base = Neturl.join_path (Neturl.url_path url) in
    let entry accum e =
      if prefix_re_match base filter_re e.name then
        f accum e
      else return accum in
    let recurse dir = is_prefix ~prefix:base dir in
    fold_list (`Url url) ~entry ~recurse accum;;

  let create ?replica (`Url url) =
    (ops_of_url url).create ?replica url;;

  let exists (`Url url) =
    (ops_of_url url).exists url;;

  let token_of_user (`Url url) =
    (ops_of_url url).token_of_user url;;

  let check (`Url url) =
    (ops_of_url url).check url;;

  let delete (`Url url) =
    (ops_of_url url).delete url;;

  let put ?metafn dsturl source ~srcpos =
    (ops_of_url dsturl).put ?metafn source srcpos dsturl;;

  let with_src src f = match src with
    | `Source source -> f source
    | `Url _ as srcurl -> with_url_source srcurl f;;

  let noop _ = return ()

  let generic_copy ?metafn src ~srcpos dst = match dst with
  | `Null ->
      with_src src (fun source ->
        source.seek srcpos >>= fun stream ->
        iter stream noop)
  | `Url dsturl ->
      with_src src (put ?metafn dsturl ~srcpos)
  | `Sink (sink:sink) ->
      sink 0L >>= fun out ->
      with_src src (fun source ->
        source.seek srcpos >>= fun stream ->
        iter stream out
      );;

  let get_meta (`Url src) =
    (ops_of_url src).get_meta src

  let copy ?metafn src ~srcpos dst = match src, dst with
  | (`Url srcurl), (`Url dsturl) -> (* TODO: check that scheme/hosts match *)
      (ops_of_url dsturl).copy_same srcurl dsturl >>= (function
      | true -> return ()
      | false -> generic_copy ?metafn src ~srcpos dst)
  | (`Url _ | `Source _), (`Null | `Url _ | `Sink _) ->
      generic_copy ?metafn src ~srcpos dst;;

  module type SchemeOps = sig
    type state
    val scheme : string
    val syntax: Neturl.url_syntax

    val init: unit -> unit
    val token_of_user: Neturl.url -> string option M.t
    val check: Neturl.url -> string option M.t
    val open_source: Neturl.url -> (entry * state) M.t
    val seek: state -> int64 -> unit M.t
    val read: state -> output_data M.t
    val close_source : state -> unit M.t

    (* true: optimized copy if scheme and authority matches
     * false: fallback to generic copy *)
    val copy_same: Neturl.url -> Neturl.url -> bool M.t

    val get_meta: Neturl.url -> (string*string) list M.t
    val put: ?metafn:metafn -> source -> int64 -> Neturl.url -> unit M.t
    val delete: Neturl.url -> unit M.t
    val create: ?metafn:metafn -> ?replica:int -> Neturl.url -> unit M.t

    val exists: Neturl.url -> int64 option M.t
    val fold_list: Neturl.url ->
        ('a -> entry -> 'a t) -> (string -> bool) -> 'a -> 'a M.t
  end
  let try_finally fn_try fn_finally value =
    try_catch
      fn_try
      (fun e ->
        (* run finally, ignoring any exceptions, and reraise original *)
        try_catch fn_finally (fun _ -> return ()) value >>= fun () ->
        fail e) value >>= fun result ->
      fn_finally value >>= fun () ->
      return result
  ;;
  
  module RegisterURLScheme(O: SchemeOps) = struct
    let readurl state () = O.read state
    let seekurl state pos =
      O.seek state pos >>= fun () ->
      return (readurl state);;

  let withurl url f =
      O.open_source url >>= fun (entry, state) ->
      try_finally
        (fun () ->
          f { meta = entry; seek = seekurl state }
        )
        (fun () ->
          O.close_source state
        ) ();;

    let rec read_urls current_source current_urls () =
      match !current_source with
      | Some state ->
          O.read state >>= fun (str, pos, len) ->
          if len = 0 then begin
            O.close_source state >>= fun () ->
            current_source := None;
            read_urls current_source current_urls ()
          end else
            return (str, pos, len)
      | None ->
          match !current_urls with
          | [] -> return ("", 0, 0)
          | url :: tl ->
            current_urls := tl;
            O.open_source url >>= fun (entry, state) ->
            O.seek state 0L >>= fun () ->
            current_source := Some state;
            read_urls current_source current_urls ()

    let withurls urls filesize f =
      let current_source = ref None in
      let current_urls = ref urls in
      f {
        meta = { name = ""; size = filesize; mtime = 0. };
        seek = (fun pos ->
          if pos <> 0L then
            fail (Failure "Non-seekable stream")
          else
            return (read_urls current_source current_urls)
        )
      }

    let ops = {
      with_url_source = withurl;
      with_urls_source = withurls;
      fold_list = O.fold_list;
      token_of_user = O.token_of_user;
      create = O.create;
      exists = O.exists;
      check = O.check;
      delete = O.delete;
      copy_same = O.copy_same;
      put = O.put;
      get_meta = O.get_meta;
    }

    let register () =
      O.init ();
      Hashtbl.add scheme_ops O.scheme ops;
      Hashtbl.add schemes O.scheme O.syntax;;
  end

  let sxio_printer = function
  | Unix.Unix_error(code,fn,arg) ->
      Some (Printf.sprintf "%s(%s): %s" fn arg
        (Unix.error_message code))
  | Detail (e, lst) ->
      let buf = Buffer.create 128 in
      Buffer.add_string buf (Printexc.to_string e);
      Buffer.add_char buf '\n';
      List.iter (fun (k,v) ->
        Buffer.add_char buf '\t';
        Buffer.add_string buf k;
        Buffer.add_string buf " -> ";
        Buffer.add_string buf v;
        Buffer.add_char buf '\n') lst;
      Some (Buffer.contents buf)
  | _ ->
      None;;

  let () =
    Printexc.register_printer sxio_printer
end
