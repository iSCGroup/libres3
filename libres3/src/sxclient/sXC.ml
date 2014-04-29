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

open Sigs
open Http
open Neturl
(* TODO: use token_of_user in make_request and write wrapper without *)
let syntax = {
  Neturl.null_url_syntax with
  url_enable_user = Url_part_required;
  url_enable_scheme = Url_part_required;
  url_enable_host = Url_part_required;
  url_enable_port = Url_part_allowed;
  url_enable_path = Url_part_allowed;
  url_accepts_8bits = false;
  url_is_valid = (fun _ -> true);
  url_enable_relative = true;
}

module Make
  (M:Sigs.Monad)
  (OS:EventIO.OSMonad with type 'a t = 'a M.t)
  (W: Sigs.ThreadMonad with type 'a t = 'a M.t)
  : SXIO.Make(M).SchemeOps =
struct
  (* TODO: implement tmpfile buffer here *)
  module IO = EventIO.Make(M)(OS)
  module XIO = SXIO.Make(M)

  module P = Http.MakePipeline(IO.Op)(W)

  (* max upload chunk size, for now we use this as
   * the multipart part-size too *)
  let chunk_size = 4 * 1024 * 1024
  let download_max_blocks = 30 (* TODO: keep in sync with include/default.h *)
  let multipart_threshold = 132 * 1024 * 1024 (* 132 MB, should be multiple of
  chunk_size *)
  let last_threshold = Int64.of_int chunk_size

  let pipe = ref None
  let init () =
    Printf.printf "sx:// protocol registered\n";
    pipe := Some (P.start_pipeline ());;

  let pipeline () =
    match !pipe with
    | None -> failwith "Pipeline not initialized"
    | Some p -> p;;

  let scheme = "sx"
  let syntax = syntax
  open M
  let rec foldl base volume f lst accum =
    match lst with
    | hd :: tl ->
        f accum {
          XIO.name =
            "/" ^ (Netencoding.Url.encode volume) ^ "/" ^ hd.Sigs.name;
          XIO.size = hd.Sigs.size;
          XIO.mtime = hd.Sigs.mtime
        } >>= foldl base volume f tl
    | [] ->
        return accum;;

  module AsyncJson (M: Sigs.Monad) = struct
    type json_element = [`Bool of bool | `Float of float | `Null | `String of string]
    type json_container = [`Array | `Object | `Field of string]
    type 'a json_mapper = {
      start: 'a -> json_container -> 'a * 'a json_mapper;
      stop: parent:'a -> children:'a -> json_container -> 'a;
      element: 'a -> json_element -> 'a
    }
    type range = (int * int) * (int * int);;

    exception ParseError of (range * string);;
    exception Error of range * Jsonm.error;;

    type 'a json_state = {
      d: Jsonm.decoder;
      input: unit -> (string * int * int) M.t;
      mutable parents: (json_container * 'a * 'a json_mapper) list;
      mutable state: 'a;
      mutable mapper: 'a json_mapper;
    }

    open M
    type decoded = [`End | `Error of Jsonm.error | `Lexeme of Jsonm.lexeme]

    let rec decode s =
      match Jsonm.decode s.d with
      | `Await ->
          s.input () >>= fun (str, pos, len) ->
          Jsonm.Manual.src s.d str pos len;
          decode s
      | #decoded as v ->
          return v
    ;;

    let json_fail s msg =
      let r = Jsonm.decoded_range s.d in
      let (l1, c1), (l2, c2) = r in
      Printf.eprintf "Json parse error: %d:%d-%d:%d:%s\n%!" l1 c1 l2 c2 msg;
      fail (ParseError (r, msg))
    ;;
    (* run one fold step *)
    let start s v =
      try
        let state, mapper = s.mapper.start s.state v in
        s.parents <- (v, s.state, s.mapper) :: s.parents;
        s.state <- state;
        s.mapper <- mapper;
        return false
      with Failure msg ->
        json_fail s msg
    ;;

    let stop s parent parent_mapper v rest =
      s.mapper <- parent_mapper;
      s.parents <- rest;
      s.state <- s.mapper.stop ~parent ~children:s.state v;
    ;;

    let stop_field s =
      match s.parents with
      | (`Field _ as f, parent, parent_mapper) :: rest ->
          stop s parent parent_mapper f rest;
          false
      | [] ->
          true (* EOF *)
      | _ ->
          false
    ;;

    let stop_container s v =
      match s.parents with
      | (parent_type, parent, parent_mapper) :: rest->
          if parent_type = v then begin
            try
              stop s parent parent_mapper v rest;
              return (stop_field s)
            with Failure msg ->
              json_fail s msg
          end else
            json_fail s "Array/Object end mismatch"
      | [] ->
          json_fail s "Array/Object end without start"
      ;;

    let fold_step_signal s v =
      match v with
      | `Lexeme `As ->
          start s `Array
      | `Lexeme `Ae ->
          stop_container s `Array;
      | `Lexeme `Os ->
          start s `Object
      | `Lexeme `Oe ->
          stop_container s `Object;
      | `Lexeme (`Name n) ->
          start s (`Field n)
      | `Lexeme (#json_element as v) ->
          begin try
            s.state <- s.mapper.element s.state v;
            return (stop_field s)
          with Failure msg ->
            json_fail s msg
          end
      | `End ->
          begin match s.parents with
          | [] -> return true (* EOF *)
          | _ -> json_fail s "Premature EOF"
          end
      | `Error err ->
          Jsonm.pp_error Format.str_formatter err;
          json_fail s (Format.flush_str_formatter ())
    ;;

    let fold_init input state mapper = {
      d = Jsonm.decoder `Manual;
      input = input;
      parents = [];
      state = state;
      mapper = mapper
    };;

    let fold_step s =
      decode s >>= fold_step_signal s
    ;;

    let rec fold s =
      fold_step s >>= function
      | true -> return s.state
      | false -> fold s
    ;;

    type json = [`A of json list | `O of json list | `F of string * json list |
                json_element]

    open Printf
    let rec pp_json = function
      | `A l ->
          printf "array (\n";
          List.iter pp_json l;
          printf ")\n";
      | `O l ->
          printf "object (\n";
          List.iter pp_json l;
          printf ")\n";
      | `F (n, l) ->
          printf "field %s:(\n" n;
          List.iter pp_json l;
          printf ")";
      | `Bool b ->
          printf "bool: %b\n" b
      | `String s ->
          printf "string: %s\n" s
      | `Float f ->
          printf "number: %g\n" f
      | `Null ->
          printf "null\n"
    ;;

    let rec json_parser = {
      start = (fun state _ -> [], json_parser);
      stop = (fun ~parent ~children v ->
        match v with
        | `Array -> (`A (List.rev children)) :: parent
        | `Object -> (`O (List.rev children)) :: parent
        | `Field name -> (`F (name, children)) :: parent
      );
      element = (fun state v ->
        (v :> json) :: state);
    };;

    let rec read_eof input =
      input () >>= fun (_,_,len) ->
      if len = 0 then return ()
      else read_eof input;;

    let rec json_parse_tree input =
      fold (fold_init input [] json_parser) >>= fun result ->
      read_eof input >>= fun () ->
      return result
    ;;
  end

  let format_date_header t =
    Netdate.format "%a, %d %b %Y %H:%M:%S GMT" (Netdate.create t)
  ;;

  open Cryptokit
  let string_of_method = function
    | `DELETE -> "DELETE"
    | `GET -> "GET"
    | `HEAD -> "HEAD"
    | `POST -> "POST"
    | `PUT -> "PUT"
    | _ -> "N/A";;

  (* TODO: allow setting this via url's user/password param,
   * and pass it from the server *)
  let base64_encode s =
    (* Base64.encode_compact_pad is not available in older cryptokit releases *)
    let e = transform_string (Base64.encode_compact ()) s in
    let padding = match (String.length e) mod 4 with
    | 3 -> 1
    | 2 -> 2
    | _ -> 0 in
    e ^ (String.make padding '=');;

  let sign_request token r =
    if token = "" then
      failwith "SX token is not set";
    let date = format_date_header (Unix.gettimeofday ()) in
    let headers = ("Date", date) :: r.req_headers in
    let d = transform_string (Base64.decode ()) token in
    let i = String.sub d 0 20
    and k = String.sub d 20 20 in
    let hmac = MAC.hmac_sha1 k in
    let buf = Buffer.create 128 in
    Buffer.add_string buf  (string_of_method r.meth);
    Buffer.add_char buf  '\n';
    let n = String.length r.relative_url in
    let s = String.sub r.relative_url 1 (n-1) in
    Buffer.add_string buf s;
    Buffer.add_char buf  '\n';
    Buffer.add_string buf  date;
    Buffer.add_char buf  '\n';
    let sha1 = hash_string (Hash.sha1 ()) r.req_body in
    let sha1_hex = transform_string (Hexa.encode ()) sha1 in
    Buffer.add_string buf sha1_hex;
    Buffer.add_char buf  '\n';
    let signed = (hash_string hmac (Buffer.contents buf)) in
    let a = i ^ signed ^ (String.sub d 40 2) in
    let auth = "SKY " ^ (transform_string (Base64.encode_compact ()) a) in
    { r with
      req_headers = ("Authorization", auth) :: headers
    }

    open IO.Op
    module Json = AsyncJson(IO.Op)
    open Json

    let finish t =
      match !t with
      | Some p ->
          P.stop_pipeline p;
          t := None
      | None -> ();;

    type 'a source = 'a IO.Source.t
    type reader = unit -> (string * int * int) M.t
    type file_meta = {
      filename: string;
      mutable blocksize: int option;
      mutable filesize: int64 option;
      mutable hashaddrs: (string * string) list;
      mutable sizeremaining : int64;
      http_port: int;
    }

    let file_empty filename port ={
      filename = filename;
      blocksize = None;
      filesize = None;
      hashaddrs = [];
      sizeremaining = 0L;
      http_port = port;
    }


    let check_reply reply =
      let c = reply.code in
      if c >= 400 then
        fail (Failure (Printf.sprintf "SX replied with code %d" c))
      else
        return ()
    ;;

    let expect_field_number s f = {
      start = (fun _ _ -> failwith "field value expected");
      stop = (fun ~parent ~children _ -> failwith "field value expected");
      element = (fun s -> function
        | `Float flt -> s >>= (fun state -> f flt state; s)
        | _ -> failwith "number expected")
    }

    let empty_list _ = []
    let get_blocksize s = match s.blocksize with
    | None ->
        failwith "no blockSize"
    | Some b -> b

    let chain ~parent ~children _ =
      parent >>= fun _ ->
      children
    ;;

(*    let download_one_hash origurl host port hash blocksize =
      let p = pipeline () in
      P.make_cached_request p ~key:hash (
          sign_request (token_of_user origurl) {
            meth = `GET;
            host = host;
            port = port;
            relative_url = Printf.sprintf "/.data/%d/%s" blocksize hash;
            req_headers = [];
            req_body = "";
          });;*)

    let download_hash_nodes hash =
      {
      start = (fun _ -> failwith "string expected");
      stop = chain;
      element = (fun s -> function
      | `String host ->
          s >>= fun state ->
          state.hashaddrs <- (host, hash) :: state.hashaddrs;
          return state
      | _ ->
          failwith "string expected"
        )
    };;

    let download_hash_nodelist hash = {
      start = (fun s -> function
      | `Array ->
          s, download_hash_nodes hash
      | `Object | `Field _ ->
          failwith "json array expected"
        );
      stop = chain;
      element = (fun _ _ -> failwith "json array expected")
    }
    ;;

    let download_hash_top = {
      start = (fun s -> function
      | `Field hash ->
          s, download_hash_nodelist hash
      | `Object | `Array ->
          failwith "json field expected"
      );
      stop = chain;
      element = (fun _ _ -> failwith "json field expected")
    }
    ;;

    let hashlist_mapper_obj = {
      start = (fun s -> function
        | `Object ->
            s, download_hash_top
        | `Array | `Field _ ->
            failwith "json object expected"
      );
      stop = chain;
      element = (fun _ _ -> failwith "json object expected");
    }

    let hashlist_mapper_array = {
      start = (fun s -> function
        | `Array ->
            s, hashlist_mapper_obj
        | `Object | `Field _ ->
            failwith "json array expected"
      );
      stop = (fun ~parent ~children _ -> children);
      element = (fun _ _ -> failwith "json array expected")
    }

    let rec ignore_value = {
      start = (fun s _ -> s, ignore_value);
      stop = (fun ~parent ~children _ -> parent);
      element = (fun s _ -> s)
    }

    let file_mapper = {
      start = (fun s -> function
        | `Array | `Object ->
           failwith "json field expected"
        | `Field "fileSize" ->
            s, expect_field_number s (fun f s -> s.filesize <- Some (Int64.of_float f))
        | `Field "blockSize" ->
            s, expect_field_number s (fun f s -> s.blocksize <- Some (int_of_float f))
        | `Field "fileData" ->
            s, hashlist_mapper_array
        | `Field _ ->
            s, ignore_value
      );
      stop = chain;
      element = (fun _ _ ->
        failwith "Field expected")
    }

    let file_meta_mapper = {
      start = (fun f -> function
        | `Object -> f, file_mapper
        | `Array | `Field _ ->
            (* TODO: json_fail *)
            failwith "json object expected"
        );
      stop = (fun ~parent ~children _ -> children);
      element = (fun _ _ ->
        failwith "json object expected")
    }

    let rec parse_hashes file state =
      fold_step state >>= function
      | false ->
          parse_hashes file state
      | true ->
          file.hashaddrs <- List.rev file.hashaddrs;
          return ()
      ;;

(*    let retrieve origurl file () =
      match file.hashaddrs with
      | (host, hash) :: rest ->
          file.hashaddrs <- rest;
          download_one_hash origurl host file.http_port hash (get_blocksize file) >>= fun str ->
          let len =
            Int64.to_int (
              min (Int64.of_int (String.length str)) file.sizeremaining
            ) in
          if len = 0 then
            return ("",0,0)
          else begin
            file.sizeremaining <-
              Int64.sub file.sizeremaining (Int64.of_int len);
            return (str, 0,len)
          end
      | [] ->
          if file.sizeremaining > 0L then
            fail (Failure (Printf.sprintf "We think we are at EOF, but file
              not fully sent: %Ld" file.sizeremaining))
          else
              return ("",0,0)
    ;;*)

    exception SXProto of string
    let expect_content_type reply ct =
      try
        let server = Nethttp.Header.get_server reply.headers in
        let expected = "Skylable" in
        if (String.length server < String.length expected ||
            String.sub server 0 (String.length expected) <> expected) then
              fail (Http_client.Http_protocol
                (SXProto (Printf.sprintf "Not an SX server: %s" server)))
        else
          try
            let actual, _ = reply.headers#content_type () in
            if actual <> ct then
              fail (SXProto (Printf.sprintf
                "Bad content-type: %s, expected: %s" actual ct))
            else
              return ()
          with Not_found ->
          fail (SXProto (Printf.sprintf "No Content-Type: header in reply!"))
      with Not_found ->
        fail (SXProto (Printf.sprintf "No Server: header in reply!"))
    ;;

(*    let rec retrieve_fileandsize origurl file state mtime =
      match file.filesize with
      | None ->
          begin fold_step state >>= function
          | true ->
             fail (Failure "premature EOF on json stream (no filesize)")
          | false ->
              retrieve_fileandsize origurl file state mtime
          end
      | Some size ->
          file.sizeremaining <- size;
          parse_hashes file state >>= fun () ->
          return ((retrieve origurl file),{
            name = file.filename;
            size = size;
            mtime = mtime
          });;*)

    let http_syntax = Hashtbl.find Neturl.common_url_syntax "http"
    let locate url =
      Neturl.modify_url url
        ~encoded:true
        ~query:"o=locate&volumeMeta"
        ~scheme:"http"
        ~syntax:http_syntax;;

    let fetch_nodes url =
      Neturl.modify_url url
        ~encoded:true
        ~path:[""]
        ~query:"nodeList&clusterLimits"
        ~scheme:"http"
        ~syntax:http_syntax;;

    let url_port_opt url =
      try url_port url
      with Not_found ->
        if !Config.sx_ssl then 443 else 80

    let delay ms =
      OS.sleep (ms /. 1000.)

    let request_of_url ~token meth ?(req_body="") url =
      let syntax = url_syntax_of_url url in
      let relative_url =
        Neturl.remove_from_url
          ~scheme:true ~user:true ~user_param:true ~password:true
          ~host:true ~port:true
          (Neturl.modify_url ~syntax:(Neturl.partial_url_syntax syntax) url) in
      sign_request token {
      meth = meth;
      host = url_host url;
      port = url_port_opt url;
      relative_url = string_of_url relative_url;
      req_headers = [];
      req_body = req_body;
    }

    let filter_field field =
      List.filter (function
        | `F (name,_) when name = field -> true
        | _ -> false
        );;

    let has_field field =
      List.exists (function
        | `F (name, _) when name = field -> true
        | _ -> false)
    ;;

    let filter_field_one field lst =
      match filter_field field lst with
      | `F (_,[value]) :: [] ->
         value
      | [] ->
         List.iter pp_json lst;
         failwith ("bad volume list format, no filelist: " ^ field)
      | _ ->
         List.iter pp_json lst;
         failwith "bad volume list format, multiple filelists"
    ;;

    let detail_of_reply reply =
      try_catch (fun () ->
        expect_content_type reply "application/json" >>= fun () ->
        json_parse_tree (P.input_of_async_channel reply.body) >>= function
          | [`O obj] ->
              begin match filter_field "ErrorMessage" obj with
              | `F (_,[`String value]) :: [] ->
                  return value
              | _ ->
                  return ""
              end
          | _ -> return ""
        )
        (fun e ->
          return ("Unparsable reply" ^ (Printexc.to_string e))) ();;

    let rec make_request_token ~token meth ?(req_body="") url =
      (* TODO: check that scheme is sx! *)
      let p = pipeline () in
      P.make_http_request p (request_of_url ~token meth ~req_body url) >>= fun reply ->
      if reply.status = `Ok || reply.status = `Partial_content then
        return reply
      else if reply.code = 429 then
        (* TODO: next in list, better interval formula *)
        delay 20. >>= fun () ->
        make_request_token ~token meth ~req_body url
      else
        let url = Neturl.modify_url ~scheme:"http" url in
        detail_of_reply reply >>= fun detail ->
        let code = match reply.status with
        | `Not_found | `Bad_request (* SX bug: returns 400 instead of 404 *) ->
          Some Unix.ENOENT
        | `Unauthorized | `Forbidden -> Some Unix.EACCES
        | `Conflict ->
              Some (if meth=`DELETE then Unix.ENOTEMPTY else Unix.EEXIST)
        | `Request_uri_too_long -> Some Unix.ENAMETOOLONG
        | `Requested_range_not_satisfiable -> Some Unix.EINVAL
        | _ -> None in
        let details = [
          "SXErrorMessage",detail;
          "SXHttpCode", string_of_int reply.code
        ] in
        match code with
        | Some c ->
            fail (SXIO.Detail(
              Unix.Unix_error(c,string_of_method meth,string_of_url url),
              details))
        | None ->
          fail (SXIO.Detail (
            Failure ((string_of_method meth) ^ " " ^ (string_of_url url)),
            details));;

    let filter_field_int field lst =
      match filter_field_one field lst with
      | `Float f ->
          (* TODO: check that it doesn't have fractional part *)
          Int64.of_float f
      | _ ->
          failwith "numeric field expected";;

    let filter_field_number field lst =
      match filter_field_one field lst with
      | `Float f -> f
      | _ ->
          failwith "numeric field expected";;

    let filter_field_string field lst =
      match filter_field_one field lst with
      | `String f -> f
      | _ ->
          failwith "string field expected";;

  module AJson = AsyncJson(IO.Op)
  module UserCache = Cache.Make(M)(struct
    type t = string
    let compare a b = String.compare a b
    type data = string option
    let cache_size = 10
  end)

  let token_of_user url =
    let p = pipeline () in
    let user = Neturl.url_user url in
    let url = Neturl.remove_from_url ~query:true (Neturl.modify_url
      ~path:["";".users";user] url ~scheme:"http") in
    try_catch (fun () ->
        UserCache.bind (M.return user) (fun _ ->
            make_request_token ~token:!Config.secret_access_key `GET url >>= fun reply ->
            json_parse_tree (P.input_of_async_channel reply.body) >>= function
            | [`O obj] ->
              let key = filter_field_string "userKey" obj in
              let b64 = Base64.encode_compact () in
              let auth_uid = hash_string (Hash.sha1 ()) user in
              b64#put_string auth_uid;
              b64#put_string (transform_string (Hexa.decode ()) key);
              b64#put_string "\x00\x00";
              b64#finish;
              M.return (Some b64#get_string)
            | lst ->
              List.iter AJson.pp_json lst;
              failwith "bad user info json"
          )
      ) (function
        | SXIO.Detail(Unix.Unix_error(Unix.ENOENT, _,_), _) ->
            M.return None
        | e -> M.fail e
      ) ()

    let choose_error = function
      | e :: _ ->
          M.fail e
      | [] ->
          failwith "empty error list"

    let rec make_request_loop meth ?req_body nodes url errors = match nodes with
    | node :: rest ->
      try_catch (fun () ->
          let url = Neturl.modify_url ~host:node url in
          token_of_user url >>= function
          | Some token ->
            make_request_token ~token meth ?req_body url
          | None ->
            (* no such user *)
            let user = Neturl.url_user url in
            M.fail (SXIO.Detail(Unix.Unix_error(Unix.EPERM,"", user),[
                "SXErrorMessage",Printf.sprintf "Cannot retrieve token for user %S"
                  user
              ]))
      ) (fun e ->
        make_request_loop meth ?req_body rest url (e :: errors)
      ) ()
    | [] ->
        choose_error errors

    let make_request meth ?req_body nodes url =
      try_catch (fun () ->
        make_request_loop meth ?req_body nodes url []
      ) (fun _ ->
        make_request_loop meth ?req_body nodes url []
      ) ()

    (* TODO: full DNS list? *)
    let initial_nodelist url = [ Neturl.url_host url ]

    let get_nodelist url =
      match url_path ~encoded:true url with
      | "" :: volume :: path ->
          let url = Neturl.modify_url url ~path:["";volume] in
          let nodes = initial_nodelist url in
          make_request `GET nodes (locate url) >>= (fun reply ->
          json_parse_tree (P.input_of_async_channel reply.body) >>= function
          | [`O [`F ("nodeList", [`A nodes]); `F ("volumeMeta", [`O metalist])]] ->
            if has_field "filterActive" metalist then
              fail (SXIO.Detail(
                Unix.Unix_error(Unix.EACCES, volume, "Volume has filters"),
                ["LibreS3ErrorMessage","Cannot access a volume that has filters"]
              ))
            else
              return (List.rev_map
                (function
                  | `String h -> h
                  | _ -> failwith "bad locate nodes format"
                ) nodes)
          | lst ->
              List.iter pp_json lst;
              failwith "bad locate nodes json"
          )
      | _ ->
          (* cannot download the volume or the root *)
          fail (Unix.Unix_error(Unix.EISDIR,"get",(string_of_url url)));;

    let remove_obj = function
      | `O [one] -> one
      | p ->
          AJson.pp_json p;
          failwith "Bad json reply format: obj with one field (hash) expected"
    ;;

    module StringMap = Map.Make(String)
    let process_batch_reply hashes replybody blocksize map =
      snd (List.fold_left (fun (pos, accum) hash ->
        (pos + blocksize, StringMap.add hash (replybody, pos) accum)
      ) (0,map) hashes)
    ;;

    module StringSet = Set.Make(String)
    module NodesMap = Map.Make(StringSet)

    let map_unique_nodes nodes =
      List.fold_left
        (fun accum -> function
        | `String host ->
          StringSet.add host accum
        | _ -> failwith "Bad json nodes format"
        ) StringSet.empty nodes
    ;;

    let add_to_nodemap key value map =
      try
        NodesMap.add key (value :: (NodesMap.find key map)) map
      with Not_found ->
        NodesMap.add key [value] map
    ;;

    let group_by_node hashes =
      List.fold_left
      (fun accum -> function
      | `F (hash, [`A nodes]) ->
        add_to_nodemap (map_unique_nodes nodes) hash accum
      | _ -> failwith "Bad json hash format"
      ) NodesMap.empty hashes
    ;;

    let rec split_at_threshold bs limit listin listout current amount =
      match listin with
      | [] ->
          (* the actual order of hashes inside each batch is reversed,
           * but fold_upload reverses it again so upload
           * will happen in ascending offset order to optimize FS reads *)
          List.rev_append listout [current]
      | hd :: tail ->
        let next = amount + bs in
        if (next <= limit) then
          split_at_threshold bs limit tail listout (hd :: current) next
        else
          split_at_threshold bs limit tail (current :: listout) [hd] bs
    ;;


    (* download a part of the file fully into memory.
     * this is needed because we need to send data in stream order,
     * but we have to download them in different order to use batching and
     * multiple hosts *)
    let download_full_mem url blocksize hashes =
      let p = pipeline () in
      let grouped = group_by_node hashes in
      (* launch all requests *)
      let batches = NodesMap.fold (fun nodes hashes accum ->
        let split = split_at_threshold 1 download_max_blocks (List.rev hashes) [] [] 0 in
        let nodelist = StringSet.elements nodes in
        List.fold_left (fun accum hashesr ->
          let hashes = List.rev hashesr in
          let batch = (String.concat "" hashes) in
          let u = Neturl.modify_url url
            ~path:["";".data";string_of_int blocksize;batch] in
          (hashes, make_request `GET nodelist u) :: accum
        ) accum split
      ) grouped [] in
      (* build map of replys *)
      List.fold_left (fun accum (hashes, request) ->
        accum >>= fun map ->
        (request >>= fun reply ->
        return (process_batch_reply hashes reply.body blocksize map))
      ) (return StringMap.empty) batches >>= fun hashmap ->
      let batch = String.make (blocksize * (List.length hashes)) 'X' in
      let _ = List.fold_left (fun pos hf ->
        match hf with
        | `F (hash, _ ) ->
          let str, strpos = StringMap.find hash hashmap in
          String.blit str strpos batch pos blocksize;
          pos + blocksize
        | _ -> failwith "bad json hash format"
      ) 0 hashes in
      return batch
    ;;

    let get_meta url =
     let port = url_port_opt url in
     get_nodelist url >>= fun nodes ->
     let url = Neturl.modify_url ~syntax:http_syntax url ~query:"fileMeta" in
     make_request `GET nodes url >>= fun reply ->
     expect_content_type reply "application/json" >>= fun () ->
     let input = P.input_of_async_channel reply.body in
     AJson.json_parse_tree input >>= function
     | [`O [`F ("fileMeta", [`O metalist])]] ->
      return (List.rev_map (function
        | `F (k, [`String v]) -> k, transform_string (Hexa.decode()) v
        | _ -> failwith "bad meta reply format"
      ) metalist)
     | _ -> failwith "bad meta reply format"

    let get url =
     let port = url_port_opt url in
     get_nodelist url >>= fun nodes ->
     try_catch (fun () ->
       make_request `GET nodes url >>= fun reply ->
       try
         let mtime = Nethttp.Header.get_last_modified reply.headers in
         expect_content_type reply "application/json" >>= fun () ->
         let input = P.input_of_async_channel reply.body in
         AJson.json_parse_tree input >>= function
         | [`O obj] ->
            let filesize = filter_field_int "fileSize" obj
            and blocksize = Int64.to_int (filter_field_int "blockSize" obj) in
            begin match filter_field_one "fileData" obj with
            | `A a ->
                let hashes = List.rev (List.rev_map remove_obj a) in
                (* split after DOWNLOAD_MAX_BLOCKS hashes *)
                let split_map = ref (split_at_threshold blocksize (blocksize * download_max_blocks) hashes [] [] 0) in
                let remaining = ref filesize in
                return ((fun () ->
                  match !split_map with
                  | hd :: tl ->
                      split_map := tl;
                      download_full_mem url blocksize (List.rev hd) >>= fun reply ->
                      let n = String.length reply in
                      let len = min (Int64.of_int n) !remaining in
                      remaining := Int64.sub !remaining len;
                      return (reply, 0, Int64.to_int len)
                  | [] ->
                    return ("",0,0);
                  ), {
                    name = "";
                    size = filesize;
                    mtime = mtime;
                })
            | p ->
             AJson.pp_json p;
             fail (Failure "Bad json reply format: array expected")
            end
         | p ->
             List.iter AJson.pp_json p;
             fail (Failure "Bad json reply format: object expected")
       with Not_found ->
         fail (Failure ("Last-Modified missing in SX reply: "
          ^ (string_of_url  url)))
      )
      (function
        | SXIO.Detail(Unix.Unix_error(Unix.ENOENT,_,_) as e ,_) ->
            fail e
        | e -> fail e) ();;

    let remove_leading_slash name =
      let n = String.length name in
      if n > 0 && name.[0] = '/' then
        String.sub name 1 (n-1)
      else
        name

    let parse_file = function
    | `F (name,[`O meta]) ->
        {
          name = remove_leading_slash name;
          size = (match filter_field_one "fileSize" meta with
            | `Float f -> Int64.of_float f
            | _ -> failwith "bad filesize format");
          mtime = (match filter_field_one "createdAt" meta with
          | `Float f -> f
          | _ -> failwith "bad mtime format")
        }
    | p ->
        pp_json p;
        failwith "bad filelist format"
    ;;

    let listit url =
      get_nodelist url >>= fun nodes ->
      make_request `GET nodes url >>=  fun reply ->
      begin
       expect_content_type reply "application/json" >>= fun () ->
       let input = P.input_of_async_channel reply.body in
       json_parse_tree input >>= function
       | [`O obj] ->
          (* TODO: stream parse instead *)
          begin match filter_field_one "fileList" obj with
          | `O files ->
            return (List.rev_map parse_file files)
          | p ->
            pp_json p;
            fail (Failure "bad volume list format")
          end
       | p ->
           List.iter pp_json p;
           fail (Failure "bad volume list format2")
     end

    let parse_volume = function
    | `F (name,[`O _meta]) ->
        name
    | p ->
        pp_json p;
        failwith "bad volumeslist format"
    ;;

    let volumelist url =
      make_request `GET (initial_nodelist url) url >>=  fun reply ->
      begin
       expect_content_type reply "application/json" >>= fun () ->
       let input = P.input_of_async_channel reply.body in
       json_parse_tree input >>= function
       | [`O obj] ->
          begin match filter_field_one "volumeList" obj with
          | `O files ->
            return (List.rev_map parse_volume files)
          | p ->
            pp_json p;
            fail (Failure "bad volumes list format")
          end
       | p ->
           List.iter pp_json p;
           fail (Failure "bad volumes list format2")
     end


    type json = [ `Array of json list | `Bool of bool | `Float of float
                | `Null | `Object of json_mem list | `String of string]
    and json_mem = string * json

    let rec print_json e = function
    | `Array a ->
        ignore (Jsonm.encode e (`Lexeme `As));
        List.iter (print_json e) a;
        ignore (Jsonm.encode e (`Lexeme `Ae));
    | `Object o ->
        ignore (Jsonm.encode e (`Lexeme `Os));
        List.iter (print_json_mem e) o;
        ignore (Jsonm.encode e (`Lexeme `Oe));
    | `Null | `Bool  _ | `Float _ | `String _ as l ->
        ignore (Jsonm.encode e (`Lexeme l))

    and print_json_mem e (n, v) =
      ignore (Jsonm.encode e (`Lexeme (`Name n)));
      print_json e v;;

    type err = [`Error | `ParseError];;
    type range = (int * int) * (int * int);;

    exception ParseError of (range * string);;
    exception Error of range * Jsonm.error;;

    let rec parse_json_val d = function
    | `Lexeme `As ->
        `Array (parse_json_arr d [])
    | `Lexeme `Os ->
        `Object (parse_json_obj d [])
    | `Lexeme (`Null | `Bool _ | `Float _ | `String _ as v) ->
        v
    | `Lexeme (`Ae | `Oe | `Name _) ->
        raise (ParseError (Jsonm.decoded_range d, "Premature array/object end"))
    | `Error err ->
        raise (Error (Jsonm.decoded_range d, err))
    | `Await | `End -> assert false

    and parse_json_obj d lst = match Jsonm.decode d with
    | `Lexeme (`Name n) ->
        parse_json_obj d ((n, parse_json_val d (Jsonm.decode d)) :: lst)
    | `Lexeme `Oe ->
        List.rev lst
    | v ->
        parse_json_obj d (("", parse_json_val d v) :: lst)

    and parse_json_arr d lst = match Jsonm.decode d with
    | `Lexeme `Ae ->
        List.rev lst
    | v ->
        parse_json_arr d ((parse_json_val d v) :: lst);;

    let parse_json d =
      parse_json_val d (Jsonm.decode d);;

    let send_json nodelist url json =
      let b = Buffer.create 4096 in
      let encoder = Jsonm.encoder (`Buffer b) in
      print_json encoder json;
      ignore (Jsonm.encode encoder `End);
      make_request `PUT ~req_body:(Buffer.contents b) nodelist url;;

    type estate = EOF | PartialBlock of int | FullBlock

    type buf = {
      buf: string;
      mutable str: string;
      mutable pos: int;
      mutable n: int
    }

    let rec read_block stream buf pos size =
      if size > 0 then
        let read = if buf.pos + buf.n <= String.length buf.str && buf.n > 0 then
            return ()
          else begin
            stream () >>= fun (src, srcpos, n) ->
            buf.str <- src;
            buf.pos <- srcpos;
            buf.n <- n;
            return ()
          end in
        read >>= fun () ->
        let n = min buf.n size in
        String.blit buf.str buf.pos buf.buf pos n;
        buf.pos <- buf.pos + n;
        buf.n <- buf.n - n;
        if n = 0 then
          if pos = 0 then
            return EOF
          else
            return (PartialBlock pos)
        else
          read_block stream buf (pos + n) (size - n)
      else
        return FullBlock
      ;;

    (* TODO: these would belong in eventIO *)
    let rec really_read fd buf pos n =
      if n > 0 then
        OS.read fd buf pos n >>= fun amount ->
        if amount = 0 then return pos
        else really_read fd buf (pos + amount) (n - amount)
    else return pos

    let rec really_write out str pos len =
      if len <= 0 then return ()
      else
        OS.write out str pos len >>= fun amount ->
        really_write out str (pos + amount) (len - amount);;


    let rec compute_hashes_loop uuid tmpfd stream buf blocksize lst map pos stop =
      if pos >= stop then return (lst, map, stop)
      else begin
      String.fill buf.buf 0 (String.length buf.buf) '\x00';
      read_block stream buf 0 blocksize >>= fun status ->
      really_write tmpfd buf.buf 0 (String.length buf.buf) >>= fun () ->
      if status = EOF then return (lst, map, pos)
      else
        let h = Hash.sha1 () in
        h#add_string uuid;
        let sha1 = hash_string h buf.buf in
        let sha1_hex = transform_string (Hexa.encode ()) sha1 in
        let nextmap = StringMap.add sha1_hex pos map in
        let nextlst = sha1_hex :: lst in
        let nextpos = Int64.add pos (Int64.of_int blocksize) in
        match status with
        | FullBlock ->
          compute_hashes_loop uuid tmpfd stream buf blocksize nextlst nextmap nextpos stop
        | PartialBlock len ->
          return (nextlst, nextmap, Int64.add pos (Int64.of_int len)) (* it was a partial block, we're done *)
        | _ -> assert false
        end
    ;;

    let fold_upload user port (offset, tmpfd) map token blocksize nodes hashes previous =
      previous >>= fun () ->
      let buf = {
        buf = String.make (blocksize * (List.length hashes)) '\x00';
        str = ""; pos = 0; n = 0
      } in
      (*  TODO: chop into 4MB chunks *)
      List.fold_left (fun prev hash ->
        prev >>= fun pos ->
        try
          buf.pos <- 0;
          buf.n <- 0;
          let seekpos = StringMap.find hash map in
(*          Printf.printf "Uploading hash %s from offset %Ld\n" hash seekpos;*)
          let offset = Int64.sub seekpos offset in
          OS.LargeFile.lseek tmpfd offset Unix.SEEK_SET >>= fun _ ->
          String.fill buf.buf 0 (String.length buf.buf);
          really_read tmpfd buf.buf pos blocksize >>= function
          | 0 -> fail (Failure "eof when trying to read hash")
          | _ ->
              return (pos + blocksize)
        with Not_found ->
          StringMap.iter (fun k v -> Printf.printf "%s -> %Ld\n%!" k v) map;
          fail (Failure ("hash not found:" ^ hash))
      ) (return 0) hashes >>= fun _ ->
      (* TODO: retry on failure *)
      let nodelist = StringSet.elements nodes in
      let url =
        Neturl.make_url ~encoded:false
          ~scheme:"sx"
          ~port ~path:["";".data";string_of_int blocksize;token]
          ~user ~host:(List.hd nodelist)
          http_syntax
      in
      make_request `PUT ~req_body:buf.buf nodelist url >>= fun _ ->
      return ()
    ;;

    let check_reply reply =
      let c = reply.code in
      if c >= 400 then
        fail (Failure (Printf.sprintf "SX replied with code %d" c))
      else
        return ()
    ;;

    let rec job_poll origurl url expected_id interval max_interval =
      delay interval >>= fun () ->
      make_request `GET [Neturl.url_host url] url >>= fun reply ->
      AJson.json_parse_tree (P.input_of_async_channel reply.body) >>= function
        | [`O obj] ->
            let requestid = filter_field_string "requestId" obj
            and status = filter_field_string "requestStatus" obj
            and msg = filter_field_string "requestMessage" obj in
            if requestid <> expected_id then
              fail (SXIO.Detail(
                Failure "Job id mismatch",
                ["SXErrorMessage",msg;"SXJobStatus",status;
                "ExpectedId",expected_id;"ActualId",requestid]))
            else begin match status with
            | "PENDING" ->
              (* TODO: investigate other formulas, for now we use libsx formula *)
              let interval = min (interval *. 2.0) max_interval in
              job_poll origurl url expected_id interval max_interval
            | "OK" ->
              return ()
            | "ERROR" ->
              let e = match msg with
              | "Volume already exists" ->
                  Unix.Unix_error(Unix.EEXIST,"PUT",string_of_url origurl)
              | _ ->
                Failure ("Operation failed: " ^ msg) in
              fail (SXIO.Detail(e, ["SXErrorMessage", msg;"SXHttpCode","200"]))
            | _ ->
              fail (SXIO.Detail(
                Failure (Printf.sprintf "Invalid request status %s: %s" status msg),
                ["SXErrorMessage", msg;"SXHttpCode","200"]))
            end
        | p ->
              List.iter AJson.pp_json p;
              fail (Failure "Bad json reply format: object expected")
    ;;

    let job_get url reply =
      check_reply reply >>= fun () ->
      AJson.json_parse_tree (P.input_of_async_channel reply.body) >>= function
        | [`O obj] ->
            let requestid = filter_field_string "requestId" obj
            and minPoll = filter_field_number "minPollInterval" obj
            and maxPoll = filter_field_number "maxPollInterval" obj in
            let pollurl = Neturl.modify_url
              ~path:["";".results";requestid] url in
            job_poll url pollurl requestid minPoll maxPoll
        | p ->
              List.iter AJson.pp_json p;
              fail (Failure "Bad json reply format: object expected")
    ;;

    let flush_token url token =
      let url = Neturl.modify_url url
        ~path:["";".upload";token] ~encoded:false in
      make_request `PUT [Neturl.url_host url] url >>=
      job_get url

    open Printf
    let rec pp_json = function
      | `Array l ->
          printf "array (\n";
          List.iter pp_json l;
          printf ")\n";
      | `Object l ->
          printf "object (\n";
          List.iter pp_json l;
          printf ")\n";
      | `Field (n, l) ->
          printf "field %s:(\n" n;
          List.iter pp_json l;
          printf ")";
      | `Bool b ->
          printf "bool: %b\n" b
      | `String s ->
          printf "string: %s\n" s
      | `Float f ->
          printf "number: %g\n" f
      | `Null ->
          printf "null\n"
    ;;

    let parse_server server =
      try
        Scanf.sscanf server "Skylable/%_s (%s@)" (fun s -> s)
      with e ->
        failwith ("Bad servername " ^ server ^ ":" ^ (Printexc.to_string e))
    ;;

    let parse_sx_cluster server =
      try
        Scanf.sscanf server "%_s (%s@)" (fun s -> s)
      with e ->
        failwith ("Bad servername " ^ server ^ ":" ^ (Printexc.to_string e))
    ;;

    let locate_upload url size =
      let p = pipeline () in
      match url_path ~encoded:true url with
      | "" :: volume :: _ ->
        make_request `GET (initial_nodelist url) (Neturl.modify_url
            ~path:["";volume]
            ~scheme:"http"
            ~syntax:http_syntax
            ~encoded:true
            ~query:(Printf.sprintf "o=locate&volumeMeta&size=%Ld" size) url)
        >>= fun reply ->
      AJson.json_parse_tree (P.input_of_async_channel reply.body) >>= function
      | [`O [
          `F ("nodeList",[`A nodes]);
          `F ("blockSize",[`Float blocksize]);
          `F ("volumeMeta", [`O metalist])
        ]] ->
          if has_field "filterActive" metalist then
            fail (SXIO.Detail(
              Unix.Unix_error(Unix.EACCES, volume, "Volume has filters"),
              ["LibreS3ErrorMessage","Cannot access a volume that has filters"]
            ))
          else let nodes =
            List.rev_map (function
              | `String h -> h
              | _ -> failwith "bad locate  nodes format"
              ) nodes
          in
          let uuid =
            try parse_sx_cluster (reply.headers#field "SX-Cluster")
            with Not_found -> parse_server (reply.headers#field "Server")
          in
          return (uuid, nodes, int_of_float blocksize)
      | p ->
          List.iter AJson.pp_json p;
          fail (Failure "bad json locate format")
    ;;

    let upload_batch user port source map token blocksize upload_map () =
      let grouped_hashes = group_by_node upload_map in
      NodesMap.fold
        (fold_upload user port source map token blocksize)
        grouped_hashes (return ())
    ;;

    let build_meta = function
      | None -> []
      | Some f ->
          ["fileMeta", `Object (
            List.rev_map (fun (k,v) ->
              k, `String (transform_string (Hexa.encode ()) v)
            ) (f ())
          )]

    let upload_part ?metafn nodelist url source blocksize hashes map size extendSeq =
      let obj = List.rev_append [
        "fileData", `Array hashes;
        if extendSeq > 0L then
          "extendSeq", `Float (Int64.to_float extendSeq)
        else
          "fileSize", `Float (Int64.to_float size);
      ] (build_meta metafn) in
      send_json nodelist url (`Object obj) >>= fun reply ->
      check_reply reply >>= fun () ->
      AJson.json_parse_tree (P.input_of_async_channel reply.body) >>= function
      | [`O [
        `F ("uploadToken",[`String token]);
        `F ("uploadData",[`O upload_map])
        ]] ->
        Printf.printf "offering %d hashes, requested %d hashes\n%!"
          (List.length hashes) (List.length upload_map);
        let split_map = split_at_threshold blocksize chunk_size upload_map [] [] 0 in
        List.fold_left
          (fun accum umap ->
            let user = Neturl.url_user url in
            accum >>= upload_batch user (url_port_opt url) source map token blocksize umap)
          (return ()) split_map >>= fun () ->
        return (reply.req_host, token)
      | p ->
        List.iter AJson.pp_json p;
        fail (Failure "Bad json reply format")
    ;;

    (* !! partial blocks can only be sent in the final upload request,
     * because otherwise auto-bs would choose the wrong bs
     * when uploading the partial blocks.
     * partial blocks = stuff under auto-bs threshold
     *)
    let rec upload_chunks ?metafn buf tmpfd nodes url size uuid stream blocksize pos token =
      let endpos = min (Int64.add pos (Int64.of_int multipart_threshold)) size in
      let end_threshold = Int64.sub size last_threshold in
      let endpos =
        (* upload the very last chunk of the file separately:
         * first time: pos < end_threshold -> end_threshold
         * second time: pos = end_threshold -> endpos *)
        if end_threshold > 0L && pos < end_threshold then end_threshold
        else endpos in
      let bs64 = Int64.of_int blocksize in
      let extendseq = Int64.div (Int64.add pos (Int64.sub bs64 1L)) bs64 in
      if pos = endpos && token <> "" then begin
        (* all multiparts uploaded, flush token *)
        flush_token url token
      end else begin
        (* still have parts to upload *)
        compute_hashes_loop uuid tmpfd stream buf blocksize [] StringMap.empty pos endpos
        >>= fun (hashes_rev, map, _) ->
        let hashes = List.rev_map (fun h -> `String h) hashes_rev in
        let n = List.length hashes_rev in
        (* TODO: multiple hosts and retry *)
        let chunksize = Int64.of_int (blocksize * n) in
        let expected_bytes = Int64.to_int (Int64.sub endpos pos) in
        let expected = (expected_bytes + blocksize - 1) / blocksize in
        if (n <> expected) then
          fail (Failure (Printf.sprintf "Bad hash counts: %d != %d; pos: %Ld, bytes: %d"
            n expected pos expected_bytes))
        else
          let url = if extendseq > 0L then
            Neturl.modify_url url ~path:["";".upload";token] ~encoded:false
          else url in
          OS.LargeFile.lseek tmpfd 0L Unix.SEEK_SET >>= fun _ ->
          let metafn_final = if endpos = size then metafn else None in
          upload_part ?metafn:metafn_final nodes url (pos, tmpfd) blocksize hashes map size extendseq >>= fun (host, token) ->
          let url = Neturl.modify_url ~host url in
          OS.LargeFile.lseek tmpfd 0L Unix.SEEK_SET >>= fun _ ->
          upload_chunks ?metafn buf tmpfd [host] url size uuid stream blocksize endpos token
      end
    ;;

    (* TODO: create a temporary dir, and create all tmpfiles there *)
    let rng =
      Random.pseudo_rng (Printf.sprintf "%g%04d%04d"
        (Unix.gettimeofday ()) (Unix.getpid ()) (Unix.getppid ()))

    let tempfilename () =
      let rand = Random.string rng 16 in
      let hex = transform_string (Hexa.encode ()) rand in
      Filename.concat Filename.temp_dir_name
        (Printf.sprintf "libres3_upload_%s.tmp" hex)

    let put ?metafn src srcpos url =
      let host = url_host url
      and port = url_port_opt url in
      let size = Int64.sub (src.XIO.meta.XIO.size)  srcpos in
      if size < 0L then
        fail (Failure "Source position beyond EOF")
      else match url_path ~encoded:true url with
      | "" :: volume :: path as split ->
        (* TODO: handle no such volume errors *)
        locate_upload url size >>= fun (uuid, nodes, blocksize) ->
        let dst = join_path split in
        let url = Neturl.modify_url ~host url in
        let tmpfile = tempfilename () in
        src.XIO.seek 0L >>= fun stream ->
        (* TODO: better error handling *)
          OS.openfile tmpfile [Unix.O_RDWR; Unix.O_CREAT; Unix.O_EXCL] 0o600 >>= fun tmpfd ->
        let buf = {
          buf = String.make blocksize '\x00';
          str = ""; pos = 0; n = 0 } in
        try_catch (fun () ->
          upload_chunks ?metafn buf tmpfd nodes url size uuid stream blocksize srcpos "" >>= fun () ->
          OS.close tmpfd >>= fun () ->
          OS.unlink tmpfile
        ) (fun e ->
          OS.close tmpfd >>= fun () ->
          OS.unlink tmpfile >>= fun () ->
          fail e) ()

  let fold_list url f recurse accum =
    let fullpath = url_path ~encoded:true url in
    match fullpath with
    | "" :: volume :: path ->
      let base = Neturl.modify_url url ~encoded:true ~path:["";volume] in
      let query = match path with
      | [] | [""] -> "recursive"
      | _ ->
          Printf.sprintf "recursive&filter=%s"
            ((join_path path) ^ "*") in
      let url = Neturl.modify_url url
        ~encoded:true ~scheme:"http" ~syntax:http_syntax
        ~path:["";volume] ~query in
      get_nodelist url >>= fun nodes ->
      (* TODO: support multiple nodes *)
      let node :: _ = nodes in
      let url = Neturl.modify_url ~host:node url in
      listit url >>= fun lst ->
      foldl base volume f lst accum
    | [""] | [] ->
        let base = Neturl.modify_url url
          ~encoded:true ~path:[""] ~scheme:"http" ~syntax:http_syntax
          ~query:"volumeList" in
        volumelist base >>= fun lst ->
        List.iter (fun dir ->
          ignore (recurse ("/" ^ dir))) lst;
        return accum
    | _ ->
        failwith "invalid URL";;

  type state = unit -> (string * int * int) M.t
  let open_source url =
    get url >|= fun (reader, entry) ->
    {
      XIO.name = Neturl.join_path (Neturl.url_path ~encoded:false url);
      size = entry.Sigs.size;
      mtime = entry.Sigs.mtime
    },
    reader

  let read reader = reader ()
  let seek _ pos =
    if pos = 0L then return ()
    else IO.fail (Failure "seeking not supported yet")
  let close_source _ =
    (* TODO: abort download*)
    return ()

  let copy_same src dst =
    (* TODO: optimize sx to sx copy if same host: copy just hashlist *)
    return false;;

  let exists url =
    let p = pipeline () in
    token_of_user url >>= function
    | None ->
      return None
    | Some token ->
      begin match url_path ~encoded:true url with
      | "" :: volume :: ("" :: [] | []) ->
          (* does volume exist? *)
          P.make_http_request p (request_of_url ~token `HEAD (locate url))
          >>= fun reply ->
          begin match reply.status with
          | `Ok -> return (Some 0L)
          | _ -> return None
          end
      | "" :: _volume :: _path ->
          (* does a file exist? *)
          open_source url >>= fun (meta, _) ->
          return (Some meta.XIO.size)
      | _ ->
          (* can I fetch the nodeslist? *)
          P.make_http_request p (request_of_url ~token `HEAD (fetch_nodes url))
          >>= fun reply ->
          begin match reply.status with
          | `Ok -> return (Some 0L)
          | _ -> return None
          end
      end

  let check url =
    try_catch (fun () ->
      make_request `GET (initial_nodelist url) (fetch_nodes url) >>= fun _ ->
      return None
    ) (function
      | SXIO.Detail(e, details) ->
        let msg = try List.assoc "SXErrorMessage" details with Not_found -> "" in
        return (Some (Printf.sprintf
          "Remote SX server reports: %s (%s)" msg (Printexc.to_string e))
        )
      | e -> fail e) ()

  let create ?metafn ?(replica=(!Config.replica_count)) url =
    match url_path url ~encoded:true with
    | "" :: volume :: ([""] | []) ->
      (* elevate to admin privileges for volume creation *)
      let owner = Neturl.url_user url in
      let url =
        if !Config.volume_create_elevate_to_admin then
          Neturl.modify_url
          ~path:["";volume] ~user:"admin" url
        else url in
      try_catch
        (fun () ->
          send_json (initial_nodelist url) url (`Object [
            (* max size allowed in json is 2^53, in SX it is 2^50*)
            "volumeSize", `Float !Config.volume_size;
            "owner", `String owner;
            "replicaCount", `Float (float_of_int replica)
          ]) >>=
          job_get url
        )
        (function
          | SXIO.Detail (Unix.Unix_error(Unix.EEXIST,_,_) as e, _) ->
              fail e
          | e -> fail e
        ) ()
    | path ->
        let rec last l = match l with
        | _ :: [tl] -> tl
        | _ :: rest -> last rest in
        let path =
          if (last path) = "" then
            path @ ["."] (* can't create files with a trailing slash *)
          else path in
        let url = Neturl.modify_url ~encoded:true ~path url in
        let `Source src = XIO.of_string "" in
        put ?metafn src 0L url;;

  let delete url =
    get_nodelist url >>= fun nodes ->
    let url =
      match url_path url ~encoded:true with
      | ["";volume;""] ->
          Neturl.modify_url ~path:["";volume] url
      | _ -> url in
    try_catch
      (fun () ->
        make_request `DELETE nodes url >>= fun _ ->
        return ()
      )
      (function
       | SXIO.Detail (Unix.Unix_error((Unix.ENOENT|Unix.ENOTEMPTY),_,_) as e, _) ->
         fail e
       | e -> fail e)
      ();;
end
