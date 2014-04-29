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

open Netstring_str

type ('a,'b) methods = [ `DELETE | `GET | `HEAD | `POST of 'a | `PUT of 'b | `UNSUPPORTED ]
type header = string * string
type request_info = {
  req_headers: header list;
  undecoded_url: string;
}

module StringMap = Map.Make(String)
type ('a,'b) t = {
  req_method: ('a,'b) methods;
  bucket: Bucket.t;
  path: string;
  orig_uri: string;
  content_md5: string;
  content_type: string;
  date: float;
  date_header: string;
  host: string;
  expires: string;
  undecoded_uri_path: string;
  headers: Headers.t;
  query_params: string StringMap.t;
  id: RequestId.t;
  user : string;
}


let x_amz_re = regexp_case_fold "x-amz-.*"

let string_of_method = function
  | `DELETE -> "DELETE"
  | `GET -> "GET"
  | `HEAD -> "HEAD"
  | `POST _ -> "POST"
  | `PUT _ -> "PUT"
  | _ -> "N/A";;

let canonicalized_amz_headers c =
  let sorted_lowercased_amz = Headers.filter_names (fun name ->
    (* TODO: its already lowercase, use starts_with *)
    (string_match x_amz_re name 0) <> None
  ) c.headers in
  (* TODO: replace folding white-space with single-space? *)
  let combined =
    List.map (fun name ->
      name ^ ":" ^ (String.concat "," (Headers.field_values c.headers name)) ^ "\n"
    ) sorted_lowercased_amz in
  String.concat "" combined;;

let prefix_bucket b =
  let s = Bucket.to_string b in
  if s = "" then ""
  else "/" ^ s;;

let stringmap_all map =
  (* can't use Map.bindings, because OCaml 3.11 doesn't have it *)
  StringMap.fold (fun key data accum ->
    (key, data) :: accum
  ) map [];;

let compare_nameval (name1,_) (name2,_) = String.compare name1 name2
let canonicalized_resource c =
  (* TODO: this is an approximation, do we need the real
   * un-decoded URI? *)
  (* TODO: strip bucket if it starts with it, and stop at '?'*)
  let filtered_subresources = List.filter (fun (n,_) ->
    (* TODO: would it be more efficient to take the difference
     * of two maps than this linear comparison?*)
    n = "acl" || n = "lifecycle" || n = "location" || n = "logging" ||
    n = "notification" || n = "partNumber" || n = "policy" ||
    n = "requestPayment" || n = "torrent" || n = "uploadId" ||
    n = "uploads" || n = "versionId" || n = "versions" ||
    n = "website") (stringmap_all c.query_params) in
  let sorted_subresources = List.fast_sort compare_nameval filtered_subresources
  in
  let subresources =
    if sorted_subresources = [] then "" else
      "?" ^ (String.concat "&" (
              List.map (fun (n,v) ->
                if v = "" then n else n ^ "=" ^ v
              ) sorted_subresources
            )) in
  (prefix_bucket c.bucket) ^ c.undecoded_uri_path ^ subresources

let split_query_re = regexp "[&;]"
let split_param_re = regexp "="

let parse_query encoded_query =
  try
    let params = Netstring_str.split split_query_re encoded_query in
    List.fold_left (fun accum nameval ->
      let name, value =
        match Netstring_str.split split_param_re nameval with
        | name :: value :: [] ->
            Netencoding.Url.decode name, Netencoding.Url.decode value
        | name :: [] ->
            Netencoding.Url.decode name, ""
        | _ ->
            raise (Error.ErrorReply(Error.InvalidURI,["querypart",nameval],[]))
      in
      StringMap.add name value accum) StringMap.empty params
  with Not_found ->
    StringMap.empty;;

let base_syntax =
  { (Hashtbl.find Neturl.common_url_syntax "http")
    with Neturl.url_accepts_8bits = true
  };;

let get_query_param_opt q param =
  try
    StringMap.find param q
  with Not_found -> ""
;;

let header_overrides = ["x-amz-date","Date"]

let canonicalize_request ~id req_method
  {req_headers=req_headers; undecoded_url=undecoded_url} =
  let headers = Headers.build header_overrides req_headers in
  let host = Headers.field_single_value headers "host" !Config.base_hostname in
  let hurl = Neturl.parse_url ~base_syntax (
    "http://" ^ host) in
  (* client may not encode some characters that OCamlnet would reject as unsafe,
   * for example [.
   * fixup_url_string doesn't help with [ either.
   * Do the safe thing: decode the URL path, and encode it again *)
  let bucket, undecoded_uri_path = Bucket.from_url hurl undecoded_url in
  let decoded = Neturl.split_path (Netencoding.Url.decode undecoded_uri_path) in
  let absolute_url = Neturl.modify_url hurl ~path:decoded ~encoded:false in
  let query =
    try
      let qpos = String.index undecoded_url '?' in
      let len = (String.length undecoded_url) - qpos - 1 in
      if len > 0 then
        String.sub undecoded_url (qpos+1) len
      else ""
    with Not_found -> "" in
  let query_params = parse_query query in
  let lpath = Neturl.join_path (Neturl.url_path ~encoded:false absolute_url) in
  let path = if lpath = "" then "/" else lpath in
  {
    req_method = req_method;
    content_md5 = Headers.field_single_value headers "Content-MD5" "";
    content_type = Headers.field_single_value headers "Content-Type" "";
    date = Headers.get_date headers;
    date_header = Headers.orig_field_value headers "Date";
    expires = get_query_param_opt query_params "Expires";
    orig_uri = undecoded_url;
    host = host;
    bucket = bucket;
    headers = headers;
    path = path;
    undecoded_uri_path = undecoded_uri_path;
    query_params = query_params;
    id = id;
    user = ""
  };;

let string_to_sign canon_req =
  let b = Buffer.create Config.small_buffer_size in
  let add s =
    Buffer.add_string b s;
    Buffer.add_char b '\n' in
  add (string_of_method canon_req.req_method);
  add canon_req.content_md5;
  add canon_req.content_type;
  if Headers.has_header canon_req.headers "authorization" then
    add canon_req.date_header
  else
    add canon_req.expires;
  Buffer.add_string b (canonicalized_amz_headers canon_req);
  Buffer.add_string b (canonicalized_resource canon_req);
  Buffer.contents b;;

type auth_header =
  | AuthNone | AuthMalformed of string | AuthDuplicate | Authorization of string * string

let parse_authorization req =
  match Headers.field_values req.headers "authorization" with
  | [] ->
      let keyid = get_query_param_opt req.query_params "AWSAccessKeyId"
      and signature = get_query_param_opt req.query_params "Signature"
      and expires = req.expires in
      (* TODO: check Expires and raise ExpiredToken *)
      if keyid = "" || signature = "" || expires = "" then
        AuthNone
      else
        Authorization (keyid, signature)
  | auth :: [] ->
      begin try
        Scanf.sscanf auth "AWS %s@:%s" (fun a b -> Authorization (a,b))
      with
      | Scanf.Scan_failure s | Failure s ->
          AuthMalformed s
      | End_of_file -> AuthMalformed "too short"
      end
  | _ ->
      AuthDuplicate
;;

(* TODO: remove this *)
(* TODO: we should call the sign method from here, as we only know
 * access key id here *)
let validate_authorization req string_to_sign expected_signature =
  match parse_authorization req with
  | AuthNone ->
      Error.AccessDenied, ["MissingHeader", "Authorization"]
  | AuthMalformed s ->
      Error.InvalidSecurity, ["BadAuthorization", s]
  | AuthDuplicate ->
      Error.InvalidSecurity,
      ["BadAuthorization", "Multiple occurences of Authorization header"]
  | Authorization (key, signature) ->
      if key <> !Config.key_id then
        Error.InvalidAccessKeyId, [
          "Hint","For libres3 the key id must always be:" ^ !Config.key_id]
      else if signature <> expected_signature then
        Error.SignatureDoesNotMatch, [
          ("StringToSign", string_to_sign);
          ("Host", req.host);
          ("UndecodedPath", req.undecoded_uri_path);
          ("Bucket", Bucket.to_string req.bucket);
          ("Hint", "Your S3 secret key should be set to the SX access key")
        ]
      else
        Error.NoError, []
  ;;

let validate req tosign =
  let signature = Cryptoutil.sign_str !Config.secret_access_key tosign in
  validate_authorization req tosign signature;;

let buf = Buffer.create 128
let gen_debug ~canon =
  Buffer.reset buf;
  Buffer.add_string buf (Bucket.to_string canon.bucket);
  Buffer.add_char buf '\x00';
  Buffer.add_string buf canon.path;
  Buffer.add_char buf '\x00';
  (* todo: string-to-sign, auth id, etc. *)
  Buffer.add_string buf (string_of_float (Unix.gettimeofday ()));
  Cryptoutil.base64_encode (Buffer.contents buf);;

let gen_debug2 info =
  Cryptoutil.base64_encode (info.undecoded_url);;

(* query params without the optional signature parameters *)
let actual_query_params r =
  List.fast_sort compare_nameval (
    List.filter (fun (name,_) ->
      not (name = "AWSAccessKeyId" || name = "Signature" || name = "Expires")
    ) (stringmap_all r.query_params)
  );;
