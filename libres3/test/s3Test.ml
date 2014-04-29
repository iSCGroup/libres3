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

open HttpTest
open OUnit
open TestUtil

let slash_re = Netstring_str.regexp "%2F"

let encode s =
  let r = Netencoding.Url.encode ~plus:true s in
  Netstring_str.global_replace slash_re "/" r;;

let std_headers = ["Server"]
let std_headers_opt = ["x-amz-request-id";"x-amz-id-2"]

let string_of_attrs lst =
  String.concat " " (List.rev_map (fun ((ns,name),value) ->
    (if ns <> "" then ns ^ ":" ^ name
    else name)^"="^value)
  lst);;

let string_of_xmllist lst =
  String.concat "\n" (List.rev (List.rev_map CodedIO.Xml.to_string lst));;

let rec get_xml_field exp_tag = function
  | `El (((_,tag),[]),[`Data d]) when tag = exp_tag ->
      Some d
  | `El (_,c) ->
      List.fold_left (fun accum e ->
        match get_xml_field exp_tag e with
        | None -> accum
        | Some d -> Some d) None c
  | `Data _ ->
      None;;

let match_field exp_ns exp_tag exp_data lst =
  let matched = ref false in
  List.iter (function
    | `El (((ns, tag),attrs),children) when tag = exp_tag ->
        assert_eq_string ~msg:"tag namespace" ns exp_ns;
        assert_eq_string ~msg:"tag attributes" "" (string_of_attrs attrs);
        begin match children with
        | [] -> assert_failure ("empty tag contents, expected "^exp_data)
        | `Data d :: [] ->
            assert_eq_string ~msg:"data" exp_data d
        | xml_list ->
            assert_failure ("bad tag contents: " ^ (string_of_xmllist xml_list))
        end;
        matched := true
    | _ ->
        ()
  ) lst;
  assert_bool ("expected tag " ^ exp_tag ^ " in: " ^ (string_of_xmllist lst)) (!matched);;

let expect_xml_error ?(resource=true) desc path body =
 try begin match CodedIO.Xml.parse_string body with
  | `El (((_,"Error"),[]), children) ->
      match_field "" "Code" desc children;
      if not resource then
        assert_bool "no resource" (List.for_all (fun c ->
          (get_xml_field "Resource" c) = None) children);
      true
  | `El _ | `Data _ ->
      false
  end
 with Xmlm.Error ((line,col), err) ->
   assert_failure (Printf.sprintf "Xml error %s at %d:%d"
    (Xmlm.error_message err) line col)

let expect_error ?(head=false) errcode path =
  let desc, _, status = Error.info errcode in
  let code = Nethttp.int_of_http_status status in
  {
  headers_exact = [];
  headers_present = std_headers;
  headers_present_opt = std_headers_opt;
  check_body =
    if head then (function "" -> true | _ -> false)
    else expect_xml_error ~resource:(errcode <> Error.AccessDenied) desc path;
  expected_code = code;
  is_head = head
}

let key_id = Config.key_id
let secret_access_key = Config.secret_access_key

let map_method = function
  | (`GET | `HEAD | `DELETE) as a -> a
  | `POST ->
      `POST ()
  | `PUT ->
      `PUT ()
  | _ -> `UNSUPPORTED

let format_date_header t =
  Netdate.mk_mail_date t;;

let siglog = open_out "sigs.log"

let sign_request req =
  let date = format_date_header (Unix.gettimeofday ()) in
  let meth = map_method req.meth in
  let h = ("Date", date) :: req.req_headers in
  let canon = CanonRequest.canonicalize_request ~id:(RequestId.generate()) meth
    {CanonRequest.req_headers = ("Authorization","") :: h; CanonRequest.undecoded_url = req.relative_url} in
  let tosign = CanonRequest.string_to_sign canon in
  let signature = Cryptoutil.sign_str (!secret_access_key) tosign in
  Printf.fprintf siglog "URL:%s\nStringToSignBytes:%s\nSignature:%s\n\n"
    req.relative_url (Cryptoutil.to_hex tosign) signature;
  let auth = Printf.sprintf "AWS %s:%s" !key_id signature in
  { req with
    req_headers =
      ("Authorization", auth) ::
      ("Content-Length", string_of_int (String.length req.req_body)) ::
      h
  }

let check_xml_root ~ns_root ~tag_root = function
  | `El (((ns,tag),_),_) ->
      assert_eq_string ~msg:"root tag" tag_root tag;
      assert_eq_string ~msg:"root tag namespace" ns_root ns;
      true
  | _ ->
      false;;

let expect_xml_root ns_root tag_root body =
  check_xml_root ~ns_root ~tag_root (CodedIO.Xml.parse_string body);;

let expect_xml_status_root status ns tag = {
  headers_exact = ["Content-Type","application/xml"];
  headers_present = std_headers;
  headers_present_opt = std_headers_opt;
  check_body = expect_xml_root ns tag;
  expected_code = Nethttp.int_of_http_status status;
  is_head = false
}

let rec check_xml_children child_tag child_contents xml =
  match xml with
  | `El (((_,tag),_),c) when tag = child_tag ->
      begin match c with
      | (`Data d) :: _ when child_contents = d ->
          true
      | [] when child_contents = "" ->
          true
      | _ ->
          false
      end
  | `El (_,c) ->
      List.exists (check_xml_children child_tag child_contents) c
  | `Data _ ->
      false;;

let rec check_no_tag child_tag xml =
  match xml with
  | `El (((_,tag),_),c) ->
      if tag = child_tag then false
      else List.for_all (check_no_tag child_tag) c
  | `Data _ ->
      true;;

let expect_xml_status_root_child status
  ~ns_root ~tag_root ~child_tag ~child_contents = {
    headers_exact = ["Content-Type","application/xml"];
    headers_present = std_headers;
    headers_present_opt = std_headers_opt;
    check_body = (fun body ->
      let xml = CodedIO.Xml.parse_string body in
      (check_xml_root ~ns_root ~tag_root xml) &&
      (check_xml_children child_tag child_contents xml));
    expected_code = Nethttp.int_of_http_status status;
    is_head = false
}

let expect_xml_status_root_nochild status
  ~ns_root ~tag_root ~child_tag = {
    headers_exact = ["Content-Type","application/xml"];
    headers_present = std_headers;
    headers_present_opt = std_headers_opt;
    check_body = (fun body ->
      let xml = CodedIO.Xml.parse_string body in
      (check_xml_root ~ns_root ~tag_root xml) &&
      (check_no_tag child_tag xml));
    expected_code = Nethttp.int_of_http_status status;
    is_head = false
}

let generate_block size =
  let s = Int64.to_int size in
  let prng = Cryptokit.Random.pseudo_rng "1234567890123456" in
  let str = String.make s '0' in
  prng#random_bytes str 0 s;
  str;;
