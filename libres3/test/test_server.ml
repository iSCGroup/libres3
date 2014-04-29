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

open OUnit
open CanonRequest

type methods = [ `DELETE | `GET | `HEAD | `POST | `PUT | `UNSUPPORTED ]
type request_test = {
  name: string;
  (* input *)
  headers: (string * string) list;
  req_method: methods;
  orig_url: string;
  (* expected output *)
  expected_tosign: string;
  expected_valid: bool;
  expected_bucket: string;
  expected_path: string;
}

let assert_str_equal ?msg expected actual =
  assert_equal ?msg ~printer:(fun s -> s) expected actual;;

let map_method = function
  | `DELETE | `GET | `HEAD | `UNSUPPORTED as m -> m
  | `POST ->
     `POST ()
  | `PUT ->
      `PUT ()

let test_request_parse_sign data =
  data.name>::(fun () ->
    let meth = map_method data.req_method in
    let canon_req =
      canonicalize_request ~id:(RequestId.generate ()) meth {
        req_headers = data.headers;
        undecoded_url = data.orig_url
      } in
    assert_str_equal ~msg:"bucket" data.expected_bucket (Bucket.to_string canon_req.bucket);
    let tosign = string_to_sign canon_req in
    assert_str_equal ~msg:"string-to-sign" data.expected_tosign tosign;
    let valid = validate canon_req tosign = (Error.NoError, []) in
    assert_equal ~msg:"signature" data.expected_valid valid;
    assert_str_equal ~msg:"path" data.expected_path canon_req.path;
  );;

let request_data = [
  {
    name = "Object GET";
    headers = [
      "Host","johnsmith.s3.amazonaws.com";
      "Date","Tue, 27 Mar 2007 19:36:42 +0000";
      "Authorization","AWS AKIAIOSFODNN7EXAMPLE:bWq2s1WEIj+Ydj0vQ697zp+IXMU="
    ];
    req_method = `GET;
    orig_url = "/photos/puppy.jpg";
    expected_tosign = "GET\n\n\nTue, 27 Mar 2007 19:36:42 +0000\n/johnsmith/photos/puppy.jpg";
    expected_valid = true;
    expected_bucket = "johnsmith";
    expected_path = "/photos/puppy.jpg";
  };
  {
    name = "Object PUT";
    headers = [
      "Content-Type","image/jpeg";
      "Content-Length", "94328";
      "Host","johnsmith.s3.amazonaws.com";
      "Date","Tue, 27 Mar 2007 21:15:45 +0000";
      "Authorization","AWS AKIAIOSFODNN7EXAMPLE:MyyxeRY7whkBe+bq8fHCL/2kKUg="
    ];
    req_method = `PUT;
    orig_url = "/photos/puppy.jpg";
    expected_tosign = "PUT\n\nimage/jpeg\nTue, 27 Mar 2007 21:15:45 +0000\n/johnsmith/photos/puppy.jpg";
    expected_valid = true;
    expected_bucket = "johnsmith";
    expected_path = "/photos/puppy.jpg";
  };
  {
    name = "List";
    headers = [
      "User-Agent","Mozilla/5.0";
      "Host","johnsmith.s3.amazonaws.com";
      "Date","Tue, 27 Mar 2007 19:42:41 +0000";
      "Authorization","AWS AKIAIOSFODNN7EXAMPLE:htDYFYduRNen8P9ZfE/s9SuKy0U="
    ];
    req_method = `GET;
    orig_url = "/?prefix=photos&max-keys=50&marker=puppy";
    expected_tosign = "GET\n\n\nTue, 27 Mar 2007 19:42:41 +0000\n/johnsmith/";
    expected_valid = true;
    expected_bucket = "johnsmith";
    expected_path = "/"
  };
  {
    name = "Fetch";
    headers = [
      "Host","johnsmith.s3.amazonaws.com";
      "Date","Tue, 27 Mar 2007 19:44:46 +0000";
      "Authorization","AWS AKIAIOSFODNN7EXAMPLE:c2WLPFtWHVgbEmeEG93a4cG37dM="
    ];
    req_method = `GET;
    orig_url = "/?acl";
    expected_tosign = "GET\n\n\nTue, 27 Mar 2007 19:44:46 +0000\n/johnsmith/?acl";
    expected_valid = true;
    expected_bucket = "johnsmith";
    expected_path = "/"
  };
  {
    name = "Delete";
    headers = [
      "User-Agent","dotnet";
      "Host","s3.amazonaws.com";
      "Date","Tue, 27 Mar 2007 21:20:27 +0000";
      "x-amz-date","Tue, 27 Mar 2007 21:20:26 +0000";
      "Authorization","AWS AKIAIOSFODNN7EXAMPLE:R4dJ53KECjStyBO5iTBJZ4XVOaI="
    ];
    req_method = `DELETE;
    orig_url = "/johnsmith/photos/puppy.jpg";
    (* Example in REST API signing docs are wrong: there have to be 4 \n not 3
     * *)
    expected_tosign = "DELETE\n\n\n\nx-amz-date:Tue, 27 Mar 2007 21:20:26 +0000\n/johnsmith/photos/puppy.jpg";
    expected_valid = true;
    expected_bucket = "johnsmith";
    expected_path = "/photos/puppy.jpg"
  };
  {
    name = "Upload";
    headers = [
      "User-Agent","curl/7.15.5";
      "Host","static.johnsmith.net:8080";
      "Date","Tue, 27 Mar 2007 21:06:08 +0000";
      "x-amz-acl","public-read";
      "content-type","application/x-download";
      "Content-MD5","4gJE4saaMU4BqNR0kLY+lw==";
      "X-Amz-Meta-ReviewedBy", "joe@johnsmith.net";
      "X-Amz-Meta-ReviewedBy", "jane@johnsmith.net";
      "X-Amz-Meta-FileChecksum", "0x02661779";
      "X-Amz-Meta-ChecksumAlgorithm","crc32";
      "Content-Disposition","attachment; filename=database.dat";
      "Content-Encoding","gzip";
      "Content-Length","5913339";
      "Authorization","AWS AKIAIOSFODNN7EXAMPLE:ilyl83RwaSoYIEdixDQcA4OnAnc="
    ];
    req_method = `PUT;
    orig_url = "/db-backup.dat.gz";
    expected_tosign = "PUT\n4gJE4saaMU4BqNR0kLY+lw==\napplication/x-download\nTue, 27 Mar 2007 21:06:08 +0000\nx-amz-acl:public-read\nx-amz-meta-checksumalgorithm:crc32\nx-amz-meta-filechecksum:0x02661779\nx-amz-meta-reviewedby:joe@johnsmith.net,jane@johnsmith.net\n/static.johnsmith.net/db-backup.dat.gz";
    expected_valid = true;
    expected_bucket = "static.johnsmith.net";
    expected_path = "/db-backup.dat.gz"
  };
  {
    name = "ListAllMyBuckets";
    headers = [
      "Host","s3.amazonaws.com";
      "Date","Wed, 28 Mar 2007 01:29:59 +0000";
      "Authorization","AWS AKIAIOSFODNN7EXAMPLE:qGdzdERIC03wnaRNKh6OqZehG9s="
    ];
    req_method = `GET;
    orig_url = "/";
    expected_tosign = "GET\n\n\nWed, 28 Mar 2007 01:29:59 +0000\n/";
    expected_valid = true;
    expected_bucket = "";
    expected_path = "/"
  };
  {
    name = "UnicodeKeys";
    headers = [
      "Host","s3.amazonaws.com";
      "Date","Wed, 28 Mar 2007 01:49:49 +0000";
      "Authorization","AWS AKIAIOSFODNN7EXAMPLE:DNEZGsoieTZ92F3bUfSPQcbGmlM="
    ];
    req_method = `GET;
    orig_url = "/dictionary/fran%C3%A7ais/pr%c3%a9f%c3%a8re";
    expected_tosign = "GET\n\n\nWed, 28 Mar 2007 01:49:49 +0000\n/dictionary/fran%C3%A7ais/pr%c3%a9f%c3%a8re";
    expected_valid = true;
    expected_bucket = "dictionary";
    expected_path = "/français/préfère";
  }
]

let suite =
  "Request">::: [
    "signing">:::
      List.map test_request_parse_sign request_data;
  ]
;;

let _ =
  Config.key_id := "AKIAIOSFODNN7EXAMPLE";
  Config.secret_access_key := "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY";
  Config.base_hostname := "s3.amazonaws.com";
  run_test_tt_main suite
;;
