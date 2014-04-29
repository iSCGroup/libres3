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

open S3Test
open HttpTest
open TestUtil
open OUnit

(* TODO:
  *  multipart abort test
  *  multipart list parts
  *  multipart complete with incomplete parts
  *  multipart complete with wrong etags
  *  multipart with part copy
  *  multipart put to nonexistent bucket should be refused at start
  *   multipart part uploads/complete must use same name as initial
  *  put copy
  *  get range
  *  head!
  *  options!
  *  corner-cases for auth: duplicate Date/Authorization headers,
  *  invalid format for Authorization header
  *)

let bucketname = Printf.sprintf "vtest%f" (Unix.gettimeofday ())
let bucketname2 = Printf.sprintf "vtest_enoent%f" (Unix.gettimeofday ())
let filename2 = Filename.concat (Filename.concat "/" bucketname) "testfile2"
let file_name_3 = "test file 3 "
let filename3 = Filename.concat (Filename.concat "/" bucketname) file_name_3
let reply_ns = Config.reply_ns

let parse_mpart reply =
  try
  match CodedIO.Xml.parse_string reply.body with
  | `El (((_, tag),_), c) as xml ->
      assert_eq_string ~msg:reply.body "InitiateMultipartUploadResult" tag;
      begin match get_xml_field "UploadId" xml with
      | None -> assert_failure "expected upload id"
      | Some id -> id
      end
  | `Data _ ->
      assert_failure "expected upload id xml"
 with Xmlm.Error ((line,col), err) ->
   assert_failure (Printf.sprintf "Xml error %s at %d:%d"
    (Xmlm.error_message err) line col)
  ;;

let generate_part fname id i part =
  Direct {
    name = Printf.sprintf "part %d" i;
    req = sign_request {
      meth = `PUT;
      host = !Config.base_hostname;
      port = !Config.base_port;
      relative_url =
        Printf.sprintf "%s?partNumber=%d&uploadId=%s"
        (encode fname) (i+1) id;
      req_headers = [];
      req_body = part;
    };
    expected = {
      headers_exact = [];
      headers_present = "ETag" :: std_headers;
      headers_present_opt = std_headers_opt;
      expected_code = 200;
      check_body = (function "" -> true | _ -> false);
      is_head = false
    }
  }

open CodedIO
let gen_complete_multipart fname uploadId _ replies =
  let parts = Array.to_list (Array.mapi (fun i r ->
    let etag = List.assoc "ETag" r.headers in
    Xml.tag "Part" [
      Xml.tag "PartNumber" [Xml.d (string_of_int (i+1))];
      Xml.tag "ETag" [Xml.d etag]
    ]
  ) (Array.of_list replies)) in
  let xml = Xml.to_string (Xml.tag "CompleteMultipartUpload" parts) in
  let port_opt = if !Config.base_port = 80 then "" else
    (Printf.sprintf ":%d" !Config.base_port) in
  let url = Printf.sprintf "http://%s%s%s"
      !Config.base_hostname port_opt (encode fname) in
  {
    name = "complete multipart";
    req = sign_request {
      meth = `POST;
      host = !Config.base_hostname;
      port = !Config.base_port;
      relative_url =
        Printf.sprintf "%s?uploadId=%s" (encode fname) uploadId;
      req_headers = [];
      req_body = xml;
    };
    (* TODO: more thorough checks *)
    expected = expect_xml_status_root `Ok reply_ns
    "CompleteMultipartUploadResult"
  };;

let rec print_mismatch a b pos =
  if pos < (String.length a) && pos < (String.length b) then
    if a.[pos] = b.[pos] then
      print_mismatch a b (pos+1)
    else
      Printf.eprintf "Mismatch at byte %d: %d != %d\n" pos
        (Char.code a.[pos]) (Char.code b.[pos]);;

let generate_multipart_test fname name parts =
let all = String.concat "" parts in
let digest = Digest.to_hex (Digest.string all) in
Chain {
  chain_name = name ^ " multipart upload/download";
  first = sign_request {
    meth = `GET;
    host = !Config.base_hostname;
    port = !Config.base_port;
    relative_url = encode fname;
    req_headers = [];
    req_body = "";
  };
  generate = (fun _ -> [
    Chain {
      chain_name = name ^ " multipart upload";
      first = sign_request {
        meth = `POST;
        host = !Config.base_hostname;
        port = !Config.base_port;
        relative_url = (encode fname) ^ "?uploads";
        req_headers = [];
        req_body = "";
      };
      generate = (fun reply ->
        let uploadId = parse_mpart reply in
        [Chain {
          chain_name = "upload";
          first = sign_request {
            meth = `GET;
            host = !Config.base_hostname;
            port = !Config.base_port;
            relative_url = encode fname;
            req_headers = [];
            req_body = "";
          };
          generate = (fun _ ->
            (* TODO: expect 404 since its not uploaded yet *)
            Array.to_list (
              Array.mapi (generate_part fname uploadId) (Array.of_list parts)
            )
          );
          finish = (gen_complete_multipart fname uploadId);
        }]);
        finish = (fun _ _ -> {
          name = "check uploaded file";
          req = sign_request {
            meth = `GET;
            host = !Config.base_hostname;
            port = !Config.base_port;
            relative_url = encode fname;
            req_headers = [];
            req_body = "";
          };
          expected = {
            headers_exact = [
              "Content-Length",string_of_int (String.length all)];
            (* the ETag is _not_ necesarely an MD5 now *)
            headers_present = "ETag" :: std_headers;
            headers_present_opt = std_headers_opt;
            expected_code = 200;
            check_body = (fun b ->
              if (all <> b) then begin
                if (String.length b) < 128 then
                  Printf.eprintf "Received body: %s\n" b;
                print_mismatch all b 0;
                if (String.length all) <> (String.length b) then
                  Printf.eprintf "Mismatch length: %d != %d" (String.length all)
                    (String.length b);
              end;
              assert_bool "body compare" (all = b);
              true);
            is_head = false
          }
        })
    }
  ]);
  finish = (fun reply _ -> {
    name = "delete multipart uploaded file";
    req = sign_request {
      meth = `DELETE;
      host = !Config.base_hostname;
      port = !Config.base_port;
      relative_url = (encode fname);
      req_headers = [];
      req_body = "";
    };
    expected = {
      headers_exact = [];
      headers_present = std_headers;
      headers_present_opt = std_headers_opt;
      check_body = (function "" -> true | _ -> false);
      expected_code = 204;
      is_head = false
    }
  })
};;

let test_file_upload ?(enc=true) filename =
  let file = if enc then encode filename else filename in
  let relative_url = Filename.concat (Filename.concat "/" bucketname) file in
  let copy_url = Filename.concat (Filename.concat (Filename.concat "/" bucketname) "copy") file in
  let body = "blah" in
  let etag = "\"" ^ (Digest.to_hex (Digest.string body)) ^ "\"" in
  Chain {
    chain_name = "file";
    first = sign_request {
      meth = `PUT;
      host = !Config.base_hostname;
      port = !Config.base_port;
      relative_url = relative_url;
      req_headers = ["Content-Type","application/blah"];(* test that it is
      not validated *)
      req_body = body;
    };
    (* TODO: expected for first too! check for etag from bug #103 *)
    generate = (fun _ -> [
      (* TODO: re-enable when SX supports this 
      Direct {
        name = "delete non-empty bucket";
        req = sign_request {
          meth = `DELETE;
          (* TODO: use a default_request *)
          host = !Config.base_hostname;
          port = !Config.base_port;
          relative_url = "/"^bucketname;
          req_headers = [];
          req_body = "";
        };
        expected = expect_error Error.BucketNotEmpty ("/"^bucketname)
      };*)
      Direct {
        name = "newly created file is listable";
        req = sign_request {
          meth = `GET;
          host = !Config.base_hostname;
          port = !Config.base_port;
          relative_url = "/" ^ bucketname;
          req_headers = [];
          req_body = "";
        };
        expected =
          expect_xml_status_root_child `Ok
          ~ns_root:reply_ns ~tag_root:"ListBucketResult"
          ~child_tag:"Key" ~child_contents:filename
      };
      Direct {
        name = "newly created file is downloadable";
        req = sign_request {
          meth = `GET;
          host = !Config.base_hostname;
          port = !Config.base_port;
          relative_url = relative_url;
          req_headers = [];
          req_body = "";
        };
        expected = {
          headers_exact = ["Content-Type","application/blah"];
          headers_present = std_headers;
          headers_present_opt = std_headers_opt;
          check_body = (function "blah" -> true | _ -> false);
          expected_code = 200;
          is_head = false
        }
      };
      Direct {
        name = "newly created file is HEADable";
        req = sign_request {
          meth = `HEAD;
          host = !Config.base_hostname;
          port = !Config.base_port;
          relative_url = relative_url;
          req_headers = [];
          req_body = "";
        };
        expected = {
          headers_exact =
            ["Content-Type","application/blah";"Content-Length","4"];
          headers_present = std_headers;
          headers_present_opt = std_headers_opt;
          check_body = (function "" -> true | _ -> false);
          expected_code = 200;
          is_head = true
        }
      };
      Direct {
        name = "newly created file is copyable " ^ etag;
        req = sign_request {
          meth = `PUT;
          host = !Config.base_hostname;
          port = !Config.base_port;
          relative_url = copy_url;
          req_headers = ["x-amz-copy-source",relative_url];
          req_body = "";
        };
        expected =
          expect_xml_status_root_child `Ok
          ~ns_root:"" ~tag_root:"CopyObjectResult"
          ~child_tag:"ETag" ~child_contents:etag
      };
      (* TODO: chain? *)
      Direct {
        name = "copy is deletable";
        req = sign_request {
          meth = `DELETE;
          host = !Config.base_hostname;
          port = !Config.base_port;
          relative_url = copy_url;
          req_headers = [];
          req_body = "";
        };
        expected = {
          headers_exact = [];
          headers_present = std_headers;
          headers_present_opt = std_headers_opt;
          check_body = (function "" -> true | _ -> false);
          expected_code = 204;
          is_head = false
        }
      }
    ]);
    finish = (fun _ _ -> {
      name = "delete file";
      req = sign_request {
        meth = `DELETE;
        host = !Config.base_hostname;
        port = !Config.base_port;
        relative_url = relative_url;
        req_headers = [];
        req_body = "";
      };
      expected = {
        headers_exact = [];
        headers_present = std_headers;
        headers_present_opt = std_headers_opt;
        check_body = (function "" -> true | _ -> false);
        expected_code = 204;
        is_head = false
      }
    })
  };;

let test_head_no file =
  Direct {
    name = "head nonexistent object";
    req = sign_request {
      meth = `HEAD;
      host = !Config.base_hostname;
      port = !Config.base_port;
      relative_url = Filename.concat ("/" ^ bucketname) (encode file);
      req_headers = [];
      req_body = ""
    };
    expected = expect_error ~head:true Error.NoSuchKey ("/" ^ file)
  };;

let suite real = [
  Direct {
  name = "test no auth";
  req = {
    meth = `GET;
    host = "foo." ^ (!Config.base_hostname);
    port = !Config.base_port;
    relative_url = "/";
    req_headers = [];
    req_body = "";
  };
  expected = expect_error Error.AccessDenied "/foo"
  };
  Direct {
    name = "test auth";
    req = sign_request {
      meth = `GET;
      host = !Config.base_hostname;
      port = !Config.base_port;
      relative_url = "/";
      req_headers = [];
      req_body = "";
    };
    expected = expect_xml_status_root `Ok reply_ns "ListAllMyBucketsResult"
  };
  Direct {
    name = "test bucket creation";
    req = sign_request {
      meth = `PUT;
      host = !Config.base_hostname;
      port = !Config.base_port;
      relative_url = "/" ^ bucketname ^ "/";
      req_headers = [];
      req_body = "";
    };
    expected = {
      headers_exact = ["Location","/" ^ bucketname];
      headers_present = std_headers;
      headers_present_opt = std_headers_opt;
      check_body = (function "" -> true | _ -> false);
      expected_code = 200;
      is_head = false
    }
  };
  Direct {
    name = "test bucket creation";
    req = sign_request {
      meth = `PUT;
      host = !Config.base_hostname;
      port = !Config.base_port;
      relative_url = "/" ^ bucketname;
      req_headers = [];
      req_body = "";
    };
    expected = {
      headers_exact = ["Location","/" ^ bucketname];
      headers_present = std_headers;
      headers_present_opt = std_headers_opt;
      check_body = (function "" -> true | _ -> false);
      expected_code = 200;
      is_head = false
    }
  };
  Direct {
    name = "404 on nonexistent bucket";
    req = sign_request {
      meth = `GET;
      host = !Config.base_hostname;
      port = !Config.base_port;
      relative_url = "/" ^ bucketname2;
      req_headers = [];
      req_body = "";
    };
    expected = expect_error Error.NoSuchBucket ("/"^bucketname2)
  };
  Direct {
    name = "404 on nonexitent bucket HEAD";
    req = sign_request {
      meth = `HEAD;
      host = !Config.base_hostname;
      port = !Config.base_port;
      relative_url = "/" ^ bucketname2;
      req_headers = [];
      req_body = "";
    };
    expected = {
      headers_exact  =[];
      headers_present = std_headers;
      headers_present_opt = std_headers_opt;
      check_body = (function "" -> true | _ -> false);
      expected_code = 404;
      is_head = true
    }
  };
  (* TODO: re-enable when SX supports it 
  Direct {
    name = "404 on delete of nonexistent bucket";
    req = sign_request {
      meth = `DELETE;
      host = !Config.base_hostname;
      port = !Config.base_port;
      relative_url = "/" ^ bucketname2;
      req_headers = [];
      req_body = "";
    };
    expected = expect_error Error.NoSuchBucket ("/"^bucketname2)
  };*)
  Chain {
    (* TODO: test bucket creation with UTF-8 names *)
    chain_name = "create/delete bucket";
    first = sign_request {
      meth = `PUT;
      host = !Config.base_hostname;
      port = !Config.base_port;
      relative_url = "/" ^ bucketname;
      req_headers = [];
      req_body = "";
    };
    generate = (fun _ -> [
      Direct {
        name = "newly created bucket exists in listing";
        req = sign_request {
          meth = `GET;
          host = !Config.base_hostname;
          port = !Config.base_port;
          relative_url = "/";
          req_headers = [];
          req_body = "";
        };
        expected =
          expect_xml_status_root_child `Ok
          ~ns_root:reply_ns ~tag_root:"ListAllMyBucketsResult"
          ~child_tag:"Name" ~child_contents:bucketname
      };
      Direct {
        name = "newly created bucket is HEADable";
        req = sign_request {
          meth = `HEAD;
          host = !Config.base_hostname;
          port = !Config.base_port;
          relative_url = "/" ^ bucketname;
          req_headers = [];
          req_body = "";
        };
        expected = {
          headers_exact  =[];
          headers_present = std_headers;
          headers_present_opt = std_headers_opt;
          check_body = (function "" -> true | _ -> false);
          expected_code = 200;
          is_head = true
        }
      };
      Direct {
        name = "no 404 on empty prefix search by dir";
        req = sign_request {
          meth = `GET;
          host = !Config.base_hostname;
          port = !Config.base_port;
          relative_url = "/" ^ bucketname ^ "/?prefix=test/";
          req_headers = [];
          req_body = "";
        };
        expected = expect_xml_status_root_nochild `Ok
          ~ns_root:reply_ns ~tag_root:"ListBucketResult"
          ~child_tag:"Contents"
      };
      Direct {
        name = "newly created bucket is listable and empty";
        req = sign_request {
          meth = `GET;
          host = !Config.base_hostname;
          port = !Config.base_port;
          relative_url = "/" ^ bucketname;
          req_headers = [];
          req_body = "";
        };
        expected =
          expect_xml_status_root_nochild `Ok
          ~ns_root:reply_ns ~tag_root:"ListBucketResult"
          ~child_tag:"Contents"
      };
      Direct {
        name = "max-keys 0 works";
        (* TODO: test that it really returns 0 keys after uploading a file! *)
        req = sign_request {
          meth = `GET;
          host = !Config.base_hostname;
          port = !Config.base_port;
          (* python-boto uses ?& *)
          relative_url = Printf.sprintf "/%s?&max-keys=0" bucketname;
          req_headers = [];
          req_body = "";
        };
        expected =
          expect_xml_status_root_nochild `Ok
          ~ns_root:reply_ns ~tag_root:"ListBucketResult"
          ~child_tag:"Contents"
      };
      test_file_upload "testfile§öüóőúéáâășț";
      test_file_upload "thedir/thefile";
      test_file_upload "thedir/a/b/c/d/thefile";
      (* when doing the real test Netclient will always encode '^', so we cannot
       * test the unencoded '^' *)
      test_file_upload ~enc:real "Icon^M";(* s3cmd doesn't encode ^ *)
      (* TODO:^ netTest encodes this still and then test fails due to sig
        * mismatch *)
      test_head_no "thedir/";
      Direct {
        name = "404 on nonexistent file";
        req = sign_request {
          meth = `GET;
          host = !Config.base_hostname;
          port = !Config.base_port;
          relative_url = encode filename2;
          req_headers = [];
          req_body = "";
        };
        expected = expect_error Error.NoSuchKey filename2
      };
      Direct {
        name = "204 on delete of nonexistent file";
        req = sign_request {
          meth = `DELETE;
          host = !Config.base_hostname;
          port = !Config.base_port;
          relative_url = encode filename2;
          req_headers = [];
          req_body = "";
        };
        expected = {
          headers_exact = [];
          headers_present = std_headers;
          headers_present_opt = std_headers_opt;
          expected_code = 204;
          check_body = (function "" -> true | _ -> false);
          is_head = false
        }
      };
      generate_multipart_test filename3 "one" [
        generate_block Config.min_multipart
      ];
      generate_multipart_test filename3 "two" [
        generate_block Config.min_multipart;
        generate_block Config.min_multipart
      ];
      generate_multipart_test filename3 "three" [
        generate_block (Int64.add Config.min_multipart 1L);
        generate_block (Int64.add Config.min_multipart 2L);
        generate_block (Int64.add Config.min_multipart 3L)
      ];
      (* TODO: generate multiple parts of blocksize, and then blocksize+1,+2,
       * etc. *)
    ]);
    finish = (fun _ _ -> {
(*TODO: re-enable when SX supports delete
 * name = "delete bucket";*)
      name = "check bucket";
      req = sign_request {
(*        meth = `DELETE;*)
        meth = `HEAD;
        host = !Config.base_hostname;
        port = !Config.base_port;
        relative_url = "/" ^ bucketname;
        req_headers = [];
        req_body = "";
      };
      expected = {
        headers_exact = [];
        headers_present = std_headers;
        headers_present_opt = std_headers_opt;
        check_body = (function "" -> true | _ -> false);
(*        expected_code = 204;*)
        expected_code = 200;
(*        is_head = false*)
        is_head = true
      }
    })
  }
]
