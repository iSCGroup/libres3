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
open CodedIO
let id x = x

let test_roundtrip (input:Xml.t) () =
  assert_equal
    ~printer:Xml.to_string
    input
    (Xml.parse_string (Xml.to_string input));;

let codedio_tests =
  "CodedIO">:::[
    "xml data">::(fun () ->
      assert_equal ~printer:id
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\ntest"
        (Xml.to_string (Xml.d "test"))
    );
    "roundtrip">::(
      test_roundtrip (Xml.tag "ab" [
        Xml.tag "a" ~attrs:[Xml.attr "bar" "4<>"] [];
        Xml.tag "bar" [Xml.d "bar"];
        Xml.tag "x" ~attrs:[
          Xml.attr "x" "4\"";
          Xml.attr ~ns:(Xmlm.ns_xml) "p" "p"
        ] [
          Xml.d "aöpőÁ"
        ]
      ])
    )
  ];;

open Cryptoutil
let cryptoutil_tests =
  "cryptoutil">::: [
    "base64">:::(
      List.map (fun (sin,sout) ->
        sin>::(fun () ->
          assert_equal ~msg:sin ~printer:id sout (base64_encode sin)
        )
      ) [
        "","";
        "f","Zg==";
        "fo","Zm8=";
        "foo","Zm9v";
        "foob","Zm9vYg==";
        "fooba","Zm9vYmE=";
        "foobar","Zm9vYmFy"
      ]);
      "hmac-sha1">:::(
        List.map (fun (key,buf,out) ->
        buf>::(fun () ->
          let b = Buffer.create 16 in
          Buffer.add_string b buf;
          assert_equal ~msg:buf ~printer:Digest.to_hex
            out (hmac_sha1 key b)
        )
      ) [
        "\x0b\x0b\x0b\x0b\x0b\x0b\x0b\x0b\x0b\x0b\x0b\x0b\x0b\x0b\x0b\x0b\x0b\x0b\x0b\x0b",
        "Hi There",
        "\xb6\x17\x31\x86\x55\x05\x72\x64\xe2\x8b\xc0\xb6\xfb\x37\x8c\x8e\xf1\x46\xbe\x00"
      ]
      )
  ];;

let suite =
  "Misc">:::[
    codedio_tests;
    cryptoutil_tests
  ]
let _ =
  run_test_tt_main suite;;
