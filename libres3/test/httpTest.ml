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
open TestUtil

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
  headers: (string * string) list;
  body: string;
  code: int;
}

type expected_reply = {
  headers_exact: (string * string) list;
  headers_present: string list;
  headers_present_opt: string list;
  check_body: string -> bool;
  expected_code: int;
  is_head: bool
}

type direct = {
  name: string;
  req: request;
  expected: expected_reply;
}
and chain = {
  chain_name: string;
  first: request;
  (* generate more testcases based on reply from first request *)
  generate: reply -> t list;
  (* generate one final testcase based on initial reply and intermediate replies
   * *)
  finish: reply -> reply list -> direct;
}

and t = Direct of direct | Chain of chain

let body_errmsg b =
  let base = "Body doesn't match expected: " in
  if String.length b > 128 then
    base ^ b
  else
    base ^ b;;

let check_reply expected reply =
  let names = List.rev_map fst reply.headers in
  [
    "reply code" >::(fun () ->
        assert_eq_int expected.expected_code reply.code);
    "body" >:: (fun () ->
        assert_bool (body_errmsg reply.body) (expected.check_body reply.body)
    );
    "content-length" >:: (fun () ->
      if not expected.is_head then
      try
        let n = int_of_string (List.assoc "Content-Length" reply.headers) in
        assert_eq_int ~msg:"content-length" (String.length reply.body) n
      with Not_found -> ()
    );
    "headers present" >:::
      (List.rev_map (fun name ->
        name>:: (fun () ->
          assert_bool ("Header missing:" ^ name) (List.mem name names)
        )
      ) expected.headers_present);
    "headers present optional" >:::
      (List.rev_map (fun name ->
        name>:: (fun () ->
          skip_if (not (List.mem name names)) "Header missing"
        )
      ) expected.headers_present_opt);
    "headers" >:::
      List.rev_map (fun (n,expected) ->
        n>:: (fun () ->
          let actual = List.assoc n reply.headers in
          skip_if (n = "Content-Type" && expected <> actual) "Content-Type TODO";
          assert_eq_string expected actual
        )
      ) expected.headers_exact;
  ];;

open TestUtil.Pipeline

let generate_testcase d =
  after d.req (fun reply ->
    reply,
    d.name>::: check_reply d.expected reply
  );;

let rec generate_chain_testcase f c =
  after c.first (fun reply ->
    try
      let results, tests = run f split (
        List.rev_map (generate_one_testcase f) (c.generate reply)
        ) in
      let final_tests = run f split2 [generate_testcase (c.finish reply results)] in
      reply,
      c.chain_name>:::
      [
        "pipeline">:::tests;
      "finish">:::final_tests
      ]
    with e ->
      raise e;
      reply,
      c.chain_name>::(fun () -> raise e)
  )

and generate_one_testcase f = function
  | Direct d -> generate_testcase d
  | Chain c -> generate_chain_testcase f c;;

let generate_pipeline_testcases f testcases =
  split2 (run f id (List.map (generate_one_testcase f) testcases));;

let failing e = {
  headers = [];
  body = "Test failed: " ^ (Printexc.to_string e);
  code = 502;
}

let generate_direct_testcases f testcases =
  generate_pipeline_testcases (List.rev_map (fun e ->
    try
      match f [e] with
      | one :: [] -> one
      | _ -> failwith "Bad reply count"
    with e -> failing e
  )) testcases;;

let generate_tests name f testcases =
  try
    name>:::[
      "direct">::: generate_direct_testcases f testcases;
(*      "pipeline">::: generate_pipeline_testcases f testcases*)
    ]
  with e ->
    let bt = Printexc.get_backtrace () in
    name>::(fun () ->
      assert_failure (
        Printf.sprintf "Exception %s\n%s\n" (Printexc.to_string e)  bt)
    );;
