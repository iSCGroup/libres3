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
(*module EInt = struct
type t = int
  let compare = ( - )
  let pp_printer = Format.pp_print_int
  let pp_print_sep fmt () = Format.fprintf fmt ",@ "
end
module ListInt = OUnitDiff.ListSimpleMake(EInt);;

module EString = struct
  type t = string
  let compare = String.compare
  let pp_printer = Format.pp_print_string
  let pp_print_sep = OUnitDiff.pp_comma_separator
end
module ListString = OUnitDiff.ListSimpleMake(EString);;*)

let assert_eq_function ~msg a b =
  assert_bool msg (a == b);;

let id (x:string) = x

let assert_eq_string ?msg expected actual =
  assert_equal ?msg ~printer:id expected actual;;

let assert_eq_int ?msg expected actual =
  assert_equal ?msg ~printer:string_of_int expected actual;;

let assert_eq_int64 ?msg expected actual =
  assert_equal ?msg ~printer:Int64.to_string expected actual;;

let assert_eq_float ?msg expected actual =
  assert_equal ?msg ~printer:string_of_float expected actual;;

let string_of_list s = String.concat "," s

let assert_eq_stringlist ?msg expected actual =
  assert_equal ?msg ~printer:string_of_list expected actual;;

(* this lacks some error-handling code *)
let rec rmdirs dir =
    let d = Unix.opendir dir in
    try
        while true; do
            let dirent = Unix.readdir d in
            if dirent <> "." && dirent <> ".." then
            let entry = Filename.concat dir dirent in
            if (Unix.stat entry).Unix.st_kind = Unix.S_DIR then
                rmdirs entry
            else
                Unix.unlink entry
        done
    with
    | End_of_file ->
            Unix.closedir d;
            Unix.rmdir dir;;


let assert_eq_sort_stringlist ?msg expected actual =
  let e = List.fast_sort String.compare expected
  and a = List.fast_sort String.compare actual in
  assert_eq_stringlist ?msg e a;;

let assert_eq_intlist ?msg expected actual =
  assert_equal ?msg expected actual;;

module Pipeline = struct
  type ('i, 'o) listmap = 'i list -> 'o list
  type ('i, 'o) listjoin = 'i list -> 'o
  type ('i,'o,'r) dep = {
    input: 'i;
    after: 'o -> 'r;
  }

  let after a b = { input = a; after = b }

  let join lst =
    List.rev (List.fold_left (fun a d -> List.rev_append d a) [] lst);;

  let join2 lst =
    List.rev_map fst lst, join (List.rev_map snd lst);;

  let split2 lst =
    List.rev_map snd lst

  let split lst =
    let a, b =
      List.fold_left (fun (a1,a2) (e1,e2) ->
        e1 :: a1, e2 :: a2) ([], []) lst in
    List.rev a, List.rev b;;

  let id x = x

  (* pipeline combiner *)
  let lift lst = {
      input = List.rev_map (fun d -> d.input) lst;
      after = List.rev_map2 (fun d o -> d.after o) lst;
  };;

  let run f f2 deplist =
    let deps = lift deplist in
    f2 (deps.after (f deps.input));;
end
(*open Pipeline
let compute = List.map int_of_string;;
let foo =
  "pipeline">:::
    run compute id [
      after "16" (fun n ->
        "testX">:::
        [
          "test1">::(fun () -> assert_eq_int 16 n)
        ] @
        (let results, tests = run compute join2 [
          after (string_of_int (n+1)) (fun n1 ->
            n1, [
            "test3">::(fun () -> assert_eq_int 17 n1)
          ]);
          after (string_of_int (n+2)) (fun n2 ->
            n2, [
            "test4">::(fun () -> assert_eq_int 18 n2)
          ])
        ] in
        List.rev_append [
            "testn">::(fun () -> assert_eq_intlist [17;18] results)
        ] tests
      ))
];;*)
