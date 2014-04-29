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
module Make(M:Sigs.Monad)(IO:Sigs.EventIOSig with type 'a t = 'a M.t) : sig
  val tests: test
end = struct
  open IO.Op

  let test_monad () =
    assert_bool "return" (IO.Op.return == M.return);
    assert_bool ">>=" (IO.Op.(>>=) == M.(>>=));
    assert_bool "try_catch" (IO.try_catch == M.try_catch);
    assert_bool "fail" (IO.fail == M.fail);;

  let test_seq_map () =
    M.run (
      (return 4 >|= fun v ->
      v + 5) >|= fun result ->
      assert_equal ~printer:string_of_int 9 result
    );;

  let f_no_err v =
    return (v + 4);;

  exception TestException of string

  let id x = x
  let print_exn_opt = function
    | None -> "N/A"
    | Some e -> Printexc.to_string e;;

  let test_try_finally try_exn finally_exn () =
    let v0 = 42 in
    let ex = TestException "try exception" in
    let ex2 = TestException "finally exception" in
    let expected_exn =
      if try_exn then Some ex
      else if finally_exn then Some ex2
      else None in
    let finally_executed = ref false in
    let caught_exn = M.run (
      IO.try_catch (fun () ->
        IO.try_finally
        (fun v ->
          f_no_err v >>= fun res ->
          if try_exn then
            IO.fail ex
          else
            return res)
        (fun v ->
          assert_equal ~printer:string_of_int v0 v;
          finally_executed := true;
          if finally_exn then
            IO.fail ex2
          else
            return ()
        )
        v0 >>= fun res ->
        f_no_err v0 >>= fun expected ->
        assert_equal ~printer:string_of_int expected res;
        return None;
      ) (fun exn -> return (Some exn)) ()
    ) in
    assert_equal
      ~msg:"expect/caught exn don't match" ~printer:print_exn_opt
      expected_exn caught_exn;;

  let test_iter_s () =
    let counter = ref 0 in
    let r1 = ref 0
    and r2 = ref 0
    and r3 = ref 0
    and f r =
      incr counter;
      r := !counter;
      return () in
    M.run (IO.iter_s f [r1; r2; r3]);
    assert_equal ~printer:string_of_int 1 (!r1);
    assert_equal ~printer:string_of_int 2 (!r2);
    assert_equal ~printer:string_of_int 3 (!r3);;


  let test_rev_map_p () =
    (* TODO: test that it really is parallel,
     * by introducing some sleeping functions *)
    M.run (
      let f v = return (v + 4) in
      IO.rev_map_p f [5;6;7;8] >|= fun l ->
      TestUtil.assert_eq_intlist [12;11;10;9] l
    );;

  let expect_invalid_arg f =
    IO.try_catch
      (fun () -> f () >>= fun () ->
        assert_failure "invalid argument expected"
      )
      (function
      | Invalid_argument _ -> return ()
      | e -> IO.fail e)
      ();;

  let test_file_write () =
    M.run (
      let name = "test-file-write"
      and expected = "foo" in
      IO.with_file_write name 0o700 (fun out ->
        out expected 0 (String.length expected)) >>= fun () ->
      IO.string_of_file name >>= fun actual ->
      assert_equal ~printer:id expected actual;
      IO.unlink name
    );;

  let build_onebuf c =
    String.make Config.buffer_size c;;

  let mkdir_maybe name =
    IO.try_catch 
      (fun () -> IO.mkdir name 0o700)
      (function
      (* TODO: put this into eventIO too *)
      | Unix.Unix_error(Unix.EEXIST,_,_) -> return ()
      | e -> IO.fail e)
      ();;

  let test_with_dir () =
    M.run (
      let name = "test.dir" in
      mkdir_maybe name >>= fun () ->
      mkdir_maybe (Filename.concat name "one") >>= fun () ->
      let lst = ref [] in
      IO.with_dir name (fun entry -> lst := entry :: !lst; return ()) >>= fun () ->
      lst := List.sort String.compare !lst;
      let expected = List.sort String.compare [".";"..";"one"] in
      TestUtil.assert_eq_stringlist expected !lst;
      IO.rmdir (Filename.concat name "one") >>= fun () ->
      IO.rmdir name
    );;

  let tests =
    "EventIO">:::[
      "basic monad">::test_monad;
      ">|=">::test_seq_map;
      "try_finally">:::[
        "noexn/noexn">::(test_try_finally false false);
        "noexn/exn">::(test_try_finally false true);
        "exn/noexn">::(test_try_finally true false);
        "exn/exn">::(test_try_finally true true);
      ];
      "iter_s">::test_iter_s;
      "rev_map_p">::test_rev_map_p;
      "file write">::test_file_write;
      "with_dir">::test_with_dir;
    ]
end
