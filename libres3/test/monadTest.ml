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
module Make(M:Sigs.Monad) : sig
  type ('a,'b,'c) t = {
    name: string;
    value: unit -> 'a;
    f: 'a -> 'b M.t;
    g: 'b -> 'c M.t;
    print_a: ('a -> string) option;
    print_b: ('b -> string) option;
    print_c: ('c -> string) option;
  }

  val tests : ('a,'b,'c) t -> test
end = struct
    (* f and g can have side-effects, so
     * we need a function that constructs the same value,
     * instead of just a value (which would be modified by multiple calls to f).
     * Hence value: unit -> 'a
     * *)
  type ('a,'b,'c) t = {
    name: string;
    value: unit -> 'a;
    f: 'a -> 'b M.t;
    g: 'b -> 'c M.t;
    print_a: ('a -> string) option;
    print_b: ('b -> string) option;
    print_c: ('c -> string) option;
  }

  open M

  let compare_equal printer expected actual =
    assert_equal ?printer expected actual

  let expect_equal printer actual_monad expected_monad =
    let actual = run actual_monad
    and expected = run expected_monad in
    compare_equal printer expected actual;;

  let assert_same_exception expected actual =
    assert_equal ~msg:"exceptions must match" ~printer:Printexc.to_string expected actual;;

  exception TestException of string
  let test_try_fail_catch1 () =
    let ex = TestException "test" in
    let catch_executed = ref false in
    ignore (run (
      try_catch
      (fun () -> fail ex)
      (fun e ->
        assert_same_exception ex e;
        catch_executed := true;
        return ()
      )
      ()
    ));
    assert_bool "catch must be executed" !catch_executed;;

  let test_try_fail_catch2 spec () =
    let ex = TestException "test" in
    let catch_executed = ref false in
    ignore (run (
      try_catch
      (fun v ->
        spec.f v >>= fun _ ->
        fail ex >>= fun _ ->
        assert_failure "code must not be executed after fail!";
      )
      (fun e ->
        assert_same_exception ex e;
        catch_executed := true;
        return ()
      )
      (spec.value())
    ));
    assert_bool "catch must be executed" !catch_executed;;

  let test_try_fail_catch_fail spec () =
    let ex = TestException "test" in
    let ex2 = TestException "test2" in
    assert_raises ~msg:"must raise exception" ex2 (fun () ->
      run (try_catch
        (fun v ->
          spec.f v >>= fun _ ->
          fail ex >>= fun _ ->
          assert_failure "code must not be executed after fail!";
        )
        (fun e ->
          assert_same_exception ex e;
          fail ex2
        )
        (spec.value())
      )
    );;

  let test_try_fail_catch_fail_catch spec () =
    let ex = TestException "test" in
    let ex2 = TestException "test2" in
    let catch_called = ref false in
    run (try_catch
      (try_catch
        (fun v ->
          spec.f v >>= fun _ ->
          fail ex >>= fun _ ->
          assert_failure "code must not be executed after fail!";
        )
        (fun e ->
          assert_same_exception ex e;
          fail ex2
        )
      )
      (fun e ->
        assert_same_exception ex2 e;
        catch_called := true;
        return ()
      )
      (spec.value ())
    );
    assert_bool "catch must be called" !catch_called;;

  let test_try_raise_catch () =
    let ex = TestException "test" in
    let catch_executed = ref false in
    ignore (run (
      try_catch
      (fun () -> raise ex)
      (fun e ->
        assert_same_exception ex e;
        catch_executed := true;
        return ()
      )
      ()
    ));
    assert_bool "catch must be executed" !catch_executed;;

  let test_try_raise_catch_nested spec () =
    let ex = TestException "test" in
    let catch_executed = ref false in
    ignore (run (
      try_catch
      (fun v ->
        spec.f v >>= fun _ ->
        raise ex
      )
      (fun e ->
        assert_same_exception ex e;
        catch_executed := true;
        return ()
      )
      (spec.value ())
    ));
    assert_bool "catch must be executed" !catch_executed;;

  let test_try_fail_catch_raise spec () =
    let ex = TestException "test" in
    let ex2 = TestException "test2" in
    assert_raises ~msg:"must raise exception" ex2 (fun () ->
      run (try_catch
        (fun v ->
          spec.f v >>= fun _ ->
          fail ex >>= fun _ ->
          assert_failure "code must not be executed after fail!";
        )
        (fun e ->
          assert_same_exception ex e;
          raise ex2
        )
        (spec.value())
      )
    );;

  let tests spec =
    spec.name>::: [
      "basic">:::
      [
        "return/run">::(fun () ->
          let a = spec.value () in
          expect_equal spec.print_a (return a) (return a);
        );
        "return/bind/run">::(fun () ->
          run (
            return (spec.value ()) >>= fun a ->
            return (spec.value ()) >>= fun b ->
            return (compare_equal spec.print_a a b)
          )
        );
      ];
      "exceptions">:::[
        "try_fail_catch1">::test_try_fail_catch1;
        "try_fail_catch2">::(test_try_fail_catch2 spec);
        "try_fail_catch_fail">::(test_try_fail_catch_fail spec);
        "try_fail_catch_fail_catch">::(test_try_fail_catch_fail_catch spec);
        "try_raise_catch">::test_try_raise_catch;
        "try_raise_catch_nested">::(test_try_raise_catch_nested spec);
        "try_raise_catch_raise">::(test_try_fail_catch_raise spec);
      ];
      "monad laws">:::[
        "left identity">::(fun () ->
          let a = spec.value () in
          expect_equal spec.print_b (return a >>= spec.f) (spec.f a)
        );
        "right identity">::(fun () ->
          let m = return (spec.value ()) in
          expect_equal spec.print_a (m >>= return) m
        );
        "associativity">::(fun () ->
          let m1 = return (spec.value ())
          and m2 = return (spec.value ()) in
          expect_equal spec.print_c
            ((m1 >>= spec.f) >>= spec.g)
            (m2 >>= (fun x -> spec.f x >>= spec.g))
        )
      ]
    ]
end

module MakeTests(M:Sigs.Monad): sig
  val tests: test
end = struct
  module Build = Make(M)
  open M

  let id x = x

  let f_int x =
    return (float_of_int (x + 4))

  let g_float x =
    return (string_of_float (x *. 6.))

  let f_ref x =
    x := !x + 16;
    return x;;

  let g_ref x =
    x := !x + 400;
    return (!x);;

  let print_ref x =
    string_of_int (!x);;

  let tests =
    "monad">:::
    [
      Build.tests {
        Build.name = "basic";
        value = (fun () -> 42);
        f = f_int;
        g = g_float;
        print_a = Some string_of_int;
        print_b = Some string_of_float;
        print_c = Some id;
      };
      Build.tests {
        Build.name = "side-effect";
        value = (fun () -> ref 4);
        f = f_ref;
        g = g_ref;
        print_a = Some print_ref;
        print_b = Some print_ref;
        print_c = Some string_of_int;
      }
    ]

end
