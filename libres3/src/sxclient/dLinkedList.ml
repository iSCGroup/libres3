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

type 'a node = {
  v: 'a;
  parent: 'a t;
  mutable prev: 'a node option;
  mutable next: 'a node option;
}
and 'a t = {
  mutable head: 'a node option;
  mutable tail: 'a node option;
}

let create () = {
  head = None;
  tail = None;
}

let tail l = match l.tail with
| None -> invalid_arg "list is empty"
| Some tail -> tail

let add_head l v =
  let node = { v = v; prev = None; next = l.head; parent = l } in
  begin match l.head with
  | None -> l.tail <- Some node
  | Some old_head ->
      old_head.prev <- Some node;
  end;
  l.head <- Some node;
  node;;

let add_tail l v =
  let node = { v = v; prev = l.tail; next = None; parent = l } in
  begin match l.tail with
  | None -> ()
  | Some old_tail ->
      old_tail.next <- Some node
  end;
  l.tail <- Some node;
  node;;

let remove v =
(* remove from double-linked list *)
  begin match v.prev with
  | None ->
    (* this was head *)
    v.parent.head <- v.next;
  | Some prev ->
      prev.next <- v.next
  end;
  begin match v.next with
  | None ->
      (* this was tail *)
      v.parent.tail <- v.prev;
  | Some next ->
      next.prev <- v.prev
  end;
  v.v
;;
