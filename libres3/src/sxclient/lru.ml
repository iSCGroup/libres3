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

module Make(Key:Map.OrderedType) = struct
  module KMap = Map.Make(Key)
  type 'a kind =
    | Amain of 'a * Key.t DLinkedList.node
    | A1in of (Key.t * 'a)
    | A1out of (Key.t, 'a) WeakBuffer.id

  (* TODO: use bytes for accounting! *)
  type 'a cache = {
    amain: Key.t DLinkedList.t;
    a1in: (Key.t * 'a) Queue.t;
    a1out: Key.t Queue.t;
    buf: (Key.t, 'a) WeakBuffer.t;
    mutable map: 'a kind KMap.t;
    mutable a1in_size: int;
    mutable a1out_size: int;
    mutable amain_size: int;
    kin: int;
    kout: int;
    total_size: int;
  }

  let create n =
    let kout = n / 2 in {
    amain = DLinkedList.create ();
    a1in = Queue.create ();
    a1out = Queue.create ();
    buf = WeakBuffer.create kout;
    map = KMap.empty;
    a1in_size = 0;
    a1out_size = 0;
    amain_size = 0;
    kin = n / 4;
    kout = kout;
    total_size = n
  }

  open DLinkedList

  let add_a1out cache key value =
    let element = A1out (WeakBuffer.add cache.buf key value) in
    Queue.push key cache.a1out;
    cache.a1out_size <- cache.a1out_size + 1;
    if cache.a1out_size > cache.kout then begin
      cache.map <- KMap.remove (Queue.pop cache.a1out) cache.map;
      cache.a1out_size <- cache.a1out_size - 1;
    end;
    cache.map <- KMap.add key element cache.map;;

  let add_a1in cache key value =
    Queue.push (key,value) cache.a1in;
    let element = A1in (key,value) in
    cache.map <- KMap.add key element cache.map;
    cache.a1in_size <- cache.a1in_size + 1
  ;;

  let add_main cache key value =
    let node = add_head cache.amain key in
    let element = Amain (value, node) in
    cache.map <- KMap.add key element cache.map;
    cache.amain_size <- cache.amain_size + 1
  ;;

  let reclaim cache =
    if (cache.a1in_size + cache.a1out_size + cache.amain_size < cache.total_size) then
      ()
    else if cache.a1in_size > cache.kin then begin
      let ykey,yval = Queue.pop cache.a1in in
      cache.a1in_size <- cache.a1in_size - 1;
      cache.map <- KMap.remove ykey cache.map;
      add_a1out cache ykey yval
    end else begin
      let y = remove (tail cache.amain) in
      cache.map <- KMap.remove y cache.map;
      cache.amain_size <- cache.amain_size - 1
      (* do not put it on A1out, it hasn't been accessed for a while *)
    end
  ;;

  let find cache key =
    match KMap.find key cache.map with
    | Amain (data, node) ->
        cache.map <- KMap.remove key cache.map;
        ignore (remove node);
        add_main cache key data;
        data
    | A1in (_, data) ->
        data
    | A1out id ->
        begin match WeakBuffer.get cache.buf id with
        | Some data ->
            reclaim cache;
            add_main cache key data;
            data
        | None ->
            raise Not_found
        end
    ;;

  let replace cache key data =
    try
      match KMap.find key cache.map with
      | Amain (_,node) ->
        cache.map <- KMap.add key (Amain (data, node)) cache.map
      | A1in _ ->
        cache.map <- KMap.add key (A1in (key, data)) cache.map
      | A1out _ ->
          reclaim cache;
          add_main cache key data
    with Not_found ->
      reclaim cache;
      add_a1in cache key data;
  ;;
end
