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

(* 2Q: A Low Overhead High Performance Buffer Management Replacement Algorithm
 * Theodore Johnson, Dennis Shasha
 * 1994 
*)

module type Entry = sig
  type t (* key *)
  val compare: t -> t -> int
  type data
  val cache_size: int
end

module Make (M:Sigs.Monad)(E: Entry) : sig
  type 'a t
  type key = E.t
  type data = E.data
  val bind: key M.t -> (key -> data M.t) -> data M.t
end = struct
  type 'a t = unit -> 'a
  type key = E.t
  type data = E.data
  type 'a entry =
    | Cached of 'a
    | Pending of 'a M.t

  module LRUMap = Lru.Make(E)

  let shared_cache = SharedData.create (LRUMap.create E.cache_size)

  open M
  let cached_bind key (f:key -> 'a M.t) cache : 'a M.t =
   try match LRUMap.find cache key with
    | Pending r -> r
    | Cached data ->
      return data
    with Not_found ->
      let pending = begin
        f key >>= fun result ->
        (* when pending operation finishes
         * update the cache *)
        LRUMap.replace cache key (Cached result);
        return result
      end in
      (* add pending key now *)
      LRUMap.replace cache key (Pending pending);
      pending;;

  let bind k f =
    k >>= fun key ->
    SharedData.use shared_cache (cached_bind key f);;
end
