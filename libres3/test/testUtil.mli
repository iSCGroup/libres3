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

val assert_eq_function : msg:string -> 'a -> 'a -> unit
val id : string -> string
val assert_eq_string : ?msg:string -> string -> string -> unit
val assert_eq_int : ?msg:string -> int -> int -> unit
val assert_eq_int64 : ?msg:string -> int64 -> int64 -> unit
val assert_eq_float : ?msg:string -> float -> float -> unit
val assert_eq_stringlist :
  ?msg:string -> string list -> string list -> unit
val assert_eq_sort_stringlist :
  ?msg:string -> string list -> string list -> unit
val assert_eq_intlist : ?msg:string -> int list -> int list -> unit
val rmdirs : string -> unit
module Pipeline : sig
  type ('i, 'o) listmap = 'i list -> 'o list
  type ('i, 'o) listjoin = 'i list -> 'o
  type ('i, 'o, 'r) dep
  val after : 'a -> ('b -> 'c) -> ('a, 'b, 'c) dep
  val join : 'a list list -> 'a list
  val join2 : ('a * 'b list) list -> 'a list * 'b list
  val split : ('a * 'b) list -> ('a list * 'b list)
  val split2 : ('a * 'b) list -> 'b list
  val id : 'a list -> 'a list
  val run : ('i,'o) listmap -> ('r,'d) listjoin -> ('i, 'o, 'r) dep list -> 'd
end
