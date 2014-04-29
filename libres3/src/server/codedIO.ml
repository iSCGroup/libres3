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

module Xml = struct
  type t = t Xmlm.frag

  (* helpers to build an Xmlm tree
   * without having to use the verbose specification
   * with namespaces *)

  let name ?ns s = match ns with
  | None -> "", s
  | Some ns -> ns, s;;

  let attr ?ns attr_name attr_value =
    name ?ns attr_name, attr_value;;

  let tag ?ns tag_name ?attrs children =
    `El (
      (
        name ?ns tag_name,
        match attrs with None -> [] | Some a -> a
      ),
      children
    );;

  let d str = `Data str

  let parse_string str : t =
    let source = Xmlm.make_input (`String (0, str)) in
    snd (Xmlm.input_doc_tree
      ~el:(fun tag lst -> `El (tag, lst)) ~data:(fun data -> `Data data) source
    );;

  let conv_id (x:t) = x

  let to_string tree =
    let buf = Buffer.create Config.small_buffer_size in
    let out = Xmlm.make_output (`Buffer buf) in
    Xmlm.output_doc_tree conv_id out (None, tree);
    Buffer.contents buf
  ;;
end
