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

module StringSet = Set.Make(String)

type t = {
  orig: Nethttp.http_header_ro;
  ro: Nethttp.http_header_ro;
  names: StringSet.t;
}

let has_header h name = StringSet.mem name h.names
let build overrides l =
  let orig = new Netmime.basic_mime_header l
  and header = new Netmime.basic_mime_header l in
  let names =
    List.fold_left (fun set (name, _) ->
      StringSet.add (String.lowercase name) set) StringSet.empty l in
  List.iter (fun (source,target) ->
    try
      header#update_field target (header#field source);
      orig#update_field target "";
    with Not_found -> ()
  ) overrides;
  {
    ro = (header :> Nethttp.http_header_ro);
    orig = (orig :> Nethttp.http_header_ro);
    names = names
  };;

let filter_names f h =
  StringSet.elements (StringSet.filter f h.names);;

let field_values h name = h.ro#multiple_field name

let field_single_value h name default =
  match field_values h name with
  | [] -> default
  | one :: _ -> one

let get_authorization h = Nethttp.Header.get_authorization h.ro
let get_range h =
  try
    Some (Nethttp.Header.get_range h.ro)
  with Not_found ->
    None;;

let get_date h =
  try
    Nethttp.Header.get_date h.ro
  with Not_found ->
    0.;;

let orig_field_value h name =
  try
    h.orig#field name
  with Not_found -> ""

let make_content_range bytes =
  let h = new Netmime.basic_mime_header [] in
  Nethttp.Header.set_content_range h bytes;
  Nethttp.Header.set_accept_ranges h ["bytes"];
  h#fields;;
