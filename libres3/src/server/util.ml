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

let string_before_suffix str suffix =
  let n1 = String.length str
  and n2 = String.length suffix in
  if n1 >= n2 && (String.sub str (n1 - n2) n2) = suffix then
    String.sub str 0 (n1 - n2)
  else
    str;;

let url_split_first_component split_url =
  match split_url with
  | "" :: first :: [] ->
      first, ""
  | "" :: first :: rest ->
      first, Neturl.join_path ("" :: rest)
  | first :: rest ->
      first, Neturl.join_path ("" :: rest)
  | [] -> "","/";;

let raw_url_after raw_url prefix =
  let url = Str.string_after raw_url 0 in
  let n1 = String.length url
  and n2 = String.length prefix in
  if n1 >= n2 && (String.sub url 0 n2) = prefix then
    String.sub url n2 (n1 - n2)
  else
    url;;

let raw_path_after raw_url prefix =
  let u = raw_url_after raw_url prefix in
  try
    String.sub u 0 (String.index u '?')
  with Not_found ->
    u;;

let format_date unix_timestamp =
  Netdate.format ~fmt:"%Y-%m-%dT%H:%M:%S.000Z" (Netdate.create unix_timestamp);;

let sanitize_path path =
  let normalized = Neturl.norm_path (Neturl.split_path path) in
  if List.exists (function | ".." -> true | _ -> false) normalized then
    invalid_arg "Directory traversal attempted" (* TODO: raise a 404 instead *);
  Neturl.join_path normalized;;
