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

type t = Bucket of string

(* Parse an absolute URL and return the bucket name.
 * This is the only function that can return a Bucket.t *)
let from_url url orig_full_path =
  let url_host = Neturl.url_host url in
  let opath =
    try
      let qpos = String.index orig_full_path '?' in
      String.sub orig_full_path 0 qpos
    with Not_found ->
      orig_full_path in
  if url_host = !Config.base_hostname then
    (* "legacy" style URL: /bucket/path, extract bucket from the URL: *)
    let bucket, path = Util.url_split_first_component
      (Neturl.split_path opath) in
    Bucket bucket, path
  else
    (* virtual hosting style URL: Host: bucket.<base_hostname>; URL: /path *)
    let base_hostname_suffix = "." ^ !Config.base_hostname in
    (* extract bucket from Host *)
    let bucket = Util.string_before_suffix url_host base_hostname_suffix in
    Bucket bucket, opath
;;

let to_string (Bucket s) = s
