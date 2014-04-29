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

open Cryptokit

let base64_encode s =
  (* Base64.encode_compact_pad is not available in older cryptokit releases *)
  let e = transform_string (Base64.encode_compact ()) s in
  let padding = match (String.length e) mod 4 with
  | 3 -> 1
  | 2 -> 2
  | _ -> 0 in
  e ^ (String.make padding '=');;

let base64url_encode s =
  (* rfc4648 *)
  let r = transform_string (Base64.encode_compact ()) s in
  for i = 0 to ((String.length r)-1) do
    if r.[i] = '+' then
      r.[i] <- '-'
    else if r.[i] = '/' then
      r.[i] <- '_'
  done;
  r;;

let base64_decode s =
  transform_string (Base64.decode ()) s;;

let hmac_sha1 key b =
  hash_string (MAC.hmac_sha1 key) (Buffer.contents b)

let sign_str key s =
  let hmac = hash_string (MAC.hmac_sha1 key) s in
  base64_encode hmac;;

let to_hex s =
  Cryptokit.transform_string (Cryptokit.Hexa.encode ()) s;;
