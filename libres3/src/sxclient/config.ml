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

(* cmdline configurable *)
let buckets_dir = ref "/"
let cache_dir = ref ""
let syslog_facility = ref "local7"
let user : string option ref = ref None
let group : string option ref = ref None
let pidfile = ref ""
let ssl_certificate_file : string option ref = ref None
let ssl_privatekey_file : string option ref = ref None
let base_hostname : string ref = ref ""
let base_port = ref 8008
let base_ssl_port = ref 8443
let secret_access_key = ref ""
let sx_host : string option ref = ref None
let sx_ssl : bool ref = ref true
let replica_count = ref 0
let volume_size = ref 0.
let max_connected = ref 0 (* each server sets this *)
let daemonize = ref true

(* must match the value in _oasis *)
let config_file = "libres3/libres3.conf";;

let buffer_size = 131072
let small_buffer_size = 4096

let min_multipart = 5242880L
let reply_ns = "http://s3.amazonaws.com/doc/2006-03-01/"
let key_id = ref "admin"
let max_keys = 1000
let owner_id = "da39a3ee5e6b4b0d3255bfef95601890afd80709"
let owner_name = "libres3"

let verbose = ref false
let volume_create_elevate_to_admin = ref true
