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

module type Monad = sig
  (* monad operations *)
  type 'a t
  val return: 'a -> 'a t
  val (>>=): 'a t -> ('a -> 'b t) -> 'b t
  val run : 'a t -> 'a

  (* exception handling inside the monads *)
  val fail: exn -> 'a t
  val try_catch: ('a -> 'b t) -> (exn -> 'b t) -> 'a -> 'b t
end

module type ThreadMonad = sig
  (* communication between threads *)
  type 'a t
  type 'a result
  type 'a wakener = 'a result -> unit
  val result: (unit -> 'a) -> 'a result
  val wait : unit -> 'a t * 'a wakener
end

module type Cache = sig
  type 'a t
  val get : name:string -> (string option) t
  val put : name:string -> value:string -> unit t
end

type metafn = unit -> (string * string) list
module type SXIOSig = sig
  module M : Monad
  type url

  type output_data = string * int * int
  type input_stream = unit -> output_data M.t
  type output_stream = output_data -> unit M.t

  type entry = {
    name: string;
    size: int64;
    mtime: float
  }

  type source = {
    meta: entry;
    seek: int64 -> input_stream M.t
  }

  val iter: input_stream -> output_stream -> unit M.t
  type sink = int64 -> output_stream M.t

  (* auth (note: the urls below should all have a user part too) *)
  val token_of_user : [< `Url of url ] -> string option M.t

  (* operations *)
  val get_meta: [< `Url of url] -> (string*string) list M.t
  val copy: ?metafn:metafn ->
      [< `Source of source | `Url of url] -> srcpos:int64 ->
      [< `Sink of sink | `Url of url | `Null] -> unit M.t
  val delete: [< `Url of url ] -> unit M.t

  (* create a volume, directory, or file as appropiate *)
  val create: ?replica:int -> [< `Url of url ] -> unit M.t
  val fold_list: base:[< `Url of url] -> [< `Url of url ] ->
        entry:('a -> entry -> 'a M.t) -> recurse:(string -> bool) -> 'a -> 'a M.t

  val exists: [< `Url of url ] -> int64 option M.t
  val check : [< `Url of url ] -> string option M.t

  (* sources *)
  val of_source: source -> [> `Source of source]
  val of_string: string -> [> `Source of source ]

  (* sinks *)
  val of_sink: sink -> [> `Sink of sink ]

  (* urls *)
  val of_url : string -> [> `Url of url]
  val of_neturl: Neturl.url -> [> `Url of url]
  val with_url_source: [< `Url of url] -> (source -> 'a M.t) -> 'a M.t
  val with_urls_source: [< `Url of url] list -> int64 -> (source -> 'a M.t) -> 'a M.t

  module type SchemeOps = sig
    type state
    val scheme : string
    val syntax: Neturl.url_syntax

    val init : unit -> unit
    val token_of_user : Neturl.url -> string option M.t
    val check: Neturl.url -> string option M.t
    val open_source: Neturl.url -> (entry * state) M.t
    val seek: state -> int64 -> unit M.t
    val read: state -> output_data M.t
    val close_source : state -> unit M.t

    (* true: optimized copy if scheme and authority matches
     * false: fallback to generic copy *)
    val copy_same: Neturl.url -> Neturl.url -> bool M.t

    val get_meta: Neturl.url -> (string*string) list M.t
    val put: ?metafn:metafn -> source -> int64 -> Neturl.url -> unit M.t
    val delete: Neturl.url -> unit M.t
    val create: ?metafn:metafn -> ?replica:int -> Neturl.url -> unit M.t
    val exists: Neturl.url -> int64 option M.t

    val fold_list: Neturl.url -> ('a -> entry -> 'a M.t) -> (string -> bool) -> 'a -> 'a M.t
  end
  module RegisterURLScheme(O: SchemeOps) : sig
    val register: unit -> unit
  end
end

exception HttpCode of Nethttp.http_status
type remote = { sx_host: string; sx_port: int; volume: string }
type entry = {
  name: string;
  size: int64;
  mtime: float;
}

module type EventIOSig = sig
  type 'a t
  module Op : sig
    include Monad with type 'a t = 'a t
    val ( >|= ) : 'a t -> ('a -> 'b) -> 'b t
  end

  val fail : exn -> 'a t
  val try_catch : ('a -> 'b t) -> (exn -> 'b t) -> 'a -> 'b t
  val try_finally : ('a -> 'b t) -> ('a -> unit t) -> 'a -> 'b t

  type output_data = string * int * int
  type output_stream = string -> int -> int -> unit t

  module type RawStream = sig
    type t
    type name
    val read: t -> output_data Op.t
    val open_readable: name -> t Op.t
    val close_readable: t -> unit Op.t
  end

  module Stream : sig
    type 'a t
    val read: 'a t -> output_data Op.t
    val iter: 'a t -> output_stream -> unit Op.t
  end

  module MakeStream(R:RawStream) : sig
    type state
    type name = R.name
    val with_readable : name -> (state Stream.t -> 'a Op.t) -> 'a Op.t
  end

  module type RawSource = sig
    type t
    type name
    val read: t -> output_data Op.t
    val seek: t -> int64 -> unit Op.t
    val open_source: name -> (t * int64 * float) Op.t
    val close_source: t -> unit Op.t
  end

  module Source : sig
    type 'a t

    val size : 'a t -> int64
    val last_modified : 'a t -> float
    val begin_read: 'a t -> int64 -> 'a Stream.t Op.t
  end

  module type SourceWrap = sig
    type state
    type name
    val with_source: name -> (state Source.t -> 'a Op.t) -> 'a Op.t
  end

  module MakeSource(R: RawSource) : SourceWrap with type name = R.name

  val iter_s : ('a -> unit t) -> 'a list -> unit t
  val rev_map_p : ('a -> 'b t) -> 'a list -> 'b list t

  type filestate
  val with_file: string -> (filestate Source.t -> 'a t) -> 'a t

  val with_resource : fn_open:('a -> 'b t) -> fn_close:('b -> unit t) -> ('b -> unit t) -> 'a -> unit t
  val with_file_write : string -> Unix.file_perm -> (output_stream -> unit t) -> unit t
  val with_dir : string -> (string -> unit t) -> unit t

  val mkdir: string -> Unix.file_perm -> unit t
  val rmdir: string -> unit t
  val lstat: string -> Unix.LargeFile.stats t
  val rename: string -> string -> unit t
  val unlink: string -> unit t
  exception InputTooLarge of int
  val read_all : input:'a Stream.t -> max:int -> string t
  val string_of_file : string -> string t
end

module type SXClientMonad = sig
  type pipeline
  module IO : EventIOSig
  module Get : IO.SourceWrap
    with type name = pipeline * string * string * string

  val init : unit -> pipeline
  val finish: pipeline -> unit

  val list_volume: pipeline -> remote -> entry list IO.t
  val put: pipeline -> remote -> string -> 'a IO.Source.t -> unit IO.t
end
