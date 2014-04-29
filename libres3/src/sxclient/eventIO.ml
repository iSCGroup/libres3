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

let buffer_size = 128*1024
let small_buffer_size = 4096
module type OSMonad = sig
  type 'a t
  (* directories *)
  val mkdir: string -> Unix.file_perm -> unit t
  val rmdir : string -> unit t
  type dir_handle
  val opendir : string -> dir_handle t
  val readdir : dir_handle -> string t
  val closedir : dir_handle -> unit t
  (* files *)
  type file_descr
  val openfile: string -> Unix.open_flag list -> Unix.file_perm -> file_descr t
  val close: file_descr -> unit t
  val read: file_descr -> string -> int -> int -> int t
  val access: string -> Unix.access_permission list -> unit t
  val write: file_descr -> string -> int -> int -> int t
  val unlink: string -> unit t
  val rename: string -> string -> unit t
  val sleep : float -> unit t
  module LargeFile: sig
    val lseek: file_descr -> int64 -> Unix.seek_command -> int64 t
    val lstat: string -> Unix.LargeFile.stats t
    val fstat: file_descr -> Unix.LargeFile.stats t
  end
end;;

type output_data = string * int * int

module Make (M: Sigs.Monad) (OS: OSMonad with type 'a t = 'a M.t)
: Sigs.EventIOSig with type 'a t = 'a M.t
= struct
  include M
  module Op = struct
    include M
    let (>|=) a map =
      a >>= fun v ->
        return (map v)
  end
  open Op

  type output_data = string * int * int
  type output_stream = string -> int -> int -> unit t

  open M
  let try_finally fn_try fn_finally value =
    try_catch
      fn_try
      (fun e ->
        (* run finally, ignoring any exceptions, and reraise original *)
        try_catch fn_finally (fun _ -> return ()) value >>= fun () ->
        fail e) value >>= fun result ->
      fn_finally value >>= fun () ->
      return result
  ;;

  (* stream *)
  module type RawStream = sig
    type t
    type name
    val read: t -> output_data M.t
    val open_readable: name -> t M.t
    val close_readable: t -> unit M.t
  end

  module Stream = struct
    type 'a t = {
      state: 'a;
      read: 'a -> output_data M.t;
    }

    let read src = src.read src.state
    let rec iter src f =
      read src >>= fun (str,pos,len) ->
      if len = 0 then return ()
      else
        f str pos len >>= fun () ->
        iter src f
    ;;
  end

  module MakeStream(R: RawStream) = struct
    type name = R.name
    type state = R.t

    let with_readable name f =
      R.open_readable name >>=
      try_finally
        (fun state ->
          f {
            Stream.state = state;
            read = R.read;
          }
        )
        R.close_readable
  end

  (* seekable stream *)
  module type RawSource = sig
    type t
    type name
    val read: t -> output_data M.t
    val seek: t -> int64 -> unit M.t
    val open_source: name -> (t * int64 * float) M.t
    val close_source: t -> unit M.t
  end

  module Source = struct
    type 'a t = {
      state: 'a;
      read: 'a -> output_data M.t;
      seek: 'a -> int64 -> unit M.t;
      size: int64;
      mtime: float;
    }

    let size src = src.size
    let last_modified src = src.mtime
    let begin_read src pos =
      src.seek src.state pos >>= fun () ->
      return {
        Stream.state = src.state;
        read = src.read
      }
    ;;
  end
  module type SourceWrap = sig
    type state
    type name
    val with_source: name -> (state Source.t -> 'a Op.t) -> 'a Op.t
  end

  module MakeSource(R: RawSource) = struct
    type name = R.name
    type state = R.t
    let with_source name f =
      R.open_source name >>= fun (state, size, mtime) ->
      try_finally
        (fun state ->
          f {
            Source.state = state;
            read = R.read;
            seek = R.seek;
            size = size;
            mtime = mtime
          }
        )
        R.close_source
        state
  end

  let rec iter_s fn lst =
    match lst with
    | [] ->
        return ()
    | hd :: tl ->
        fn hd >>= (fun () -> iter_s fn tl)
    ;;

  let rev_map_p fn lst =
    let rec mapper result = function
      | [] -> return result
      | hd :: tl ->
          fn hd >>= fun r ->
          mapper (r :: result) tl
    in
    mapper [] lst;;

  module FileSource = MakeSource(struct
    type t = OS.file_descr * string
    type name = string

    let read (fd, buf) =
      OS.read fd buf 0 (String.length buf) >>= fun amount ->
      return (buf, 0, amount)

    let seek (fd,_) pos =
      OS.LargeFile.lseek fd pos Unix.SEEK_SET >>= fun _ ->
      return ()

    let close_source (fd, _) =
      OS.close fd

    let open_source filename =
      OS.openfile filename [Unix.O_RDONLY] 0 >>= fun fd ->
      try_catch
        (fun () ->
          OS.LargeFile.fstat fd >>= fun stat ->
          if stat.Unix.LargeFile.st_kind <> Unix.S_REG then
            fail (Unix.Unix_error(Unix.ENOENT,"openfile",filename))
          else
            let state = fd, String.create buffer_size in
            return
              (state,
              stat.Unix.LargeFile.st_size,
              stat.Unix.LargeFile.st_mtime)
        )
        (fun e ->
          OS.close fd >>= fun () ->
          fail e
        ) ()
    ;;
  end)
  type filestate = FileSource.state
  let with_file = FileSource.with_source

  let with_resource ~fn_open ~fn_close f value =
    fn_open value >>= fun r ->
      try_finally f fn_close r;;

  exception InputTooLarge of int

  let read_all ~input ~max =
    let buf = Buffer.create small_buffer_size in
    Stream.iter input (fun str pos len ->
        let n = (Buffer.length buf) + len in
        if n > max then
          fail (InputTooLarge n)
        else begin
          Buffer.add_substring buf str pos len;
          return ()
        end
    ) >>= fun () ->
    return (Buffer.contents buf)
  ;;

  let mkdir = OS.mkdir
  let rmdir = OS.rmdir
  let lstat = OS.LargeFile.lstat
  let rename = OS.rename
  let unlink = OS.unlink

  let rec really_write out str pos len =
    if len <= 0 then return ()
    else
      OS.write out str pos len >>= fun amount ->
      really_write out str (pos + amount) (len - amount);;

  let with_file_write filename perm f =
    with_resource
      ~fn_open:(fun name ->
        OS.openfile name [Unix.O_WRONLY; Unix.O_CREAT] perm)
      ~fn_close:OS.close
      (fun out ->
        f (really_write out)
      )
      filename;;

  let with_dir dirname fn_file =
    let rec loop_dir dir =
      OS.readdir dir >>= fn_file >>= fun () ->
      loop_dir dir
    in
    try_catch
      (with_resource
        ~fn_open:OS.opendir
        ~fn_close:OS.closedir
        loop_dir)
      (function
      | End_of_file -> return ()
      | e -> fail e
      )
      dirname
    ;;

  let string_of_file filename =
    FileSource.with_source filename (fun source ->
      Source.begin_read source 0L >>= fun readable ->
      read_all ~input:readable ~max:(Int64.to_int (Source.size source))
    );;
end
