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

module Make(M:Sigs.Monad)(OS:EventIO.OSMonad with type 'a t = 'a M.t) = struct
  type state = string * OS.file_descr
  let scheme = "file"
  let syntax = Hashtbl.find Neturl.common_url_syntax scheme

  let init () = ()
  open M
  let file url = Neturl.local_path_of_file_url url
  module IO = SXIO.Make(M)

  let token_of_user _ = return (Some !Config.secret_access_key)
  let check _ = return None
  let open_source url =
    let name = file url in
    OS.openfile name [Unix.O_RDONLY] 0 >>= fun fd ->
    try_catch
      (fun () ->
        OS.LargeFile.fstat fd >>= fun stat ->
        if stat.Unix.LargeFile.st_kind <> Unix.S_REG then
          (* it is not a file (could be a dir, etc. *)
          fail (Unix.Unix_error (Unix.ENOENT, "open_source", name))
        else
          let entry = {
            IO.name = name;
            size = stat.Unix.LargeFile.st_size;
            mtime = stat.Unix.LargeFile.st_mtime
          } in
          let (s:state) = String.create Config.buffer_size, fd in
          return (entry, s)
      )
      (fun e ->
        OS.close fd >>= fun () ->
        fail e) ();;

  let seek (_,fd) pos =
    OS.LargeFile.lseek fd pos Unix.SEEK_SET >>= fun _ -> return ();;
  let read (buf,fd) =
    (* TODO: check that there is only one read in-flight on this fd? *)
    OS.read fd buf 0 (String.length buf) >>= fun amount ->
    return (buf, 0, amount);;
  let close_source (_, fd) = OS.close fd

  let copy_same _ _ =
    return false (* no optimized copy, fallback to generic *)

  let is_dir name =
    let n = String.length name in
    n > 0 && name.[n-1] = '/';;

  let delete url =
    let name = file url in
    if is_dir name then
      OS.rmdir name
    else
      OS.unlink name;;

  let exists url =
    let name = file url in
    M.try_catch
      (fun () ->
        OS.access name [Unix.R_OK] >>= fun () ->
          (* TODO: return real size *)
        return (Some 0L)
      )
      (fun _ -> return None) ();;

  let readdir_safe handle =
    try_catch
      (fun () ->
        OS.readdir handle >>= fun entry ->
        return (Some entry))
      (function | End_of_file -> return None | e -> fail e) ();;

  let rec fold_dir f recurse dir accum dirs_waiting handle =
    readdir_safe handle >>= function
    | None -> return (accum, dirs_waiting)
    | Some entry ->
      let path = Filename.concat dir entry in
      OS.LargeFile.lstat path >>= fun stat ->
      match stat.Unix.LargeFile.st_kind with
      | Unix.S_REG ->
        let entry = {
          IO.name = path;
          size = stat.Unix.LargeFile.st_size;
          mtime = stat.Unix.LargeFile.st_mtime
        } in
        f accum entry >>= fun accum ->
        fold_dir f recurse dir accum dirs_waiting handle
     | Unix.S_DIR when entry <> "." && entry <> ".." ->
         if recurse path then
          fold_dir f recurse dir accum (path :: dirs_waiting) handle
        else
          fold_dir f recurse dir accum dirs_waiting handle
     | _ -> fold_dir f recurse dir accum dirs_waiting handle;;

  let rec fold_dir_safe f recurse accum dirs_waiting =
    match dirs_waiting with
    | dir :: rest ->
      try_catch (fun () ->
        OS.opendir dir >>= fun handle ->
        try_catch
          (fun () ->
            fold_dir f recurse dir accum rest handle
          )
          (function | e -> OS.closedir handle >>= fun () -> fail e)
          () >>= fun (accum, dirs_waiting) ->
          fold_dir_safe f recurse accum dirs_waiting
        )
        (fun _ ->
          (* skip this dir *)
          fold_dir_safe f recurse accum rest) ()
    | [] ->
        return accum;;

  let is_prefix ~prefix str =
    let plen = String.length prefix in
    (String.length str) >= plen &&
    (String.sub str 0 plen) = prefix;;

  let filter_fold f prefix accum entry =
    if is_prefix ~prefix entry.IO.name then
      f accum entry
    else return accum

  let filter_recurse recurse prefix dir =
    if is_prefix ~prefix dir then
      recurse dir
    else
      false;;

  let fold_list url f recurse accum =
    let fullpath = Neturl.url_path ~encoded:false url in
    match List.rev fullpath with
    | _last :: rest ->
        (* go back to last dir *)
        let path = List.rev rest in
        let dir = file (Neturl.modify_url ~encoded:false ~path url) in
        let prefix = Neturl.join_path fullpath in
        (* then filter its contents by prefix *)
        fold_dir_safe (filter_fold f prefix) (filter_recurse recurse prefix)
          accum [dir]
    | [] -> (* file URLs are absolute *)
        assert false;;

  let create ?metafn ?replica url =
    let name = file url in
    if is_dir name then
      OS.mkdir name 0o755
    else
      OS.openfile name [Unix.O_WRONLY;Unix.O_CREAT] 0o644 >>= OS.close;;

  let rec create_parents path =
    let parent = Filename.dirname path in
    if parent = "" || parent = "/"  || parent = "//" then
      fail (Unix.Unix_error(Unix.ENOENT,"rename",path))
    else begin
      try_catch
        (fun () ->
          OS.mkdir parent 0o700)
        (function
        | Unix.Unix_error(Unix.ENOENT,_,_) ->
          create_parents parent >>= fun () ->
          OS.mkdir parent 0o700
        | Unix.Unix_error(Unix.EEXIST,_,_) ->
            return ()
        | e -> fail e
        ) ()
    end;;

  let maybe_mkdirs f dst =
    try_catch
      (fun () -> f dst)
      (function
        | Unix.Unix_error(Unix.ENOENT,_,_) | Sys_error _ ->
          create_parents dst >>= fun () ->
          f dst
        | e -> fail e) ();;

  let with_tmpfile tmpname name f =
    try_catch
      (fun () ->
        OS.openfile tmpname [Unix.O_WRONLY; Unix.O_CREAT] 0o644 >>= fun fd ->
        try_catch
          (fun () ->
            f fd >>= fun () ->
            OS.close fd
          )
          (fun e -> OS.close fd >>= fun () -> fail e) () >>= fun () ->
          OS.rename tmpname name
      )
      (fun e ->
        (* TODO: should send e along even if OS.unlink raises *)
        OS.unlink tmpname >>= fun () ->
        fail e
      ) ();;

  let rec really_write fd str pos len =
    if len > 0 then
      OS.write fd str pos len >>= fun amount ->
      really_write fd str (pos + amount) (len - amount)
    else return ();;

  let rec copy_stream_tofd stream fd () =
    stream () >>= fun (str, pos, len) ->
    if len > 0 then
      really_write fd str pos len >>=
      copy_stream_tofd stream fd
    else return ();;

  let metatable = Hashtbl.create 16
  let get_meta url : (string*string) list OS.t =
    try
      return (Hashtbl.find metatable url)
    with Not_found ->
      return []

  let put ?metafn src srcpos dsturl =
    (* atomically create destination with given contents:
     * copy to tmpfile in same directory and then rename if successful
     * or unlink if not *)
    let dst = file dsturl in
    begin match metafn with
    | None -> ()
    | Some f -> Hashtbl.add metatable dsturl (f ())
    end;
    maybe_mkdirs
      (fun dst ->
        return (Filename.temp_file
          ~temp_dir:(Filename.dirname dst) (Filename.basename dst) ".tmp")
      )
      dst >>= fun tmpname ->
    with_tmpfile tmpname dst (fun fd ->
      src.IO.seek srcpos >>= fun stream ->
      copy_stream_tofd stream fd ()
    );;
end
