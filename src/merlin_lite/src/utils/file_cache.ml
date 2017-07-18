(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013 - 2015  Frédéric Bour  <frederic.bour(_)lakaban.net>
                             Thomas Refis  <refis.thomas(_)gmail.com>
                             Simon Castellan  <simon.castellan(_)iuwt.fr>

  Permission is hereby granted, free of charge, to any person obtaining a
  copy of this software and associated documentation files (the "Software"),
  to deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  The Software is provided "as is", without warranty of any kind, express or
  implied, including but not limited to the warranties of merchantability,
  fitness for a particular purpose and noninfringement. In no event shall
  the authors or copyright holders be liable for any claim, damages or other
  liability, whether in an action of contract, tort or otherwise, arising
  from, out of or in connection with the software or the use or other dealings
  in the Software.

)* }}} *)

module Make(Input : sig
  type t
  val read : string -> t
  val cache_name : string
  val policy : [ `Stat_fs | `Stat_dir_cache ]
end) = struct
  let section = "File_cache("^Input.cache_name^")"

  let cache : (string, Stat_cache.file_id * Input.t) Hashtbl.t
            = Hashtbl.create 17

  let file_id = match Input.policy with
    | `Stat_fs -> Stat_cache.file_id
    | `Stat_dir_cache ->
      fun filename ->
        Stat_cache.cached_file_id (Filename.dirname filename)

  let read filename =
    let fid = file_id filename in
    try
      let fid', file = Hashtbl.find cache filename in
      if (Stat_cache.file_id_check fid fid') then
        Logger.logf section "read" "reusing %S" filename
      else (
        Logger.logf section "read" "%S was updated on disk" filename;
        raise Not_found;
      );
      file
    with Not_found ->
    try
      Logger.logf section "read" "reading %S from disk" filename;
      let file = Input.read filename in
      Hashtbl.replace cache filename (fid, file);
      file
    with exn ->
      Logger.logf section "read" "failed to read %S (%t)"
        filename (fun () -> Printexc.to_string exn);
      Hashtbl.remove cache filename;
      raise exn

  let flush () =
    let add_invalid filename (fid, _) invalids =
      if Stat_cache.file_id_check (file_id filename) fid then (
        Logger.logf section "flush" "keeping %S" filename;
        invalids
      ) else (
        Logger.logf section "flush" "removing %S" filename;
        filename :: invalids
      )
    in
    let invalid = Hashtbl.fold add_invalid cache [] in
    List.iter (Hashtbl.remove cache) invalid

  let clear () =
    Hashtbl.clear cache
end
