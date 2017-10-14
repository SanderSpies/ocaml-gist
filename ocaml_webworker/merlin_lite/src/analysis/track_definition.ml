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

open Std

let sources_path = ref []
let cfg_cmt_path = ref []
let loadpath     = ref []

let last_location = ref Location.none

let log title msg = Logger.log "track_definition" title msg
let logf title fmt = Logger.logf "track_definition" title fmt
let logfmt title fmt = Logger.logfmt "track_definition" title fmt

let erase_loadpath ~cwd ~new_path k =
  let str_path_list =
    List.map new_path ~f:(function
      | "" ->
        (* That's the cwd at the time of the generation of the cmt, I'm
            guessing/hoping it will be the directory where we found it *)
        log "erase_loadpath" cwd;
        cwd
      | x ->
        log "erase_loadpath" x;
        x
    )
  in
  let_ref loadpath str_path_list k

let restore_loadpath k =
  log "restore_loadpath" "Restored load path";
  let_ref loadpath !cfg_cmt_path k

module Fallback = struct
  let fallback = ref None

  let get () = !fallback

  let set loc =
    logfmt "Fallback.set" (fun fmt -> Location.print_loc fmt loc);
    fallback := Some loc

  let setopt = function
    | None -> log "Fallback.setopt" "None"
    | Some loc -> set loc

  let reset () = fallback := None

  let is_set () = !fallback <> None
end

module File = struct
  type t =
    | ML   of string
    | MLI  of string
    | CMT  of string
    | CMTI of string

  let name = function ML name | MLI name | CMT name | CMTI name -> name

  let ext = function
    | ML _  -> ".ml"  | MLI _  -> ".mli"
    | CMT _ -> ".cmt" | CMTI _ -> ".cmti"

  exception Not_found of t

  let explain_not_found ?(doc_from="") str_ident path =
    let msg =
      match path with
      | ML file ->
        sprintf "'%s' seems to originate from '%s' whose ML file could not be \
                 found" str_ident file
      | MLI file ->
        sprintf "'%s' seems to originate from '%s' whose MLI file could not be \
                 found" str_ident file
      | CMT file ->
        sprintf "Needed cmt file of module '%s' to locate '%s' but it is not \
                 present" file str_ident
      | CMTI file when file <> doc_from ->
        sprintf "Needed cmti file of module '%s' to locate '%s' but it is not \
                 present" file str_ident
      | CMTI _ ->
        sprintf "The documentation for '%s' originates in the current file, \
                 but no cmt is available" str_ident
    in
    `File_not_found msg
end

module Preferences : sig
  val set : [ `ML | `MLI ] -> unit

  val cmt : string -> File.t
  val ml  : string -> File.t

  val is_preferred : string -> bool
end = struct
  let prioritize_impl = ref true

  let set choice =
    prioritize_impl :=
      match choice with
      | `ML -> true
      | _ -> false

  open File

  let cmt file = if !prioritize_impl then CMT file else CMTI file
  let ml file = if !prioritize_impl then ML file else MLI file

  let is_preferred filename =
    if !prioritize_impl then
      Filename.check_suffix filename "ml" ||
      Filename.check_suffix filename "ML"
    else
      Filename.check_suffix filename "mli" ||
      Filename.check_suffix filename "MLI"
end

module File_switching : sig
  val reset : unit -> unit

  val move_to : ?digest:Digest.t -> string -> unit (* raises Can't_move *)

  val where_am_i : unit -> string option

  val source_digest : unit -> Digest.t option
end = struct
  type t = {
    last_file_visited : string option ;
    digest : Digest.t option ;
  }

  let default = { last_file_visited = None ; digest = None }

  let state = ref default

  let reset () = state := default

  let move_to ?digest file =
    logf "File_switching.move_to" "%s" file;
    state := { last_file_visited = Some file ; digest }

  let where_am_i () = !state.last_file_visited

  let source_digest () = !state.digest
end


module Utils = struct
  let is_builtin_path = function
    | Path.Pident id ->
      let f (_, i) = Ident.same i id in
      List.exists Predef.builtin_idents ~f
      || List.exists Predef.builtin_values ~f
    | _ -> false

  let is_ghost_loc { Location. loc_ghost } = loc_ghost

  let longident_is_qualified = function
    | Longident.Lident _ -> false
    | _ -> true

  let split_extension file =
    (* First grab basename to guard against directories with dots *)
    let basename = Filename.basename file in
    try
      let last_dot_pos = String.rindex basename '.' in
      let ext_name = String.sub basename last_dot_pos (String.length basename - last_dot_pos) in
      let base_without_ext = String.sub basename 0 last_dot_pos in
      (base_without_ext, Some ext_name)
    with Not_found -> (file, None)


  let synonym_extension file (implAlias, intfAlias) =
    match split_extension file with
      | (without_ext, None) -> without_ext
      | (without_ext, Some ext) ->
        if ext = ".ml" then
          without_ext ^ implAlias
        else (
          if ext = ".mli" then
            without_ext ^ intfAlias
          else
            file
        )

  let file_path_to_mod_name f =
    let pref = Misc2.chop_extensions f in
    String.capitalize (Filename.basename pref)

  (* Reuse the code of [Misc2.find_in_path_uncap] but returns all the files
     matching, instead of the first one.
     This is only used when looking for ml files, not cmts. Indeed for cmts we
     know that the load path will only ever contain files with uniq names (in
     the presence of packed modules we refine the loadpath as we go); this in
     not the case for the "source path" however.
     We therefore get all matching files and use an heuristic at the call site
     to choose the appropriate file. *)
  let find_all_in_path_uncap ?(fallback="") path name =
    let has_fallback = fallback <> "" in
    let files =
      let acc = ref [] in
      let uname = String.uncapitalize name in
      let ufbck = String.uncapitalize fallback in
      let try_dir dir =
        let fullname = Filename.concat dir name in
        let fallback = Filename.concat dir fallback in
        let ufullname = Filename.concat dir uname in
        let ufallback = Filename.concat dir ufbck in
        if Misc2.exact_file_exists ufullname then acc := ufullname :: !acc ;
        if Misc2.exact_file_exists fullname then acc := fullname :: !acc ;
        if has_fallback && Misc2.exact_file_exists ufallback then
          acc := ufallback :: !acc ;
        if has_fallback && Misc2.exact_file_exists fallback then
          acc := fallback :: !acc ;
      in
      List.iter try_dir path;
      !acc
    in
    List.map files ~f:Misc2.canonicalize_filename

  let find_all_matches ~config ?(with_fallback=false) file =
    let fname = Misc2.chop_extension_if_any (File.name file) ^ (File.ext file) in
    let fallback =
      if not with_fallback then "" else
      match file with
      | File.ML f   -> Misc2.chop_extension_if_any f ^ ".mli"
      | File.MLI f  -> Misc2.chop_extension_if_any f ^ ".ml"
      | _ -> assert false
    in
    let path = !sources_path in

    let filesList =
      List.map (fun synonym_pair -> (
          let fallback = synonym_extension fallback synonym_pair in
          let fname = synonym_extension fname synonym_pair in
          find_all_in_path_uncap ~fallback path fname
        )) Mconfig.(config.merlin.suffixes)
    in
    let files = List.concat filesList in
    List.uniq files ~cmp:String.compare

  let find_file_with_path ~config ?(with_fallback=false) file path =
    let fname = Misc2.chop_extension_if_any (File.name file) ^ (File.ext file) in
    if Misc2.unitname fname = Misc2.unitname Mconfig.(config.query.filename) then
      Mconfig.(config.query.filename)
    else
      let fallback =
        if not with_fallback then "" else
          match file with
          | File.ML f   -> Misc2.chop_extension_if_any f ^ ".mli"
          | File.MLI f  -> Misc2.chop_extension_if_any f ^ ".ml"
          | File.CMT f  -> Misc2.chop_extension_if_any f ^ ".cmti"
          | File.CMTI f -> Misc2.chop_extension_if_any f ^ ".cmt"
      in
      let rec attempt_search synonyms =
        match synonyms with
        | [] -> raise Not_found
        | [synonym_pair] ->
          (* Upon trying the final [synonym_pair], search failure should raise *)
          let fallback = synonym_extension fallback synonym_pair in
          let fname = synonym_extension fname synonym_pair in
          (
            try Misc2.find_in_path_uncap ~fallback path fname with
              Not_found -> raise (File.Not_found file)
          )
        | synonym_pair :: ((rest1 :: rest2) as rest_synonyms) ->
          (* If cannot find match, continue searching through [rest_synonyms] *)
          let fallback = synonym_extension fallback synonym_pair in
          let fname = synonym_extension fname synonym_pair in
          (
            try Misc2.find_in_path_uncap ~fallback path fname with
              Not_found -> attempt_search rest_synonyms
          )
      in
      attempt_search Mconfig.(config.merlin.suffixes)

  let find_file ~config ?with_fallback file =
    find_file_with_path ~config ?with_fallback file @@
        match file with
        | File.ML  _ | File.MLI _  -> !sources_path
        | File.CMT _ | File.CMTI _ -> !loadpath
end

type context = Type | Expr | Patt of Types.type_expr | Unknown | Label
exception Context_mismatch

let rec locate ~config ?pos path trie =
  match Typedtrie.find ?before:pos trie path with
  | Typedtrie.Found (loc, doc_opt) -> Some (loc, doc_opt)
  | Typedtrie.Resolves_to (new_path, fallback) ->
    begin match new_path with
    | (_, `Mod) :: _ ->
      logf "locate" "resolves to %s" (Typedtrie.path_to_string new_path);
      Fallback.setopt fallback ;
      from_path ~config new_path
    | _ ->
      logf "locate" "new path (%s) is not a real path. fallbacking..."
        (Typedtrie.path_to_string new_path);
      logfmt "locate" (fun fmt -> Typedtrie.dump fmt trie);
      Option.map fallback ~f:(fun x -> x, None)
    end
  | Typedtrie.Alias_of (loc, new_path) ->
    logf "locate" "alias of %s" (Typedtrie.path_to_string new_path) ;
    (* TODO: maybe give the option to NOT follow module aliases? *)
    Fallback.set loc;
    from_path ~config new_path

and browse_cmts ~config ~root modules =
  let open Cmt_format in
  let cached = Cmt_cache.read root in
  logf "browse_cmts" "inspecting %s" root ;
  File_switching.move_to ?digest:cached.Cmt_cache.cmt_infos.cmt_source_digest root ;
  if cached.Cmt_cache.location_trie <> String.Map.empty then begin
    log "browse_cmts" "cmt already cached";
    locate ~config modules cached.Cmt_cache.location_trie
  end else
    match
      match cached.Cmt_cache.cmt_infos.cmt_annots with
      | Interface intf      -> `Browse (Browse_raw.Signature intf)
      | Implementation impl -> `Browse (Browse_raw.Structure impl)
      | Packed (_, files)   -> `Pack files
      | _ ->
        (* We could try to work with partial cmt files, but it'd probably fail
        * most of the time so... *)
        `Not_found
    with
    | `Not_found -> None
    | `Browse node ->
      begin match modules with
      | [] ->
        (* we were looking for a module, we found the right file, we're happy *)
        let pos = Lexing.make_pos ~pos_fname:root (1, 0) in
        let loc = { Location. loc_start=pos ; loc_end=pos ; loc_ghost=false } in
        (* TODO: retrieve "ocaml.text" floating attributes? *)
        Some (loc, None)
      | _ ->
        let trie = Typedtrie.of_browses [Browse_tree.of_node node] in
        cached.Cmt_cache.location_trie <- trie ;
        locate ~config modules trie
      end
    | `Pack files ->
      begin match modules with
      | (mod_name, `Mod) :: _ ->
        assert (List.exists files
                  ~f:(fun s -> Utils.file_path_to_mod_name s = mod_name));
        log "loadpath" "Saw packed module => erasing loadpath" ;
        let new_path = cached.Cmt_cache.cmt_infos.cmt_loadpath in
        erase_loadpath ~cwd:(Filename.dirname root) ~new_path (fun () ->
          from_path ~config modules
        )
      | _ -> None
      end

(* The following is ugly, and deserves some explanations:
      As can be seen above, when encountering packed modules we override the
      loadpath by the one used to create the pack.
      This means that if the cmt files haven't been moved, we have access to
      the cmt file of every unit included in the pack.
      However, we might not have access to any other cmt (e.g. if others
      paths in the loadpath reference only cmis of packs).
      (Note that if we had access to other cmts, there might be conflicts,
      and the paths order would matter unless we have reliable digests...)
      Assuming we are in such a situation, if we do not find something in our
      "erased" loadpath, it could mean that we are looking for a persistent
      unit, and that's why we restore the initial loadpath. *)
and from_path ~config path =
  log "from_path '%s'" (Typedtrie.path_to_string path) ;
  match path with
  | [ fname, `Mod ] ->
    let save_digest_and_return root =
      let {Cmt_cache. cmt_infos} = Cmt_cache.read root in
      File_switching.move_to ?digest:cmt_infos.Cmt_format.cmt_source_digest root ;
      let fname =
        match cmt_infos.Cmt_format.cmt_sourcefile with
        | None   -> fname
        | Some f -> f
      in
      let pos = Lexing.make_pos ~pos_fname:fname (1, 0) in
      let loc = { Location. loc_start=pos ; loc_end=pos ; loc_ghost=true } in
      Some (loc, None)
    in
    begin try
      let cmt_file = Utils.find_file ~config ~with_fallback:true (Preferences.cmt fname) in
      save_digest_and_return cmt_file
    with File.Not_found (File.CMT fname | File.CMTI fname) ->
      restore_loadpath (fun () ->
        try
          let cmt_file = Utils.find_file ~config ~with_fallback:true (Preferences.cmt fname) in
          save_digest_and_return cmt_file
        with File.Not_found (File.CMT fname | File.CMTI fname) ->
          (* In that special case, we haven't managed to find any cmt. But we
             only need the cmt for the source digest in contains. Even if we
             don't have that we can blindly look for the source file and hope
             there are no duplicates. *)
          logf "from_path" "failed to locate the cmt[i] of '%s'" fname;
          let pos = Lexing.make_pos ~pos_fname:fname (1, 0) in
          let loc = { Location. loc_start=pos ; loc_end=pos ; loc_ghost=true } in
          File_switching.move_to loc.Location.loc_start.Lexing.pos_fname ;
          Some (loc, None)
      )
    end
  | (fname, `Mod) :: modules ->
    begin try
      let cmt_file = "/static/cmis/" ^  (String.uncapitalize fname) ^ ".cmi" in
      browse_cmts ~config ~root:cmt_file modules
    with File.Not_found (File.CMT fname | File.CMTI fname) as exn ->
      restore_loadpath (fun () ->
        try
          let cmt_file = Utils.find_file ~config ~with_fallback:true (Preferences.cmt fname) in
          browse_cmts ~config ~root:cmt_file modules
        with File.Not_found (File.CMT fname | File.CMTI fname) ->
          logf "from_path" "failed to locate the cmt[i] of '%s'" fname;
          raise exn
      )
    end
  | _ -> assert false

let path_and_loc_of_cstr desc env =
  let open Types in
  match desc.cstr_tag with
  | Cstr_extension (path, loc) -> path, desc.cstr_loc
  | _ ->
    match desc.cstr_res.desc with
    | Tconstr (path, _, _) -> path, desc.cstr_loc
    | _ -> assert false

let path_and_loc_from_label desc env =
  let open Types in
  match desc.lbl_res.desc with
  | Tconstr (path, _, _) ->
    let typ_decl = Env.find_type path env in
    path, typ_decl.Types.type_loc
  | _ -> assert false

exception Not_in_env
exception Multiple_matches of string list

let find_source ~config loc =
  let fname = loc.Location.loc_start.Lexing.pos_fname in
  let with_fallback = loc.Location.loc_ghost in
  let mod_name = Utils.file_path_to_mod_name fname in
  let file =
    let extensionless = Misc2.chop_extension_if_any fname = fname in
    if extensionless then Preferences.ml mod_name else
    if Filename.check_suffix fname "i" then File.MLI mod_name else File.ML mod_name
  in
  let filename = File.name file in
  let initial_path =
    match File_switching.where_am_i () with
    | None -> fname
    | Some s -> s
  in
  let dir = Filename.dirname initial_path in
  let dir =
    match Mconfig.(config.query.directory) with
    | "" -> dir
    | cwd -> Misc2.canonicalize_filename ~cwd dir
  in
  match Utils.find_all_matches ~config ~with_fallback file with
  | [] ->
    logf "find_source" "failed to find %S in source path (fallback = %b)"
       filename with_fallback ;
    logf "find_source" "looking for %S in %S" (File.name file) dir ;
    begin try Some (Utils.find_file_with_path ~config ~with_fallback file [dir])
    with (File.Not_found _ | Not_found) as exn->
      logf "find_source" "Trying to find %S in %S directly" fname dir;
      try Some (Misc2.find_in_path [dir] fname)
      with _ -> raise exn
    end
  | [ x ] -> Some x
  | files ->
    logf "find_source"
      "multiple files named %s exist in the source path..." filename;
    try
      match File_switching.source_digest () with
      | None ->
        logf "find_source"
          "... no source digest available to select the right one" ;
        raise Not_found
      | Some digest ->
        logf "find_source"
          "... trying to use source digest to find the right one" ;
        logf "find_source" "Source digest: %s" (Digest.to_hex digest) ;
        Some (
          List.find files ~f:(fun f ->
            let fdigest = Digest.file f in
            logf "find_source" "  %s (%s)" f (Digest.to_hex fdigest) ;
            fdigest = digest
          )
        )
    with Not_found ->
      logf "find_source" "... using heuristic to select the right one" ;
      logf "find_source" "we are looking for a file named %s in %s" fname dir ;
      let rev = String.reverse (Misc2.canonicalize_filename ~cwd:dir fname) in
      let lst =
        List.map files ~f:(fun path ->
          let path' = String.reverse path in
          let priority = (String.common_prefix_len rev path') * 2 +
                          if Preferences.is_preferred path
                          then 1
                          else 0
          in
          priority, path
        )
      in
      let lst =
        (* TODO: remove duplicates in [source_path] instead of using
          [sort_uniq] here. *)
        List.sort_uniq ~cmp:(fun ((i:int),s) ((j:int),t) ->
          let tmp = compare j i in
          if tmp <> 0 then tmp else
          match compare s t with
          | 0 -> 0
          | n ->
            (* Check if we are referring to the same files.
                Especially useful on OSX case-insensitive FS.
                FIXME: May be able handle symlinks and non-existing files,
                CHECK *)
            match Stat_cache.file_id s, Stat_cache.file_id t with
            | s', t' when Stat_cache.file_id_check s' t' ->
              0
            | _ -> n
        ) lst
      in
      match lst with
      | (i1, s1) :: (i2, s2) :: _ when i1 = i2 ->
        raise (Multiple_matches files)
      | (_, s) :: _ -> Some s
      | _ -> assert false

(* Well, that's just another hack.
   [find_source] doesn't like the "-o" option of the compiler. This hack handles
   Jane Street specific use case where "-o" is used to prefix a unit name by the
   name of the library which contains it. *)
let find_source ~config loc =
  try find_source ~config loc
  with exn ->
    let fname = loc.Location.loc_start.Lexing.pos_fname in
    try
      let i = String.first_double_underscore_end fname in
      let pos = i + 1 in
      let fname = String.sub fname ~pos ~len:(String.length fname - pos) in
      let loc =
        let lstart = { loc.Location.loc_start with Lexing.pos_fname = fname } in
        { loc with Location.loc_start = lstart }
      in
      find_source ~config loc
    with _ -> raise exn

let recover ident =
  match Fallback.get () with
  | None -> assert false
  | Some loc -> `Found (loc, None)

let namespaces = function
  | Type          -> [ `Type ; `Constr ; `Mod ; `Modtype ; `Labels ; `Vals ]
  | Expr | Patt _ -> [ `Vals ; `Constr ; `Mod ; `Modtype ; `Labels ; `Type ]
  | Unknown       -> [ `Vals ; `Type ; `Constr ; `Mod ; `Modtype ; `Labels ]
  | Label         -> [ `Labels ]

exception Found of (Path.t * Cmt_cache.path * Location.t)

let tag namespace p = Typedtrie.tag_path ~namespace (Path_aux.to_string_list p)

let get_type_name ctxt =
  match ctxt with
  | Patt t ->
    begin match t.Types.desc with
    | Types.Tvar _ | Types.Tarrow _ | Types.Ttuple _ | Types.Tobject _
    | Types.Tfield _ | Types.Tnil | Types.Tlink _ | Types.Tsubst _
    | Types.Tvariant _ | Types.Tunivar _ | Types.Tpoly _ | Types.Tpackage _ ->
      raise Not_found
    | Types.Tconstr (path,_,_) ->
      Longident.parse (String.concat ~sep:"." (Path_aux.to_string_list path))
    end
  | _ -> raise Not_found

let rec lookup ctxt ident env =
  try
    List.iter (namespaces ctxt) ~f:(fun namespace ->
      try
        match namespace with
        | `Constr ->
          log "lookup" "lookup in constructor namespace" ;
          let cstr_desc = Env.lookup_constructor ident env in
          let path, loc = path_and_loc_of_cstr cstr_desc env in
          (* TODO: Use [`Constr] here instead of [`Type] *)
          raise (Found (path, tag `Type path, loc))
        | `Mod ->
          log "lookup" "lookup in module namespace" ;
          let path = Env.lookup_module ~load:true ident env in
          raise (Found (path, tag `Mod path, Location.symbol_gloc ()))
        | `Modtype ->
          log "lookup" "lookup in module type namespace" ;
          let path, _ = Env.lookup_modtype ident env in
          raise (Found (path, tag `Modtype path, Location.symbol_gloc ()))
        | `Type ->
          log "lookup" "lookup in type namespace" ;
          let path = Env_compat.lookup_type ident env in
          let typ_decl = Env.find_type path env in
          raise (Found (path, tag `Type path, typ_decl.Types.type_loc))
        | `Vals ->
          log "lookup" "lookup in value namespace" ;
          let path, val_desc = Env.lookup_value ident env in
          raise (Found (path, tag `Vals path, val_desc.Types.val_loc))
        | `Labels ->
          log "lookup"
            "lookup in label namespace" ;
          let label_desc = Env.lookup_label ident env in
          let path, loc = path_and_loc_from_label label_desc env in
          (* TODO: Use [`Labels] here instead of [`Type] *)
          raise (Found (path, tag `Type path, loc))
      with Not_found -> ()
    ) ;
    logf "lookup" "   ... not in the environment" ;
    let id = try get_type_name ctxt with _ -> raise Not_in_env in
    lookup Type id env
  with Found x ->
    x

let locate ~config ~ml_or_mli ~path ~lazy_trie ~pos ~str_ident loc =
  File_switching.reset ();
  Fallback.reset ();
  Preferences.set ml_or_mli;
  try
    if not (Utils.is_ghost_loc loc) then `Found (loc, None)
    else begin
      logf "locate"
        "present in the environment, but ghost lock.\n\
         walking up the typedtree looking for '%s'"
        (Typedtrie.path_to_string path);
      let lazy trie = lazy_trie in
      match locate ~config ~pos path trie with
      | None when Fallback.is_set () -> recover str_ident
      | None -> `Not_found (str_ident, File_switching.where_am_i ())
      | Some (loc, doc) -> `Found (loc, doc)
    end
  with
  | _ when Fallback.is_set () -> recover str_ident
  | Not_found -> `Not_found (str_ident, File_switching.where_am_i ())
  | File.Not_found path -> File.explain_not_found str_ident path

(* Only used to retrieve documentation *)
let from_completion_entry ~config ~lazy_trie ~pos (namespace, path, loc) =
  let path_lst  = Path_aux.to_string_list path in
  let str_ident = String.concat ~sep:"." path_lst in
  let tagged_path = tag namespace path in
  locate ~config ~ml_or_mli:`MLI ~path:tagged_path ~pos ~str_ident loc
    ~lazy_trie

let from_longident ~config ~env ~lazy_trie ~pos ctxt ml_or_mli lid =
  let ident, is_label = Longident2.keep_suffix lid in
  let str_ident = String.concat ~sep:"." (Longident.flatten ident) in
  try
    let path, tagged_path, loc =
      if not is_label then lookup ctxt ident env else
      (* If we know it is a record field, we only look for that. *)
      let label_desc = Env.lookup_label ident env in
      let path, loc = path_and_loc_from_label label_desc env in
      (* TODO: Use [`Labels] here *)
      path, tag `Type path, loc
    in
    if Utils.is_builtin_path path then `Builtin else
    locate ~config ~ml_or_mli ~path:tagged_path ~lazy_trie ~pos ~str_ident loc
  with
  | Not_found -> `Not_found (str_ident, File_switching.where_am_i ())
  | Not_in_env -> `Not_in_env str_ident

let inspect_pattern ~pos ~parent p =
  let open Typedtree in
  match p.pat_desc with
  | Tpat_any -> None
  | Tpat_var _ ->
    Option.bind parent ~f:(fun parent ->
      match parent.Browse_tree.t_node with
      | Browse_raw.Pattern { pat_desc = Tpat_record (l, _); _ } ->
        let lid, _, _ = List.find l ~f:(fun (_,_,pat) -> pat == p) in
        let open Location in
        if (Location_aux.compare_pos lid.loc.loc_start p.pat_loc = 0) then
          (* { ..; pun[n]ed; ... } *)
          Some Label
        else (
          if Location_aux.compare_pos pos p.pat_loc = 0 then
            (* { ..; foo = b[a]r; ... } *)
            None
          else
            (* { ..; f[o]o = bar; ... } *)
            Some Label
        )
      | _ -> None
    )
  | Tpat_alias _ ->
    (* Assumption: if [Browse.enclosing] stopped on this node and not on the
       subpattern, then it must mean that the cursor is on the alias. *)
    None
  | _ ->
    (* We attach the type here so in the case of disambiguated constructors (or
       record fields) we can fallback on looking up the type (cf. #486).
       Of course that won't take care of constructors (resp. record fields)
       disambiguated in expressions...
       Oh well. *)
    Some (Patt p.pat_type)

let inspect_context browse path pos =
  match Mbrowse.enclosing pos browse with
  | [] ->
    logf "inspect_context" "no enclosing around: %a" Lexing.print_position pos;
    Some Unknown
  | enclosings ->
    let open Browse_raw in
    let node = Browse_tree.of_browse enclosings in
    let parent = Option.map (Mbrowse.drop_leaf enclosings) ~f:Browse_tree.of_browse in
    match node.Browse_tree.t_node with
    | Pattern p ->
      (* logfmt "inspect_context"
        (fun fmt -> Format.fprintf fmt "current node is: %a"
            (Printtyped.pattern 0) p); *)
      inspect_pattern ~pos ~parent p
    | Value_description _
    | Type_declaration _
    | Extension_constructor _
    | Module_binding_name _
    | Module_declaration_name _ as node ->
      logf "inspect_context" "current node is : %s" (string_of_node node);
      None
    | Core_type _ -> Some Type
    | Expression e ->
      begin match e.Typedtree.exp_desc with
      | Typedtree.Texp_record _ -> Some Label
      | _ -> Some Expr
      end
    | _ ->
      Some Unknown

let from_string ~config ~env ~local_defs ~pos switch path =
  let browse = Mbrowse.of_typedtree local_defs in
  let lazy_trie = lazy (Typedtrie.of_browses ~local_buffer:true
                          [Browse_tree.of_browse browse]) in
  let lid = Longident.parse path in
  match inspect_context [browse] path pos with
  | None ->
    log "from_string" "already at origin, doing nothing" ;
    `At_origin
  | Some ctxt ->
    logf "from_string" "looking for the source of '%s' (prioritizing %s files)"
      path (match switch with `ML -> ".ml" | `MLI -> ".mli") ;
    let cmt_path = Mconfig.cmt_path config in
    let_ref sources_path (Mconfig.source_path config) @@ fun () ->
    let_ref cfg_cmt_path cmt_path @@ fun () ->
    let_ref loadpath     cmt_path @@ fun () ->
    match
      from_longident ~config ~pos ~env ~lazy_trie ctxt switch lid
    with
    | `File_not_found _ | `Not_found _ | `Not_in_env _ as err -> err
    | `Builtin -> `Builtin path
    | `Found (loc, _) ->
      try
        match find_source ~config loc with
        | None     -> `Found (None, loc.Location.loc_start)
        | Some src -> `Found (Some src, loc.Location.loc_start)
      with
      | File.Not_found ft -> File.explain_not_found path ft
      | Multiple_matches lst ->
        let matches = String.concat lst ~sep:", " in
        `File_not_found (
          sprintf "Several source files in your path have the same name, and \
                   merlin doesn't know which is the right one: %s"
            matches
        )


let get_doc ~config ~env ~local_defs ~comments ~pos =
  let browse = Mbrowse.of_typedtree local_defs in
  let lazy_trie = lazy (Typedtrie.of_browses ~local_buffer:true
                          [Browse_tree.of_browse browse]) in
  fun path ->
  let cmt_path = Mconfig.cmt_path config in
  let_ref sources_path (Mconfig.source_path config) @@ fun () ->
  let_ref cfg_cmt_path cmt_path @@ fun () ->
  let_ref loadpath     cmt_path @@ fun () ->
  let_ref last_location Location.none @@ fun () ->
  match
    match path with
    | `Completion_entry entry -> from_completion_entry ~config ~pos ~lazy_trie entry
    | `User_input path ->
      let lid    = Longident.parse path in
      begin match inspect_context [browse] path pos with
      | None ->
        `Found ({ Location. loc_start=pos; loc_end=pos ; loc_ghost=true }, None)
      | Some ctxt ->
        logf "get_doc" "looking for the doc of '%s'" path ;
        from_longident ~config ~pos ~env ~lazy_trie ctxt `MLI lid
      end
  with
  | `Found (loc, Some doc) ->
    `Found doc
  | `Found (loc, None) ->
    let comments =
      match File_switching.where_am_i () with
      | None -> List.rev comments
      | Some cmt_path ->
        let {Cmt_cache. cmt_infos} = Cmt_cache.read cmt_path in
        cmt_infos.Cmt_format.cmt_comments
    in
    logfmt "get_doc" (fun fmt ->
        Format.fprintf fmt "looking around %a inside: [\n"
          Location.print_loc !last_location;
        List.iter comments ~f:(fun (c, l) ->
            Format.fprintf fmt "  (%S, %a);\n" c
              Location.print_loc l);
        Format.fprintf fmt "]\n"
      );
    begin match
      Ocamldoc.associate_comment comments loc !last_location
    with
    | None, _     -> `No_documentation
    | Some doc, _ -> `Found doc
    end
  | `Builtin ->
    begin match path with
    | `User_input path -> `Builtin path
    | `Completion_entry (_, path, _) ->
      let str = String.concat ~sep:"." (Path_aux.to_string_list path) in
      `Builtin str
    end
  | `File_not_found _
  | `Not_found _
  | `Not_in_env _ as otherwise -> otherwise
  | _ -> `No_documentation
