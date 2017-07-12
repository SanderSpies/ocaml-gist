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
open Sturgeon_stub
open Misc
open Query_protocol
module Printtyp = Type_utils.Printtyp

let print_completion_entries ~with_types tr config source entries =
  if with_types then
    let input_ref = ref [] and output_ref = ref [] in
    let preprocess entry =
      match Completion.raw_info_printer entry with
      | `String s -> `String s
      | `Print t ->
        let r = ref "" in
        input_ref := t :: !input_ref;
        output_ref := r :: !output_ref;
        `Print r
      | `Concat (s,t) ->
        let r = ref "" in
        input_ref := t :: !input_ref;
        output_ref := r :: !output_ref;
        `Concat (s,r)
    in
    let entries = List.map ~f:(Completion.map_entry preprocess) entries in
    let outcomes = Mreader.print_batch_outcome tr config source !input_ref in
    List.iter2 (:=) !output_ref outcomes;
    let postprocess = function
      | `String s -> s
      | `Print r -> !r
      | `Concat (s,r) -> s ^ !r
    in
    List.rev_map ~f:(Completion.map_entry postprocess) entries
  else List.rev_map ~f:(Completion.map_entry (fun _ -> "")) entries

let with_typer pipeline f =
  let typer = Mpipeline.typer_result pipeline in
  Mtyper.with_typer typer @@ fun () ->
  f (Mpipeline.get_trace pipeline) typer

let for_completion pipeline position f =
  let pipeline = Mpipeline.for_completion position pipeline in
  let typer = Mpipeline.typer_result pipeline in
  Mtyper.with_typer typer @@ fun () ->
  f (Mpipeline.get_trace pipeline) pipeline typer

let verbosity pipeline =
  Mconfig.((Mpipeline.final_config pipeline).query.verbosity)

let dump pipeline = function
  | [`String "parsetree"] ->
    let ppf, to_string = Format.to_string () in
    begin match Mpipeline.reader_parsetree pipeline with
      | `Interface s -> Pprintast.signature ppf s
      | `Implementation s -> Pprintast.structure ppf s
    end;
    Format.pp_print_newline ppf ();
    Format.pp_force_newline ppf ();
    `String (to_string ())

  | [`String "printast"] ->
    let ppf, to_string = Format.to_string () in
    begin match Mpipeline.reader_parsetree pipeline with
      | `Interface s -> Printast.interface ppf s
      | `Implementation s -> Printast.implementation ppf s
    end;
    Format.pp_print_newline ppf ();
    Format.pp_force_newline ppf ();
    `String (to_string ())

  | (`String ("env" | "fullenv" as kind) :: opt_pos) ->
    with_typer pipeline @@ fun tr typer ->
    let kind = if kind = "env" then `Normal else `Full in
    let pos =
      match opt_pos with
      |  [`String "at"; jpos] ->
        Some (match jpos with
            | `String "start" -> `Start
            | `String "end" -> `End
            | `Int offset -> `Offset offset
            | `Assoc props ->
              begin match List.assoc "line" props, List.assoc "col" props with
                | `Int line, `Int col -> `Logical (line,col)
                | _ -> failwith "Incorrect position"
                | exception Not_found -> failwith "Incorrect position"
              end
            | _ -> failwith "Incorrect position"
          )
      | [] -> None
      | _ -> failwith "incorrect position"
    in
    let env = match pos with
      | None -> Mtyper.get_env typer
      | Some pos ->
        let source = Mpipeline.input_source pipeline in
        let pos = Msource.get_lexing_pos tr source pos in
        fst (Mbrowse.leaf_node (Mtyper.node_at tr typer pos))
    in
    let sg = Browse_misc.signature_of_env ~ignore_extensions:(kind = `Normal) env in
    let aux item =
      let ppf, to_string = Format.to_string () in
      Printtyp.signature ppf [item];
      `String (to_string ())
    in
    `List (List.map ~f:aux sg)

  | [`String "browse"] ->
    with_typer pipeline @@ fun tr typer ->
    let structure = Mbrowse.of_typedtree (Mtyper.get_typedtree typer) in
    Browse_misc.dump_browse (snd (Mbrowse.leaf_node structure))

  | [`String "tokens"] ->
    failwith "TODO"

  | [`String "flags"] ->
    let prepare_flags flags =
      `List (List.map Json.string (List.concat_map flags
                                     ~f:(fun f -> f.Mconfig.flag_list))) in
    let user = prepare_flags
        Mconfig.((Mpipeline.input_config pipeline).merlin.flags_to_apply) in
    let applied = prepare_flags
        Mconfig.((Mpipeline.final_config pipeline).merlin.flags_applied) in
    `Assoc [ "user", user; "applied", applied ]

  | [`String "warnings"] ->
    with_typer pipeline @@ fun _tr _typer ->
    Warnings.dump () (*TODO*)

  | [`String "exn"] ->
    let exns =
      Mpipeline.reader_lexer_errors pipeline @
      Mpipeline.reader_parser_errors pipeline @
      Mpipeline.typer_errors pipeline
    in
    `List (List.map ~f:(fun x -> `String (Printexc.to_string x)) exns)

  | [`String "paths"] ->
    let paths = Mconfig.build_path (Mpipeline.final_config pipeline) in
    `List (List.map paths ~f:(fun s -> `String s))

  | _ -> failwith "known dump commands: \
                   paths, exn, warnings, flags, tokens, browse, parsetree, \
                   printast, env/fullenv (at {col:, line:})"

let reconstruct_identifier tr pipeline pos = function
  | None ->
    let path = Mreader.reconstruct_identifier tr
        (Mpipeline.input_config pipeline)
        (Mpipeline.input_source pipeline)
        pos
    in
    let path = Mreader_lexer.identifier_suffix path in
    let reify dot =
      if dot = "" ||
         (dot.[0] >= 'a' && dot.[0] <= 'z') ||
         (dot.[0] >= 'A' && dot.[0] <= 'Z')
      then dot
      else "(" ^ dot ^ ")"
    in
    begin match path with
      | [] -> []
      | base :: tail ->
        let f {Location. txt=base; loc=bl} {Location. txt=dot; loc=dl} =
          let loc = Location_aux.union bl dl in
          let txt = base ^ "." ^ reify dot in
          Location.mkloc txt loc
        in
        [ List.fold_left tail ~init:base ~f ]
    end
  | Some (expr, offset) ->
    let loc_start =
      let l, c = Lexing.split_pos pos in
      Lexing.make_pos (l, c - offset)
    in
    let shift loc int =
      let l, c = Lexing.split_pos loc in
      Lexing.make_pos (l, c + int)
    in
    let add_loc source =
      let loc =
        { Location.
          loc_start ;
          loc_end = shift loc_start (String.length source) ;
          loc_ghost = false ;
        } in
      Location.mkloc source loc
    in
    let len = String.length expr in
    let rec aux acc i =
      if i >= len then
        List.rev_map ~f:add_loc (expr :: acc)
      else if expr.[i] = '.' then
        aux (String.sub expr ~pos:0 ~len:i :: acc) (succ i)
      else
        aux acc (succ i) in
    aux [] offset

let dispatch pipeline (type a) : a Query_protocol.t -> a =
  function
  | Type_expr (source, pos) ->
    with_typer pipeline @@ fun tr typer ->
    let pos = Msource.get_lexing_pos tr (Mpipeline.input_source pipeline) pos in
    let env, _ = Mbrowse.leaf_node (Mtyper.node_at tr typer pos) in
    let ppf, to_string = Format.to_string () in
    let verbosity = verbosity pipeline in
    ignore (Type_utils.type_in_env ~verbosity env ppf source : bool);
    to_string ()

  | Type_enclosing (expro, pos, index) ->
    let open Typedtree in
    with_typer pipeline @@ fun tr typer ->
    let verbosity = verbosity pipeline in
    let structures = Mbrowse.of_typedtree (Mtyper.get_typedtree typer) in
    let pos = Msource.get_lexing_pos tr (Mpipeline.input_source pipeline) pos in
    let env, path = match Mbrowse.enclosing pos [structures] with
      | [] -> Mtyper.get_env typer, []
      | browse ->
         fst (Mbrowse.leaf_node browse),
         Browse_misc.annotate_tail_calls_from_leaf browse
    in
    let aux (node,tail) =
      let open Browse_raw in
      let ret x = Some (Mbrowse.node_loc node, x, tail) in
      match node with
      | Expression {exp_type = t}
      | Pattern {pat_type = t}
      | Core_type {ctyp_type = t}
      | Value_description { val_desc = { ctyp_type = t } } ->
        ret (`Type t)
      | Type_declaration { typ_id = id; typ_type = t} ->
        ret (`Type_decl (id,t))
      | Module_expr {mod_type = m}
      | Module_type {mty_type = m}
      | Module_binding {mb_expr = {mod_type = m}}
      | Module_declaration {md_type = {mty_type = m}}
      | Module_type_declaration {mtd_type = Some {mty_type = m}}
      | Module_binding_name {mb_expr = {mod_type = m}}
      | Module_declaration_name {md_type = {mty_type = m}}
      | Module_type_declaration_name {mtd_type = Some {mty_type = m}} ->
        ret (`Modtype m)
      | _ -> None
    in
    let result = List.filter_map ~f:aux path in
    (* enclosings of cursor in given expression *)
    let small_enclosings =
      let exprs = reconstruct_identifier tr pipeline pos expro in
      let env, node = Mbrowse.leaf_node (Mtyper.node_at tr typer pos) in
      let open Browse_raw in
      let include_lident = match node with
        | Pattern _ -> false
        | _ -> true
      in
      let include_uident = match node with
        | Module_binding _
        | Module_binding_name _
        | Module_declaration _
        | Module_declaration_name _
        | Module_type_declaration _
        | Module_type_declaration_name _
          -> false
        | _ -> true
      in
      List.filter_map exprs ~f:(fun {Location. txt = source; loc} ->
          match source with
          | "" -> None
          | source when not include_lident && Char.is_lowercase source.[0] ->
            None
          | source when not include_uident && Char.is_uppercase source.[0] ->
            None
          | source ->
            try
              let ppf, to_string = Format.to_string () in
              if Type_utils.type_in_env ~verbosity env ppf source then
                Some (loc, `String (to_string ()), `No)
              else
                None
            with _ ->
              None
        )
    in
    let normalize ({Location. loc_start; loc_end}, text, _tail) =
        Lexing.split_pos loc_start, Lexing.split_pos loc_end, text in
    let all_items =
      List.merge_cons
        ~f:(fun a b ->
            (* Tail position is computed only on result, and result comes last
               As an approximation, when two items are similar, we returns the
               rightmost one *)
            if compare (normalize a) (normalize b) = 0 then Some b else None)
        (small_enclosings @ result)
    in
    Printtyp.wrap_printing_env env ~verbosity @@ fun () ->
    let ppf = Format.str_formatter in
    List.mapi all_items
      ~f:(fun i (loc,text,tail) ->
          let print = match index with None -> true | Some index -> index = i in
          let ret x = (loc, x, tail) in
          match text with
          | `String str -> ret (`String str)
          | `Type t when print ->
            Type_utils.print_type_with_decl ~verbosity env ppf t;
            ret (`String (Format.flush_str_formatter ()))
          | `Type_decl (id,t) when print ->
            Printtyp.type_declaration env id ppf t;
            ret (`String (Format.flush_str_formatter ()))
          | `Modtype m when print ->
            Printtyp.modtype env ppf m;
            ret (`String (Format.flush_str_formatter ()))
          | _ -> ret (`Index i)
        )

  | Enclosing pos ->
    with_typer pipeline @@ fun tr typer ->
    let structures = Mbrowse.of_typedtree (Mtyper.get_typedtree typer) in
    let pos = Msource.get_lexing_pos tr (Mpipeline.input_source pipeline) pos in
    let path = match Mbrowse.enclosing pos [structures] with
      | [] -> []
      | path -> List.map ~f:snd path
    in
    List.map ~f:Mbrowse.node_loc path

  | Complete_prefix (prefix, pos, with_doc, with_types) ->
    for_completion pipeline pos @@ fun tr pipeline typer ->
    let config = Mpipeline.final_config pipeline in
    let verbosity = Mconfig.(config.query.verbosity) in
    let no_labels = Mpipeline.reader_no_labels_for_completion pipeline in
    let source = Mpipeline.input_source pipeline in
    let pos = Msource.get_lexing_pos tr source pos in
    let path = Mtyper.node_at tr ~skip_recovered:true typer pos in
    let env, node = Mbrowse.leaf_node path in
    let target_type, context =
      Completion.application_context ~verbosity ~prefix path in
    let get_doc =
      if not with_doc then None else
        let local_defs = Mtyper.get_typedtree typer in
        let config = Mpipeline.final_config pipeline in
        Some (Track_definition.get_doc ~config ~env ~local_defs
                 ~comments:(Mpipeline.reader_comments pipeline) ~pos)
    in
    let entries =
      Printtyp.wrap_printing_env env ~verbosity @@ fun () ->
      Completion.node_complete config ?get_doc ?target_type env node prefix |>
      print_completion_entries ~with_types tr config source
    and context = match context with
      | `Application context when no_labels ->
        `Application {context with Compl.labels = []}
      | context -> context
    in
    {Compl. entries; context }

  | Expand_prefix (prefix, pos, with_types) ->
    for_completion pipeline pos @@ fun tr pipeline typer ->
    let source = Mpipeline.input_source pipeline in
    let pos = Msource.get_lexing_pos tr source pos in
    let env, _ = Mbrowse.leaf_node (Mtyper.node_at tr typer pos) in
    let config = Mpipeline.final_config pipeline in
    let global_modules = Mconfig.global_modules config in
    let entries =
      Completion.expand_prefix env ~global_modules prefix |>
      print_completion_entries ~with_types tr config source
    in
    { Compl. entries ; context = `Unknown }

  | Document (patho, pos) ->
    with_typer pipeline @@ fun tr typer ->
    let local_defs = Mtyper.get_typedtree typer in
    let source = Mpipeline.input_source pipeline in
    let pos = Msource.get_lexing_pos tr (Mpipeline.input_source pipeline) pos in
    let comments = Mpipeline.reader_comments pipeline in
    let env, _ = Mbrowse.leaf_node (Mtyper.node_at tr typer pos) in
    let path =
      match patho with
      | Some p -> p
      | None ->
        let path = Mreader_lexer.reconstruct_identifier source pos in
        let path = Mreader_lexer.identifier_suffix path in
        let path = List.map ~f:(fun {Location. txt} -> txt) path in
        String.concat ~sep:"." path
    in
    if path = "" then `Invalid_context else
      Track_definition.get_doc ~config:(Mpipeline.final_config pipeline)
        ~env ~local_defs ~comments ~pos (`User_input path)

  | Locate (patho, ml_or_mli, pos) ->
    with_typer pipeline @@ fun tr typer ->
    let local_defs = Mtyper.get_typedtree typer in
    let config = Mpipeline.input_config pipeline in
    let source = Mpipeline.input_source pipeline in
    let pos = Msource.get_lexing_pos tr source pos in
    let env, _ = Mbrowse.leaf_node (Mtyper.node_at tr typer pos) in
    let path =
      match patho with
      | Some p -> p
      | None ->
        let path = Mreader.reconstruct_identifier tr config source pos in
        let path = Mreader_lexer.identifier_suffix path in
        let path = List.map ~f:(fun {Location. txt} -> txt) path in
        String.concat ~sep:"." path
    in
    if path = "" then `Invalid_context else
    begin match
        Track_definition.from_string
          ~config:(Mpipeline.final_config pipeline)
          ~env ~local_defs ~pos ml_or_mli path
    with
    | `Found (file, pos) ->
      Logger.log "track_definition" "Locate"
        (Option.value ~default:"<local buffer>" file);
      `Found (file, pos)
    | otherwise -> otherwise
    end

  | Jump (target, pos) ->
    with_typer pipeline @@ fun tr typer ->
    let typedtree = Mtyper.get_typedtree typer in
    let pos = Msource.get_lexing_pos tr (Mpipeline.input_source pipeline) pos in
    Jump.get typedtree pos target

  | Phrase (target, pos) ->
    with_typer pipeline @@ fun tr typer ->
    let source = Mpipeline.input_source pipeline in
    let typedtree = Mtyper.get_typedtree typer in
    let pos = Msource.get_lexing_pos tr source pos in
    Msource.get_lexing_pos tr source (Jump.phrase typedtree pos target)

  | Case_analysis (pos_start, pos_end) ->
    with_typer pipeline @@ fun tr typer ->
    let source = Mpipeline.input_source pipeline in
    let pos_start = Msource.get_lexing_pos tr source pos_start in
    let pos_end = Msource.get_lexing_pos tr source pos_end in
    let browse = Mbrowse.of_typedtree (Mtyper.get_typedtree typer) in
    let nodes = Mbrowse.enclosing pos_start [browse] in
    let dump_node (_,node) =
      let {Location. loc_start; loc_end; _} =
        Mbrowse.node_loc node in
      let l1,c1 = Lexing.split_pos loc_start in
      let l2,c2 = Lexing.split_pos loc_end in
      `List [
         `String (Browse_raw.string_of_node node);
         `Int l1; `Int c1;
         `Int l2; `Int c2;
       ]
    in
    Logger.logj "destruct" "nodes before" (fun () -> `List (List.map nodes ~f:dump_node));
    let nodes =
      List.drop_while nodes
        ~f:(fun (_,t) ->
          let {Location. loc_start; loc_end} = Mbrowse.node_loc t in
          Lexing.compare_pos loc_start pos_start > 0 || Lexing.compare_pos loc_end pos_end < 0)
    in
    Logger.logj "destruct" "nodes after" (fun () -> `List (List.map nodes ~f:dump_node));
    begin match nodes with
      | [] -> failwith "No node at given range"
      | (env,node) :: parents ->
        let source = Mpipeline.input_source pipeline in
        let config = Mpipeline.final_config pipeline in
        let verbosity = verbosity pipeline in
        Printtyp.wrap_printing_env env ~verbosity @@ fun () ->
        Destruct.node tr config source node (List.map ~f:snd parents)
    end

  | Outline ->
    with_typer pipeline @@ fun tr typer ->
    let browse = Mbrowse.of_typedtree (Mtyper.get_typedtree typer) in
    Outline.get [Browse_tree.of_browse browse]

  | Shape pos ->
    with_typer pipeline @@ fun tr typer ->
    let browse = Mbrowse.of_typedtree (Mtyper.get_typedtree typer) in
    let pos = Msource.get_lexing_pos tr (Mpipeline.input_source pipeline) pos in
    Outline.shape pos [Browse_tree.of_browse browse]

  | Errors ->
    with_typer pipeline @@ fun tr typer ->
    let verbosity = verbosity pipeline in
    Printtyp.wrap_printing_env (Mtyper.get_env typer) ~verbosity @@ fun () ->
    let lexer_errors  = Mpipeline.reader_lexer_errors pipeline  in
    let parser_errors = Mpipeline.reader_parser_errors pipeline in
    let typer_errors  = Mpipeline.typer_errors pipeline  in
    (* When there is a cmi error, we will have a lot of meaningless errors,
       there is no need to report them. *)
    let typer_errors =
      let cmi_error = function Cmi_format.Error _ -> true | _ -> false in
      match List.find typer_errors ~f:cmi_error with
      | e -> [e]
      | exception Not_found -> typer_errors
    in
    let error_loc (e : Location.error) = e.Location.loc in
    let error_start e = (error_loc e).Location.loc_start in
    let error_end e = (error_loc e).Location.loc_end in
    (* Turn into Location.error, ignore ghost warnings *)
    let filter_error exn =
      match Location.error_of_exn exn with
      | None -> None
      | Some (err : Location.error) as result ->
        if Location.(err.loc.loc_ghost) &&
           (match exn with Msupport.Warning _ -> true | _ -> false)
        then None
        else result
    in
    let lexer_errors  = List.filter_map ~f:filter_error lexer_errors in
    let typer_errors  = List.filter_map ~f:filter_error typer_errors in
    (* Track first parsing error *)
    let first_parser_error = ref Lexing.dummy_pos in
    let filter_parser_error = function
      | Msupport.Warning _ as exn -> filter_error exn
      | exn ->
        let result = filter_error exn in
        begin match result with
          | None -> ();
          | Some err ->
            if !first_parser_error = Lexing.dummy_pos ||
               Lexing.compare_pos !first_parser_error (error_start err) > 0
            then first_parser_error := error_start err;
        end;
        result
    in
    let parser_errors = List.filter_map ~f:filter_parser_error parser_errors in
    (* Sort errors *)
    let cmp e1 e2 =
      let n = Lexing.compare_pos (error_start e1) (error_start e2) in
      if n <> 0 then n else
        Lexing.compare_pos (error_end e1) (error_end e2)
    in
    let errors = List.sort_uniq ~cmp
        (lexer_errors @ parser_errors @ typer_errors) in
    (* Filter anything after first parse error *)
    let limit = !first_parser_error in
    if limit = Lexing.dummy_pos then errors else (
      List.take_while errors
        ~f:(fun err -> Lexing.compare_pos (error_start err) limit <= 0)
    )

  | Dump args -> dump pipeline args

  | Path_of_source xs ->
    let config = Mpipeline.final_config pipeline in
    let rec aux = function
      | [] -> raise Not_found
      | x :: xs ->
        try
          find_in_path_uncap (Mconfig.source_path config) x
        with Not_found -> try
            find_in_path_uncap (Mconfig.build_path config) x
          with Not_found ->
            aux xs
    in
    aux xs

  | List_modules exts ->
    let config = Mpipeline.final_config pipeline in
    let with_ext ext = modules_in_path ~ext
        Mconfig.(config.merlin.source_path) in
    List.concat_map ~f:with_ext exts

  | Findlib_list ->
    let config = Mpipeline.final_config pipeline in
    let {Mconfig. conf; path} = config.Mconfig.findlib in
    Mconfig_dot.list_packages ?conf ~path ()

  | Extension_list kind ->
    let config = Mpipeline.final_config pipeline in
    let enabled = Mconfig.(config.merlin.extensions) in
    begin match kind with
    | `All -> Extension.all
    | `Enabled -> enabled
    | `Disabled ->
      List.fold_left ~f:(fun exts ext -> List.remove ext exts)
        ~init:Extension.all enabled
    end

  | Path_list `Build ->
    let config = Mpipeline.final_config pipeline in
    Mconfig.(config.merlin.build_path)

  | Path_list `Source ->
    let config = Mpipeline.final_config pipeline in
    Mconfig.(config.merlin.source_path)

  | Occurrences (`Ident_at pos) ->
    with_typer pipeline @@ fun tr typer ->
    let str = Mbrowse.of_typedtree (Mtyper.get_typedtree typer) in
    let pos = Msource.get_lexing_pos tr (Mpipeline.input_source pipeline) pos in
    let tnode = match Mbrowse.enclosing pos [str] with
      | [] -> Browse_tree.dummy
      | t -> Browse_tree.of_browse t
    in
    let str = Browse_tree.of_browse str in
    let get_loc {Location.txt = _; loc} = loc in
    let ident_occurrence () =
      let paths = Browse_raw.node_paths tnode.Browse_tree.t_node in
      let under_cursor p = Location_aux.compare_pos pos (get_loc p) = 0 in
      Logger.logj "occurrences" "Occurrences paths" (fun () ->
          let dump_path ({Location.txt; loc} as p) =
            let ppf, to_string = Format.to_string () in
            Printtyp.path ppf txt;
            `Assoc [
              "start", Lexing.json_of_position loc.Location.loc_start;
              "end", Lexing.json_of_position loc.Location.loc_end;
              "under_cursor", `Bool (under_cursor p);
              "path", `String (to_string ())
            ]
          in
          `List (List.map ~f:dump_path paths));
      match List.filter paths ~f:under_cursor with
      | [] -> []
      | (path :: _) ->
        let path = path.Location.txt in
        let ts = Browse_tree.all_occurrences path str in
        let loc (_t,paths) = List.map ~f:get_loc paths in
        List.concat_map ~f:loc ts

    and constructor_occurrence d =
      let ts = Browse_tree.all_constructor_occurrences (tnode,d) str in
      List.map ~f:get_loc ts

    in
    let locs =
      match Browse_raw.node_is_constructor tnode.Browse_tree.t_node with
      | Some d -> constructor_occurrence d.Location.txt
      | None -> ident_occurrence ()
    in
    let loc_start l = l.Location.loc_start in
    let cmp l1 l2 = Lexing.compare_pos (loc_start l1) (loc_start l2) in
    List.sort ~cmp locs

  | Version ->
    Printf.sprintf "The Merlin toolkit version %s, for Ocaml %s\n"
      My_config.version Sys.ocaml_version;
