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
open Browse_tree
open Browse_raw

open Cmt_cache
module Trie = struct
  include String.Map

  let get k t = find k t

  exception Found of string * Location.t * string option * namespace * node

  let find f t =
    try
      iter t ~f:(fun ~key ~data ->
        List.iter data ~f:(fun (l, doc, ns, node) ->
          if f key l doc ns node then
            raise (Found (key, l, doc, ns, node))
        )
      );
      raise Not_found
    with
    | Found (k, l, d, n, v) -> (k, l, d, n, v)

  let find_some f t =
    try Some (find f t)
    with Not_found -> None
end

let path_to_string (p : path) =
  let p =
    List.map p ~f:(function
      | (str, `Mod) -> str
      | (str, `Functor) -> str ^ "[functor]"
      | (str,`Labels) -> str ^ "[label]"
      | (str,`Constr) -> str ^ "[cstr]"
      | (str,`Type) -> str ^ "[type]"
      | (str,`Vals) -> str ^ "[val]"
      | (str,`Modtype) -> str ^ "[Mty]"
      | (str, `Unknown) -> str ^ "[?]"
    )
  in
  String.concat ~sep:"." p

let extract_doc (attrs : Parsetree.attributes) =
  String.concat ~sep:"\n" (
    List.filter_map attrs ~f:(fun attr ->
      Option.map ~f:fst (Type_utils.read_doc_attributes [attr])
    )
  )

type t = trie

(* See mli for documentation. *)
type result =
  | Found of Location.t * string option
  | Alias_of of Location.t * path
  | Resolves_to of path * Location.t option

let rec remove_top_indir =
  List.concat_map ~f:(fun bt ->
    match bt.t_node with
    | Signature _
    | Structure _ -> Lazy.force bt.t_children
    | _ -> [ bt ]
  )

let of_structure s =
  let env, node = Mbrowse.leaf_node (Mbrowse.of_structure s) in
  Browse_tree.of_node ~env node

let of_signature s =
  let env, node = Mbrowse.leaf_node (Mbrowse.of_signature s) in
  Browse_tree.of_node ~env node

let remove_indir_me me =
  match me.Typedtree.mod_desc with
  | Typedtree.Tmod_ident (path, _) -> `Alias path
  | Typedtree.Tmod_structure str -> `Str str
  | Typedtree.Tmod_functor (_param_id, param_name, _param_sig, me) ->
    `Functor (param_name, me.Typedtree.mod_loc, `Mod_expr me)
  | Typedtree.Tmod_apply (me1, me2, _) -> `Apply (me1, me2)
  | Typedtree.Tmod_constraint (me, _, _, _) -> `Mod_expr me
  | Typedtree.Tmod_unpack _ -> `Unpack

let remove_indir_mty mty =
  match mty.Typedtree.mty_desc with
  | Typedtree.Tmty_alias (path, _)
  | Typedtree.Tmty_ident (path, _) -> `Alias path
  | Typedtree.Tmty_signature sg -> `Sg sg
  | Typedtree.Tmty_functor (_param_id, param_name, _param_sig, mty) ->
    `Functor (param_name, mty.Typedtree.mty_loc, `Mod_type mty)
  | Typedtree.Tmty_with (mty, _) -> `Mod_type mty
  | Typedtree.Tmty_typeof me -> `Mod_expr me

let sig_item_idns =
  let open Types in function
  | Sig_value (id, _) -> id, `Vals
  | Sig_type (id, _, _) -> id, `Type
  | Sig_typext (id, _, _) -> id, `Type
  | Sig_module (id, _, _) -> id, `Mod
  | Sig_modtype (id, _) -> id, `Modtype
  | Sig_class (id, _, _) -> id, `Vals (* that's just silly *)
  | Sig_class_type (id, _, _) -> id, `Type (* :_D *)

let include_idents l = List.map sig_item_idns l

let identify_str_includes item =
  match item.Typedtree.str_desc with
  | Typedtree.Tstr_include { Typedtree. incl_type ; incl_mod } ->
    `Included (include_idents incl_type, `Mod_expr incl_mod)
  | _ -> `Not_included

let identify_sig_includes item =
  match item.Typedtree.sig_desc with
  | Typedtree.Tsig_include { Typedtree. incl_type ; incl_mod } ->
    `Included (include_idents incl_type, `Mod_type incl_mod)
  | _ -> `Not_included

let rec pattern_idlocs pat =
  let open Typedtree in
  match pat.pat_desc with
  | Tpat_var (id, _) -> [ Ident.name id , pat.pat_loc ]
  | Tpat_alias (p, id, _) -> (Ident.name id, pat.pat_loc) :: pattern_idlocs p
  | Tpat_tuple patts
  | Tpat_array patts
  | Tpat_construct (_, _, patts) ->
    List.concat_map patts ~f:pattern_idlocs
  | Tpat_record (lst, _) ->
    List.map lst ~f:(fun (lid_loc, _, _pattern) ->
      Longident.last lid_loc.Asttypes.txt, lid_loc.Asttypes.loc
    ) (* TODO: handle rhs, i.e. [_pattern] *)
  | Tpat_variant (_, Some pat, _) -> pattern_idlocs pat
  | _ -> []

let rec tag_path ~namespace = function
  | [] -> invalid_arg "Typedtrie.tag_path"
  | [ x ] -> [ x, namespace ]
  | x :: xs -> (x, `Mod) :: tag_path ~namespace xs

let rec build ?(local_buffer=false) ~trie browses =
  let rec node_for_direct_mod namespace = function
    | `Alias path ->
      let p = Path_aux.to_string_list path in
      Alias (tag_path ~namespace p)
    | `Str s ->
      Internal (build ~local_buffer ~trie:Trie.empty [of_structure s])
    | `Sg s ->
      Internal (build ~local_buffer ~trie:Trie.empty [of_signature s])
    | `Mod_expr me -> node_for_direct_mod `Mod (remove_indir_me me)
    | `Mod_type mty -> node_for_direct_mod `Modtype (remove_indir_mty mty)
    | `Functor (located_name, pack_loc, packed) when local_buffer ->
      (* We don't actually care about the namespace here. But whatever. *)
      let result = [ pack_loc, None, `Functor, node_for_direct_mod `Functor packed ] in
      let param  = [ located_name.Asttypes.loc, None, `Modtype, Leaf ] in
      let trie =
        Trie.of_list [
          located_name.Asttypes.txt, param;
          "0", result;
        ]
      in
      Internal trie
    | `Apply (me1, me2) ->
      let node1 = node_for_direct_mod `Mod (remove_indir_me me1) in
      let node2 = node_for_direct_mod `Mod (remove_indir_me me2) in
      let trie  =
        Trie.of_list [
          "1", [ me1.Typedtree.mod_loc, None, `Mod, node1 ];
          "2", [ me2.Typedtree.mod_loc, None, `Mod, node2 ];
        ]
      in
      Internal trie
    | `Unpack | `Functor _ -> (* TODO! *)
      Leaf
  in
  let rec collect_local_modules trie nodes =
    let aux trie t =
      let doc =
        let attrs = node_attributes t.t_node in
        let doc = extract_doc attrs in
        if doc = "" then None else Some doc
      in
      match t.t_node with
      (* Traverse expressions (there are no "match" nodes, only cases). *)
      | Case _
      | Expression _
      (* Traverse [let .. in], no need to create node for these, if we were
         looking for a local binding, its location would have been in the
         environment. *)
      | Value_binding _ -> collect_local_modules trie t.t_children
      (* Record local module bindings *)
      | Module_binding mb ->
        let node =
          node_for_direct_mod `Mod
            (remove_indir_me mb.Typedtree.mb_expr)
        in
        Trie.add_multiple (Ident.name mb.Typedtree.mb_id)
          (t.t_loc, doc, `Mod, node) trie
      (* Ignore patterns. *)
      | _ -> trie
    in
    List.fold_left (Lazy.force nodes) ~init:trie ~f:aux
  in
  List.fold_left (remove_top_indir browses) ~init:trie ~f:(fun trie t ->
    let open Typedtree in
    let doc =
      let attrs = node_attributes t.t_node in
      let doc = extract_doc attrs in
      if doc = "" then None else Some doc
    in
    match t.t_node with
    | Signature _
    | Structure _ ->
      (* Removed by [get_top_items] *)
      assert false
    | Signature_item _
    | Structure_item _ ->
      begin match
        match t.t_node with
        | Signature_item (item, _) -> identify_sig_includes item
        | Structure_item (item, _) -> identify_str_includes item
        | _ -> assert false
      with
      | `Not_included -> build ~local_buffer ~trie (Lazy.force t.t_children)
      | `Included (included_idents, packed) ->
        let rec helper packed =
          let f data =
            List.fold_left included_idents ~init:trie ~f:(fun trie (id, ns) ->
              let data = (t.t_loc, None, ns, data) in
              Trie.add_multiple (Ident.name id) data trie
            )
          in
          match
            match packed with
            | `Mod_expr me -> remove_indir_me  me
            | `Mod_type mt -> remove_indir_mty mt
          with
          | `Alias path ->
            let namespace =
              match packed with
              | `Mod_expr _ -> `Mod
              | `Mod_type _ -> `Modtype
            in
            let p = tag_path ~namespace (Path_aux.to_string_list path) in
            f (Included p)
          | `Mod_type _
          | `Mod_expr _ as packed -> helper packed
          | `Functor _ ->
            (* You can't include a functor, you can only include "structures". *)
            assert false
          | `Unpack
          | `Apply _ -> f Leaf
          | `Str str -> build ~local_buffer ~trie [of_structure str]
          | `Sg  sg -> build ~local_buffer ~trie [of_signature sg]
        in
        helper packed
      end
    | Value_binding vb ->
      (* The following doesn't seem quite correct wrt documentation. Oh well. *)
      let idlocs = pattern_idlocs vb.vb_pat in
      List.fold_left idlocs ~init:trie ~f:(fun trie (id, loc) ->
        if local_buffer then
          let children = collect_local_modules Trie.empty t.t_children in
          if Trie.is_empty children then
            Trie.add_multiple id (t.t_loc, doc, `Vals, Leaf) trie
          else
            Trie.add_multiple id (t.t_loc, doc, `Vals, Internal children) trie
        else
          Trie.add_multiple id (loc, doc, `Vals, Leaf) trie
      )
    | Value_description vd ->
      Trie.add_multiple (Ident.name vd.val_id) (t.t_loc, doc, `Vals, Leaf) trie
    | Module_binding mb ->
      let node =
        node_for_direct_mod `Mod
          (remove_indir_me mb.mb_expr)
      in
      Trie.add_multiple (Ident.name mb.mb_id) (t.t_loc, doc, `Mod, node) trie
    | Module_declaration md ->
      let node =
        node_for_direct_mod `Mod
          (remove_indir_mty md.md_type)
      in
      Trie.add_multiple (Ident.name md.md_id) (t.t_loc, doc, `Mod, node) trie
    | Module_type_declaration mtd ->
      let node =
        match mtd.mtd_type with
         None -> Leaf
        | Some m -> node_for_direct_mod `Modtype (remove_indir_mty m)
      in
      Trie.add_multiple (Ident.name mtd.mtd_id) (t.t_loc, doc, `Modtype, node) trie
    | Type_declaration td ->
      (* TODO: add constructors and labels as well.
         Because why the hell not. *)
      Trie.add_multiple (Ident.name td.typ_id) (t.t_loc, doc, `Type, Leaf) trie
    | Type_extension te ->
      (* TODO: add constructors and labels as well.
         Because why the hell not. *)
      Trie.add_multiple (Path.last te.tyext_path) (t.t_loc, doc, `Type, Leaf) trie
    | ignored_node ->
      Logger.log "typedtrie" "ignored node"
        (string_of_node ignored_node);
      trie
  )

let of_browses = build ~trie:Trie.empty

let rec follow ?before trie = function
  | [] -> invalid_arg "Typedtrie.follow"
  | (x, namespace) :: xs as path ->
    try
      let lst = Trie.get x trie in
      let lst =
        List.filter lst ~f:(fun (_, _, ns, _) -> ns = namespace || ns = `Unknown)
      in
      let lst =
        match before with
        | None -> lst
        | Some before ->
          List.filter lst ~f:(fun (l1, _, _, _) ->
            Lexing.compare_pos l1.Location.loc_start before < 0)
      in
      match
        List.sort lst ~cmp:(fun (l1, _, _, _) (l2, _, _, _) ->
          (* We wants the ones closed last to be at the beginning of the list. *)
          Lexing.compare_pos l2.Location.loc_end l1.Location.loc_end)
      with
      | [] -> Resolves_to (path, None)
      | (loc, doc, _, Leaf) :: _ ->
        (* FIXME: it seems wrong to return [Resolves_to] here.
           The prefix of the path is a leaf, anything else we might look up will
           be *wrong* *)
        if xs = [] then Found (loc, doc) else Resolves_to (path, None)
      | (loc, _, _, Alias path) :: _ ->
        begin match xs with
        | [] ->
          (* FIXME: at this point, we might be deep in the trie, and [path]
             might only make sense for a few steps, but in the upper nodes it
             might need to be prefixed.
             We need to recurse like we do for [Resolves_to] *)
          Alias_of (loc, path)
        | _ ->
          let new_path = path @ xs in
          begin match follow ~before:loc.Location.loc_start trie new_path with
          | Resolves_to (p, None) -> Resolves_to (p, Some loc)
          | otherwise -> otherwise
          end
        end
      | (loc, _, _, Included p) :: _ ->
        let new_path = p @ path in
          begin match follow ~before:loc.Location.loc_start trie new_path with
          | Resolves_to (p, None) -> Resolves_to (p, Some loc)
          | otherwise -> otherwise
          end
      | (l, doc, _, Internal t) :: _ ->
        if xs = [] then Found (l, doc) else
          match follow ?before t xs with
          | Resolves_to (p, None) when p = xs -> Found (l, doc) (* questionable *)
          | Resolves_to (p, x) as checkpoint ->
            begin match follow ~before:l.Location.loc_start trie p with
            (* This feels wrong *)
            | Resolves_to (_, None) -> checkpoint
            | otherwise -> otherwise
            end
          | otherwise -> otherwise
    with
    | Not_found ->
      Resolves_to (path, None)

let dump_namespace fmt namespace =
  Format.pp_print_string fmt
    (match namespace with
     | `Mod -> "(Mod) "
     | `Functor -> "(functor)"
     | `Labels -> "(lbl) "
     | `Constr -> "(cstr) "
     | `Type -> "(typ) "
     | `Vals -> "(val) "
     | `Modtype -> "(Mty) "
     | `Unknown -> "(?)")

let rec find ~before trie path =
  match
    Trie.find_some (fun _name loc _docopt _namespace _node ->
      Lexing.compare_pos loc.Location.loc_start before < 0
      && Lexing.compare_pos loc.Location.loc_end before > 0
    ) trie
  with
  | Some (_name, loc, _docopt, _namespace, Internal subtrie) ->
    begin match find ~before subtrie path with
    | Resolves_to (p, x) as checkpoint ->
      begin match follow ~before:loc.Location.loc_start trie p with
      | Resolves_to (_, None) -> checkpoint
      | otherwise -> otherwise
      end
    | otherwise -> otherwise
    end
  | Some (name, loc, _docopt, _namespace, _) ->
    Logger.log "locate" "Typedtrie.find"
      "cursor is in a leaf, so we look only before the leaf" ;
    follow ~before:loc.Location.loc_start trie path
  | _ -> follow ~before trie path

let find ?before trie path =
  match before with
  | None -> follow trie path
  | Some before -> find ~before trie path

let rec dump fmt trie =
  let dump_node (loc, _doc_opt, namespace, node) =
    dump_namespace fmt namespace;
    match node with
    | Leaf -> Location.print_loc fmt loc
    | Included path ->
      Format.fprintf fmt "%a <%s>" Location.print_loc loc
        (path_to_string path)
    | Alias path ->
      Format.fprintf fmt "%a = %s" Location.print_loc loc
        (path_to_string path)
    | Internal t ->
      Format.fprintf fmt "%a = %a" Location.print_loc loc dump t
  in
  Format.pp_print_string fmt "{\n" ;
  Trie.iter trie ~f:(fun ~key ~data:nodes ->
    Format.fprintf fmt "%s -> " key ;
    begin match nodes with
    | [] -> assert false
    | [ x ] -> dump_node x
    | lst ->
      Format.pp_print_string fmt "[\n" ;
      List.iter lst ~f:(fun x ->
        dump_node x ;
        Format.pp_print_newline fmt ()
      )
    end ;
    Format.pp_print_newline fmt ()
  );
  Format.pp_print_string fmt "}\n"
