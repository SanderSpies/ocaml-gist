(* {{{ Copying *(

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

type constructor_declaration = Typedtree.constructor_declaration

open Typedtree

type node =
  | Dummy
  | Pattern                  of pattern
  | Expression               of expression
  | Case                     of case
  | Class_expr               of class_expr
  | Class_structure          of class_structure
  | Class_field              of class_field
  | Class_field_kind         of class_field_kind
  | Module_expr              of module_expr
  | Module_type_constraint   of module_type_constraint
  | Structure                of structure
  | Signature                of signature
  | Structure_item           of structure_item * Env.t
  | Signature_item           of signature_item * Env.t
  | Module_binding           of module_binding
  | Value_binding            of value_binding
  | Module_type              of module_type
  | Module_declaration       of module_declaration
  | Module_type_declaration  of module_type_declaration
  | With_constraint          of with_constraint
  | Core_type                of core_type
  | Package_type             of package_type
  | Row_field                of row_field
  | Value_description        of value_description
  | Type_declaration         of type_declaration
  | Type_kind                of type_kind
  | Type_extension           of type_extension
  | Extension_constructor    of extension_constructor
  | Label_declaration        of label_declaration
  | Constructor_declaration  of constructor_declaration
  | Class_type               of class_type
  | Class_signature          of class_signature
  | Class_type_field         of class_type_field
  | Class_declaration        of class_declaration
  | Class_description        of class_description
  | Class_type_declaration   of class_type_declaration

  | Include_description      of include_description
  | Include_declaration      of include_declaration
  | Open_description         of open_description

  | Method_call              of expression * meth * Location.t
  | Module_binding_name      of module_binding
  | Module_declaration_name  of module_declaration
  | Module_type_declaration_name of module_type_declaration

let node_update_env env0 = function
  | Pattern        {pat_env = env}  | Expression     {exp_env = env}
  | Class_expr     {cl_env = env}   | Method_call    ({exp_env = env}, _, _)
  | Module_expr    {mod_env = env}  | Module_type    {mty_env = env}
  | Structure_item (_, env)         | Signature_item (_, env)
  | Core_type      {ctyp_env = env} | Class_type     {cltyp_env = env}
    -> env
  | Dummy                     | Case                    _
  | Class_structure         _ | Class_signature         _
  | Class_field             _ | Class_field_kind        _
  | Type_extension          _ | Extension_constructor   _
  | Package_type            _ | Row_field               _
  | Type_declaration        _ | Type_kind               _
  | Module_binding          _ | Module_declaration      _
  | Module_binding_name     _ | Module_declaration_name _
  | Module_type_declaration _ | Module_type_constraint  _
  | Module_type_declaration_name _ | With_constraint    _
  | Structure               _ | Signature               _
  | Value_description       _ | Value_binding           _
  | Constructor_declaration _ | Label_declaration       _
  | Class_declaration       _ | Class_description       _
  | Class_type_declaration  _ | Class_type_field        _
  | Include_description     _ | Include_declaration     _
  | Open_description        _
    -> env0

let node_real_loc loc0 = function
  | Expression              {exp_loc = loc}
  | Pattern                 {pat_loc = loc}
  | Method_call             (_, _, loc)
  | Class_expr              {cl_loc = loc}
  | Module_expr             {mod_loc = loc}
  | Signature_item          ({sig_loc = loc}, _)
  | Structure_item          ({str_loc = loc}, _)
  | Module_type             {mty_loc = loc}
  | Core_type               {ctyp_loc = loc}
  | Class_type              {cltyp_loc = loc}
  | Class_field             {cf_loc = loc}
  | Module_binding          {mb_loc = loc}
  | Module_declaration      {md_loc = loc}
  | Module_type_declaration {mtd_loc = loc}
  | Value_description       {val_loc = loc}
  | Value_binding           {vb_loc = loc}
  | Type_declaration        {typ_loc = loc}
  | Label_declaration       {ld_loc = loc}
  | Constructor_declaration {cd_loc = loc}
  | Class_type_field        {ctf_loc = loc}
  | Class_declaration       {ci_loc = loc}
  | Class_description       {ci_loc = loc}
  | Class_type_declaration  {ci_loc = loc}
  | Extension_constructor   {ext_loc = loc}
  | Include_description     {incl_loc = loc}
  | Include_declaration     {incl_loc = loc}
  | Open_description        {open_loc = loc}
    -> loc
  | Module_binding_name          {mb_name = loc}
  | Module_declaration_name      {md_name = loc}
  | Module_type_declaration_name {mtd_name = loc}
    -> loc.Location.loc
  | Structure _ | Signature _ | Case _ | Class_structure _ | Type_extension _
  | Class_field_kind _ | Module_type_constraint _ | With_constraint _
  | Row_field _ | Type_kind _ | Class_signature _ | Package_type _
  | Dummy
    -> loc0

let node_merlin_loc loc0 = function
  | Expression {exp_loc = loc; exp_attributes} ->
    begin try
        let s, _ = List.find exp_attributes ~f:(fun (s,_) -> s.Location.txt = "merlin.loc") in
        s.Location.loc
      with Not_found -> loc
    end
  | node -> node_real_loc loc0 node

let app node env f acc =
  f (node_update_env env node)
    node acc

type 'a f0 = Env.t -> node -> 'a -> 'a
type ('b,'a) f1 = 'b -> Env.t -> 'a f0 -> 'a -> 'a

let id_fold _env (_f : _ f0) acc = acc

let ( ** ) f1 f2 env (f : _ f0) acc =
  f2 env f (f1 env f acc)

let rec list_fold (f' : _ f1) xs env f acc = match xs with
  | x :: xs -> list_fold f' xs env f (f' x env f acc)
  | [] -> acc

let rec list_fold_with_next (f' : _ -> _ f1) xs env f acc = match xs with
  | x :: (y :: _ as xs) -> list_fold_with_next f' xs env f (f' (Some y) x env f acc)
  | [x] -> f' None x env f acc
  | [] -> acc

let option_fold f' o env (f : _ f0) acc = match o with
  | None -> acc
  | Some x -> f' x env f acc

let of_expression e = app (Expression e)
let of_case c = app (Case c)
let of_pattern p = app (Pattern p)
let of_core_type ct = app (Core_type ct)
let of_value_binding vb = app (Value_binding vb)
let of_module_type mt = app (Module_type mt)
let of_module_expr me = app (Module_expr me)
let of_typ_param (ct,_) = of_core_type ct

let of_pat_extra (pat,_,_) = match pat with
  | Tpat_constraint ct -> of_core_type ct
  | Tpat_type _ | Tpat_unpack -> id_fold

let of_pattern_desc = function
  | Tpat_any | Tpat_var _ | Tpat_constant _ | Tpat_variant (_,None,_) -> id_fold
  | Tpat_alias (p,_,_) | Tpat_variant (_,Some p,_) | Tpat_lazy p -> of_pattern p
  | Tpat_tuple ps | Tpat_construct (_,_,ps) | Tpat_array ps ->
    list_fold of_pattern ps
  | Tpat_record (ls,_) ->
    list_fold (fun (_,_,p) -> of_pattern p) ls
  | Tpat_or (p1,p2,_) ->
    of_pattern p1 ** of_pattern p2

let of_method_call obj meth arg loc =
  fun env (f : _ f0) acc ->
  let loc_start = obj.exp_loc.Location.loc_end in
  let loc_end = match arg with
    | None -> loc.Location.loc_end
    | Some e -> e.exp_loc.Location.loc_start
  in
  let loc = {loc with Location. loc_start; loc_end} in
  app (Method_call (obj,meth,loc)) env f acc

let of_expression_desc loc = function
  | Texp_ident _ | Texp_constant _ | Texp_instvar _
  | Texp_variant (_,None) | Texp_new _ -> id_fold
  | Texp_let (_,vbs,e) ->
    of_expression e ** list_fold of_value_binding vbs
  | Texp_function (_,cs,_) ->
    list_fold of_case cs
  | Texp_apply (e,ls) ->
    of_expression e **
    list_fold (function
        | (_,None,_) -> id_fold
        | (_,Some e,_) -> of_expression e)
      ls
  | Texp_match (e,cs1,cs2,_) ->
    of_expression e **
    list_fold of_case cs1 **
    list_fold of_case cs2
  | Texp_try (e,cs) ->
    of_expression e **
    list_fold of_case cs
  | Texp_tuple es | Texp_construct (_,_,es) | Texp_array es ->
    list_fold of_expression es
  | Texp_variant (_,Some e) | Texp_field (e,_,_)
  | Texp_assert e | Texp_lazy e | Texp_setinstvar (_,_,_,e) ->
    of_expression e
  | Texp_record (ls,e) ->
    option_fold of_expression e **
    list_fold (fun (_,_,e) -> of_expression e) ls
  | Texp_setfield (e1,_,_,e2) | Texp_ifthenelse (e1,e2,None)
  | Texp_sequence (e1,e2) | Texp_while (e1,e2) ->
    of_expression e1 ** of_expression e2
  | Texp_ifthenelse (e1,e2,Some e3) | Texp_for (_,_,e1,e2,_,e3) ->
    of_expression e1 ** of_expression e2 ** of_expression e3
  | Texp_send (e,meth,eo) ->
    of_expression e **
    of_method_call e meth eo loc **
    option_fold of_expression eo
  | Texp_override (_,ls) ->
    list_fold (fun (_,_,e) -> of_expression e) ls
  | Texp_letmodule (mb_id, mb_name, mb_expr, e) ->
    let mb = {mb_id;mb_name;mb_expr;mb_loc=Location.none;mb_attributes=[]} in
    app (Module_binding mb) ** of_expression e
  | Texp_object (cs,_) ->
    app (Class_structure cs)
  | Texp_pack me ->
    of_module_expr me

and of_exp_extra (exp,_,_) = match exp with
  | Texp_constraint ct ->
    of_core_type ct
  | Texp_coerce (cto,ct) ->
    of_core_type ct ** option_fold of_core_type cto
  | Texp_poly cto ->
    option_fold of_core_type cto
  | Texp_open _ | Texp_newtype _ ->
    id_fold

and of_class_expr_desc = function
  | Tcl_ident (_,_,cts) ->
    list_fold of_core_type cts
  | Tcl_structure cs ->
    app (Class_structure cs)
  | Tcl_fun (_,p,es,ce,_) ->
    list_fold (fun (_,_,e) -> of_expression e) es **
    of_pattern p **
    app (Class_expr ce)
  | Tcl_apply (ce,es) ->
    list_fold (function
        | (_,None,_) -> id_fold
        | (_,Some e,_) -> of_expression e)
      es **
    app (Class_expr ce)
  | Tcl_let (_,vbs,es,ce) ->
    list_fold of_value_binding vbs **
    list_fold (fun (_,_,e) -> of_expression e) es **
    app (Class_expr ce)
  | Tcl_constraint (ce,cto,_,_,_) ->
    option_fold (fun ct -> app (Class_type ct)) cto **
    app (Class_expr ce)

and of_class_field_desc = function
  | Tcf_inherit (_,ce,_,_,_) ->
    app (Class_expr ce)
  | Tcf_val (_,_,_,cfk,_) | Tcf_method (_,_,cfk) ->
    app (Class_field_kind cfk)
  | Tcf_constraint (ct1,ct2) ->
    of_core_type ct1 ** of_core_type ct2
  | Tcf_initializer e ->
    of_expression e
  | Tcf_attribute _ ->
    id_fold (*TODO*)

and of_module_expr_desc = function
  | Tmod_ident _ -> id_fold
  | Tmod_structure str ->
    app (Structure str)
  | Tmod_functor (_,_,mto,me) ->
    option_fold of_module_type mto **
    of_module_expr me
  | Tmod_apply (me1,me2,_) ->
    of_module_expr me1 **
    of_module_expr me2
  | Tmod_constraint (me,_,mtc,_) ->
    of_module_expr me **
    app (Module_type_constraint mtc)
  | Tmod_unpack (e,_) ->
    of_expression e

and of_structure_item_desc = function
  | Tstr_eval (e,_) ->
    of_expression e
  | Tstr_value (_,vbs) ->
    list_fold of_value_binding vbs
  | Tstr_primitive vd ->
    app (Value_description vd)
  | Tstr_type tds ->
    list_fold (fun td -> app (Type_declaration td)) tds
  | Tstr_typext text ->
    app (Type_extension text)
  | Tstr_exception ec ->
    app (Extension_constructor ec)
  | Tstr_module mb ->
    app (Module_binding mb)
  | Tstr_recmodule mbs ->
    list_fold (fun x -> app (Module_binding x)) mbs
  | Tstr_modtype mtd ->
    app (Module_type_declaration mtd)
  | Tstr_class cds ->
    list_fold (fun (cd,_,_) -> app (Class_declaration cd)) cds
  | Tstr_class_type ctds ->
    list_fold (fun (_,_,ctd) -> app (Class_type_declaration ctd)) ctds
  | Tstr_include i ->
    app (Include_declaration i)
  | Tstr_open d ->
    app (Open_description d)
  | Tstr_attribute _ ->
    id_fold

and of_module_type_desc = function
  | Tmty_ident _ | Tmty_alias _ -> id_fold
  | Tmty_signature sg ->
    app (Signature sg)
  | Tmty_functor (_,_,mto,mt) ->
    option_fold of_module_type mto **
    of_module_type mt
  | Tmty_with (mt,wcs) ->
    list_fold (fun (_,_,wc) -> app (With_constraint wc)) wcs **
    of_module_type mt
  | Tmty_typeof me ->
    of_module_expr me

and of_signature_item_desc = function
  | Tsig_attribute _ ->
    id_fold
  | Tsig_open d ->
    app (Open_description d)
  | Tsig_value vd ->
    app (Value_description vd)
  | Tsig_type tds ->
    list_fold (fun td -> app (Type_declaration td)) tds
  | Tsig_typext text ->
    app (Type_extension text)
  | Tsig_exception ec ->
    app (Extension_constructor ec)
  | Tsig_module md ->
    app (Module_declaration md)
  | Tsig_recmodule mds ->
    list_fold (fun md -> app (Module_declaration md)) mds
  | Tsig_modtype mtd ->
    app (Module_type_declaration mtd)
  | Tsig_include i ->
    app (Include_description i)
  | Tsig_class cds ->
    list_fold (fun cd -> app (Class_description cd)) cds
  | Tsig_class_type ctds ->
    list_fold (fun ctd -> app (Class_type_declaration ctd)) ctds

and of_core_type_desc = function
  | Ttyp_any | Ttyp_var _ -> id_fold
  | Ttyp_arrow (_,ct1,ct2) ->
    of_core_type ct1 ** of_core_type ct2
  | Ttyp_tuple cts | Ttyp_constr (_,_,cts) | Ttyp_class (_,_,cts) ->
    list_fold of_core_type cts
  | Ttyp_object (cts,_) ->
    list_fold (fun (_,_,ct) -> of_core_type ct) cts
  | Ttyp_poly (_,ct) | Ttyp_alias (ct,_) ->
    of_core_type ct
  | Ttyp_variant (rfs,_,_) ->
    list_fold (fun rf -> app (Row_field rf)) rfs
  | Ttyp_package pt ->
    app (Package_type pt)

and of_class_type_desc = function
  | Tcty_constr (_,_,cts) ->
    list_fold of_core_type cts
  | Tcty_signature cs ->
    app (Class_signature cs)
  | Tcty_arrow (_,ct,clt) ->
    of_core_type ct ** app (Class_type clt)

and of_class_type_field_desc = function
  | Tctf_inherit ct ->
    app (Class_type ct)
  | Tctf_val (_,_,_,ct) | Tctf_method (_,_,_,ct) ->
    of_core_type ct
  | Tctf_constraint (ct1,ct2) ->
    of_core_type ct1 ** of_core_type ct2
  | Tctf_attribute _ ->
    id_fold

let of_node = function
  | Dummy -> id_fold
  | Pattern { pat_desc; pat_extra } ->
    of_pattern_desc pat_desc **
    list_fold of_pat_extra pat_extra
  | Expression { exp_desc; exp_extra; exp_loc } ->
    of_expression_desc exp_loc exp_desc **
    list_fold of_exp_extra exp_extra
  | Case { c_lhs; c_guard; c_rhs } ->
    of_pattern c_lhs ** of_expression c_rhs **
    option_fold of_expression c_guard
  | Class_expr { cl_desc } ->
    of_class_expr_desc cl_desc
  | Class_structure { cstr_self; cstr_fields } ->
    of_pattern cstr_self **
    list_fold (fun f -> app (Class_field f)) cstr_fields
  | Class_field { cf_desc } ->
    of_class_field_desc cf_desc
  | Class_field_kind (Tcfk_virtual ct) ->
    of_core_type ct
  | Class_field_kind (Tcfk_concrete (_,e)) ->
    of_expression e
  | Module_expr { mod_desc } ->
    of_module_expr_desc mod_desc
  | Module_type_constraint Tmodtype_implicit ->
    id_fold
  | Module_type_constraint (Tmodtype_explicit mt) ->
    of_module_type mt
  | Structure { str_items; str_final_env } ->
    list_fold_with_next (fun next item ->
        match next with
        | None -> app (Structure_item (item, str_final_env))
        | Some item' -> app (Structure_item (item, item'.str_env)))
      str_items
  | Structure_item ({ str_desc }, _) ->
    of_structure_item_desc str_desc
  | Module_binding mb ->
    app (Module_expr mb.mb_expr) **
    app (Module_binding_name mb)
  | Value_binding { vb_pat; vb_expr } ->
    of_pattern vb_pat **
    of_expression vb_expr
  | Module_type { mty_desc } ->
    of_module_type_desc mty_desc
  | Signature { sig_items; sig_final_env } ->
    list_fold_with_next (fun next item ->
        match next with
        | None -> app (Signature_item (item, sig_final_env))
        | Some item' -> app (Signature_item (item, item'.sig_env)))
      sig_items
  | Signature_item ({ sig_desc }, _) ->
    of_signature_item_desc sig_desc
  | Module_declaration md ->
    of_module_type md.md_type **
    app (Module_declaration_name md)
  | Module_type_declaration mtd ->
    option_fold of_module_type mtd.mtd_type **
    app (Module_type_declaration_name mtd)
  | With_constraint (Twith_type td | Twith_typesubst td) ->
    app (Type_declaration td)
  | With_constraint (Twith_module _ | Twith_modsubst _) ->
    id_fold
  | Core_type { ctyp_desc } ->
    of_core_type_desc ctyp_desc
  | Package_type { pack_fields } ->
    list_fold (fun (_,ct) -> of_core_type ct) pack_fields
  | Row_field (Ttag (_,_,_,cts)) ->
    list_fold of_core_type cts
  | Row_field (Tinherit ct) ->
    of_core_type ct
  | Value_description { val_desc } ->
    of_core_type val_desc
  | Type_declaration { typ_params; typ_cstrs; typ_kind; typ_manifest } ->
    let of_typ_cstrs (ct1,ct2,_) = of_core_type ct1 ** of_core_type ct2 in
    option_fold of_core_type typ_manifest **
    list_fold of_typ_param typ_params **
    app (Type_kind typ_kind) **
    list_fold of_typ_cstrs typ_cstrs
  | Type_kind (Ttype_abstract | Ttype_open) ->
    id_fold
  | Type_kind (Ttype_variant cds) ->
    list_fold (fun cd -> app (Constructor_declaration cd)) cds
  | Type_kind (Ttype_record lds) ->
    list_fold (fun ld -> app (Label_declaration ld)) lds
  | Type_extension { tyext_params; tyext_constructors } ->
    list_fold of_typ_param tyext_params **
    list_fold (fun ec -> app (Extension_constructor ec)) tyext_constructors
  | Extension_constructor { ext_kind = Text_decl (cts,cto) } ->
    option_fold of_core_type cto **
    list_fold of_core_type cts
  | Extension_constructor { ext_kind = Text_rebind _ } ->
    id_fold
  | Label_declaration { ld_type } ->
    of_core_type ld_type
  | Constructor_declaration { cd_args; cd_res } ->
    list_fold of_core_type (Option.cons cd_res cd_args)
  | Class_type { cltyp_desc } ->
    of_class_type_desc cltyp_desc
  | Class_signature { csig_self; csig_fields } ->
    of_core_type csig_self **
    list_fold (fun x -> app (Class_type_field x)) csig_fields
  | Class_type_field { ctf_desc } ->
    of_class_type_field_desc ctf_desc
  | Class_declaration { ci_params; ci_expr } ->
    app (Class_expr ci_expr) **
    list_fold of_typ_param ci_params
  | Class_description { ci_params; ci_expr } ->
    app (Class_type ci_expr) **
    list_fold of_typ_param ci_params
  | Class_type_declaration { ci_params; ci_expr } ->
    app (Class_type ci_expr) **
    list_fold of_typ_param ci_params
  | Method_call _ -> id_fold
  | Module_binding_name _ -> id_fold
  | Module_declaration_name _ -> id_fold
  | Module_type_declaration_name _ -> id_fold
  | Open_description _ -> id_fold
  | Include_declaration i ->
    of_module_expr i.incl_mod
  | Include_description i ->
    of_module_type i.incl_mod

let fold_node f env node acc =
  of_node node env f acc

(** Accessors for information specific to a node *)

let string_of_node = function
  | Dummy -> "dummy"
  | Pattern                 p ->
    (* let fmt, printer = Format.to_string () in
    Printtyped.pattern 0 fmt p ;
    printer () *)
    "not implemented"
  | Expression              _ -> "expression"
  | Case                    _ -> "case"
  | Class_expr              _ -> "class_expr"
  | Class_structure         _ -> "class_structure"
  | Class_field             _ -> "class_field"
  | Class_field_kind        _ -> "class_field_kind"
  | Module_expr             _ -> "module_expr"
  | Module_type_constraint  _ -> "module_type_constraint"
  | Structure               _ -> "structure"
  | Structure_item          _ -> "structure_item"
  | Module_binding          _ -> "module_binding"
  | Value_binding           _ -> "value_binding"
  | Module_type             _ -> "module_type"
  | Signature               _ -> "signature"
  | Signature_item          _ -> "signature_item"
  | Module_declaration      _ -> "module_declaration"
  | Module_type_declaration _ -> "module_type_declaration"
  | With_constraint         _ -> "with_constraint"
  | Core_type               _ -> "core_type"
  | Package_type            _ -> "package_type"
  | Row_field               _ -> "row_field"
  | Value_description       _ -> "value_description"
  | Type_declaration        _ -> "type_declaration"
  | Type_kind               _ -> "type_kind"
  | Type_extension          _ -> "type_extension"
  | Extension_constructor   _ -> "extension_constructor"
  | Label_declaration       _ -> "label_declaration"
  | Constructor_declaration _ -> "constructor_declaration"
  | Class_type              _ -> "class_type"
  | Class_signature         _ -> "class_signature"
  | Class_type_field        _ -> "class_type_field"
  | Class_declaration       _ -> "class_declaration"
  | Class_description       _ -> "class_description"
  | Class_type_declaration  _ -> "class_type_declaration"
  | Method_call             _ -> "method_call"
  | Module_binding_name     _ -> "module_binding_name"
  | Module_declaration_name _ -> "module_declaration_name"
  | Module_type_declaration_name _ -> "module_type_declaration_name"
  | Open_description        _ -> "open_description"
  | Include_description     _ -> "include_description"
  | Include_declaration     _ -> "include_declaration"

let mkloc = Location.mkloc
let reloc txt loc = {loc with Location. txt}

let pattern_paths { Typedtree. pat_desc; pat_extra; pat_loc } =
  let init =
    match pat_desc with
    | Tpat_var (id,_) -> [mkloc (Path.Pident id) pat_loc]
    | Tpat_alias (_,id,loc) -> [reloc (Path.Pident id) loc]
    | _ -> []
  in
  List.fold_left ~init pat_extra
    ~f:(fun acc (extra,_,_) ->
      match extra with
      | Tpat_type (path,loc) -> reloc path loc :: acc
      | _ -> acc)

let expression_paths { Typedtree. exp_desc; exp_extra } =
  match exp_desc with
  | Texp_ident (path,loc,_) -> [reloc path loc]
  | Texp_new (path,loc,_) -> [reloc path loc]
  | Texp_instvar (_,path,loc)  -> [reloc path loc]
  | Texp_setinstvar (_,path,loc,_) -> [reloc path loc]
  | Texp_override (_,ps) ->
    List.map (fun (path,loc,_) -> reloc path loc) ps
  | Texp_letmodule (id,loc,_,_) -> [reloc (Path.Pident id) loc]
  | Texp_for (id,{Parsetree.ppat_loc = loc},_,_,_,_) ->
    [mkloc (Path.Pident id) loc]
  | _ -> []

let core_type_paths { Typedtree. ctyp_desc } =
  match ctyp_desc with
  | Ttyp_constr (path,loc,_) -> [reloc path loc]
  | Ttyp_class (path,loc,_) -> [reloc path loc]
  | _ -> []

let class_expr_paths { Typedtree. cl_desc } =
  match cl_desc with
  | Tcl_ident (path, loc, _) -> [reloc path loc]
  | _ -> []

let class_field_paths { Typedtree. cf_desc } =
  match cf_desc with
  | Tcf_val (loc,_,id,_,_) -> [reloc (Path.Pident id) loc]
  | _ -> []

let module_expr_paths { Typedtree. mod_desc } =
  match mod_desc with
  | Tmod_ident (path, loc) -> [reloc path loc]
  | Tmod_functor (id, loc, _, _) -> [reloc (Path.Pident id) loc]
  | _ -> []

let structure_item_paths { Typedtree. str_desc } =
  match str_desc with
  | Tstr_class_type cls ->
    List.map ~f:(fun (id,loc,_) -> reloc (Path.Pident id) loc) cls
  | Tstr_open { Typedtree. open_path; open_txt } ->
    [reloc open_path open_txt]
  | _ -> []

let module_type_paths { Typedtree. mty_desc } =
  match mty_desc with
  | Tmty_ident (path, loc) | Tmty_alias (path, loc) ->
    [reloc path loc]
  | Tmty_functor (id,loc,_,_) ->
    [reloc (Path.Pident id) loc]
  | Tmty_with (_,ls) ->
    List.map ~f:(fun (p,l,_) -> reloc p l) ls
  | _ -> []

let signature_item_paths { Typedtree. sig_desc } =
  match sig_desc with
  | Tsig_open { Typedtree. open_path; open_txt } ->
    [reloc open_path open_txt]
  | _ -> []

let with_constraint_paths = function
  | Twith_module (path,loc) | Twith_modsubst (path,loc) ->
    [reloc path loc]
  | _ -> []

let ci_paths {Typedtree. ci_id_name; ci_id_class } =
  [reloc (Path.Pident ci_id_class) ci_id_name]

let node_paths =
  let open Typedtree in function
  | Pattern p -> pattern_paths p
  | Expression e -> expression_paths e
  | Class_expr e -> class_expr_paths e
  | Class_field f -> class_field_paths f
  | Module_expr me -> module_expr_paths me
  | Structure_item (i, _) -> structure_item_paths i
  | Module_binding_name { mb_id; mb_name } ->
    [reloc (Path.Pident mb_id) mb_name]
  | Module_type mt -> module_type_paths mt
  | Signature_item (i,_) -> signature_item_paths i
  | Module_declaration_name { md_id; md_name } ->
    [reloc (Path.Pident md_id) md_name]
  | Module_type_declaration_name { mtd_id; mtd_name } ->
    [reloc (Path.Pident mtd_id) mtd_name]
  | With_constraint c -> with_constraint_paths c
  | Core_type ct -> core_type_paths ct
  | Package_type { pack_path; pack_txt } ->
    [reloc pack_path pack_txt]
  | Value_description { val_id; val_name } ->
    [reloc (Path.Pident val_id) val_name]
  | Type_declaration { typ_id; typ_name } ->
    [reloc (Path.Pident typ_id) typ_name]
  | Type_extension { tyext_path; tyext_txt } ->
    [reloc tyext_path tyext_txt]
  | Extension_constructor { ext_id; ext_name } ->
    [reloc (Path.Pident ext_id) ext_name]
  | Label_declaration { ld_id; ld_name } ->
    [reloc (Path.Pident ld_id) ld_name]
  | Constructor_declaration { cd_id; cd_name } ->
    [reloc (Path.Pident cd_id) cd_name]
  | Class_declaration ci -> ci_paths ci
  | Class_description ci -> ci_paths ci
  | Class_type_declaration ci -> ci_paths ci

  | _ -> []

let node_is_constructor = function
  | Constructor_declaration decl ->
    Some {decl.cd_name with Location.txt = `Declaration decl}
  | Expression {exp_desc = Texp_construct (loc, desc, _)} ->
    Some {loc with Location.txt = `Description desc}
  | Pattern {pat_desc = Tpat_construct (loc, desc, _)} ->
    Some {loc with Location.txt = `Description desc}
  | _ -> None

let node_attributes = function
  | Expression exp        -> exp.exp_attributes
  | Pattern pat           -> pat.pat_attributes
  | Class_expr cl         -> cl.cl_attributes
  | Class_field cf        -> cf.cf_attributes
  | Module_expr me        -> me.mod_attributes
  | Structure_item ({str_desc = Tstr_eval (_,attr)}, _) -> attr
  | Structure_item ({str_desc = Tstr_attribute a}, _) -> [a]
  | Signature_item ({sig_desc = Tsig_attribute a}, _) -> [a]
  | Module_binding mb     -> mb.mb_attributes
  | Value_binding vb      -> vb.vb_attributes
  | Module_type mt        -> mt.mty_attributes
  | Module_declaration md -> md.md_attributes
  | Module_type_declaration mtd -> mtd.mtd_attributes
  | Open_description o    -> o.open_attributes
  | Include_declaration i -> i.incl_attributes
  | Include_description i -> i.incl_attributes
  | Core_type ct          -> ct.ctyp_attributes
  (* FIXME: core type object attributes are ignored *)
  | Row_field (Ttag (_,attr,_,_)) -> attr
  | Value_description vd  -> vd.val_attributes
  | Type_declaration td   -> td.typ_attributes
  | Label_declaration ld  -> ld.ld_attributes
  | Constructor_declaration cd -> cd.cd_attributes
  | Type_extension te     -> te.tyext_attributes
  | Extension_constructor ec -> ec.ext_attributes
  | Class_type ct         -> ct.cltyp_attributes
  | Class_type_field ctf  -> ctf.ctf_attributes
  | Class_declaration ci -> ci.ci_attributes
  | Class_description ci -> ci.ci_attributes
  | Class_type_declaration ci -> ci.ci_attributes
  | Method_call (obj,_,_) -> obj.exp_attributes
  | _ -> []
