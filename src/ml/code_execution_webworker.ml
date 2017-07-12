Worker.import_scripts ["std.js"; "exported-unit.cmis.js"; "stdlib.cmis.js"; ];;

let (latest_typed_structure:Typedtree.structure option ref) = ref None;;
let (latest_typed_signature:Types.signature_item list option ref) = ref None;;
let (latest_env:Env.t option ref) = ref None;;

let open_implicit_module m env =
  let open Asttypes in
  let lid = {loc = Location.in_file "command line";
             txt = Longident.parse m } in
  snd (Typemod.type_open_ Override env lid.loc lid)

let type_code code = (
  let lexbuf = Lexing.from_string code in
  (try
    let structure = Parse.implementation lexbuf in
    JsooTop.initialize ();
    let env = !Toploop.toplevel_env in
    Env.reset_cache ();
    (* TODO implicitly open all the stdlib ones... *)
    let (typed_structure, typed_signature, env) = Typemod.type_structure env structure Location.none in
    latest_typed_structure := Some typed_structure;
    latest_typed_signature := Some typed_signature;
    latest_env := Some env;
    None
  with
    | Syntaxerr.Error err -> (
        match err with
        | Unclosed (loc, s, loc2, s2) -> Some ("SyntaxError", "Unclosed", [loc; loc2], s)
        | Expecting (loc, s) ->  Some ("SyntaxError", "Expecting", [loc], s)
        | Not_expecting (loc, s) -> Some ("SyntaxError", "Not_expecting", [loc], s)
        | Applicative_path loc -> Some ("SyntaxError", "Applicative_path", [loc], "")
        | Variable_in_scope (loc, s) -> Some ("SyntaxError", "Variable_in_scope", [loc], s)
        | Other loc -> Some ("SyntaxError", "Other", [loc], "")
        | Ill_formed_ast (loc, s) -> Some ("SyntaxError", "Ill_formed_ast", [loc], s)
        | Invalid_package_type (loc, s) -> Some ("SyntaxError", "Invalid_package_type", [loc], s)
      )
    | Typetexp.Error (loc, env, err) -> (
        let buffer = Buffer.create 100 in
        let formatter = Format.formatter_of_buffer buffer in
        let error = Typetexp.report_error env formatter err in
        let msg = Buffer.to_bytes buffer in
        Some ("TypetexpError", "", [loc], msg)
      )
    | Typemod.Error (loc, env, err) -> (
        let buffer = Buffer.create 100 in
        let formatter = Format.formatter_of_buffer buffer in
        let error = Typemod.report_error env formatter err in
        let msg = Buffer.to_bytes buffer in
        Some ("TypemodError", "", [loc], msg)
      )
    | Typecore.Error (loc, env, err) -> (
        let buffer = Buffer.create 100 in
        let formatter = Format.formatter_of_buffer buffer in
        let error = Typecore.report_error env formatter err in
        let msg = Buffer.to_bytes buffer in
        Some ("TypecoreError", "", [loc], msg)
      )
))

let autocomplete (pos:Lexing.position) str = (
  match !latest_env, !latest_typed_structure with
  | Some latest_env, Some latest_typed_structure -> (
    let (foo:Mbrowse.t list) = [[(latest_env, Structure latest_typed_structure)]] in
    let (env, node) = List.hd (List.hd foo) in
    let entries = Completion.node_complete env node str in
    print_endline "autocomplete entries:";
    let _ = List.iter (fun ({Query_protocol.Compl.name; _}) ->
      print_endline ("- " ^ name)) entries in
    ()
  )
  | _ ->
    ()
)

let comments = [("This is an example...", Location.{loc_start = Lexing.{pos_fname = ""; pos_lnum = 20; pos_bol = 0; pos_cnum = 0}; loc_end = Lexing.{pos_fname = ""; pos_lnum = 4; pos_bol = 0; pos_cnum = 20}; loc_ghost = false})]
;;

let comment x = (
  (* print_endline "okay 1";
  let foo = Track_definition.get_doc ~env ~local_defs:(`Implementation latest_typed_structure) ~pos ~comments ~config:Mconfig.initial (`User_input "List.iter") in
  print_endline "okay 2";
  (match foo with
  | `Found str -> print_endline ("Found this documentation:" ^ str)
  | _ -> print_endline "No documentation found"); *)

)

let execute_code code = (
  let markLocations = ref [] in
  let highlight_location loc = (
    let _file1,line1,col1 = Location.get_pos_info (loc.Location.loc_start) in
    let _file2,line2,col2 = Location.get_pos_info (loc.Location.loc_end) in
    markLocations := List.append !markLocations [(line1, col1, line2, col2)];
    ()
  ) in
  let stdout_buffer = Buffer.create 100 in
  let stderr_buffer = Buffer.create 100 in
  Sys_js.set_channel_flusher stdout (Buffer.add_string stdout_buffer);
  Sys_js.set_channel_flusher stderr (Buffer.add_string stderr_buffer);
  JsooTop.initialize ();
  let answer_buffer = Buffer.create 100 in
  JsooTop.execute true ~highlight_location (Format.formatter_of_buffer answer_buffer)  (code ^ ";;");
  let error = Buffer.to_bytes stderr_buffer in
  let output = Buffer.to_bytes stdout_buffer in
  let answer = Buffer.to_bytes answer_buffer in
  if String.length error > 0 then
    (error, markLocations)
  else if String.length output > 0 then
    (output ^ "\n\n" ^ answer, markLocations)
  else
    (answer, markLocations)
)
;;

let load_resource_aux url =
  try
    print_endline "riiight";
    let xml = XmlHttpRequest.create () in
    xml##_open(Js.string "GET", url, Js._false);
    xml##send(Js.null);
    if xml##status = 200 then Some (xml##responseText) else None
  with _ -> None
;;


Sys_js.register_autoload' "/" (fun (_,s) -> print_endline "dafuq"; load_resource_aux ((Js.string "cmtis/")##concat(s)))

let old_loader = !Env.Persistent_signature.load;;
Env.Persistent_signature.load := (fun ~unit_name ->
  (
    let cmi_infos2 = Cmt_format.read ("/cmis/" ^ (String.lowercase_ascii unit_name) ^ ".cmi") in
    match cmi_infos2 with
    | (Some cmi_infos, _) -> (
      (* List.iter (fun (c, _) -> print_endline c) cmt_infos.cmt_comments; *)
      print_endline ("Loading:" ^ unit_name);

      Some {
        Env.Persistent_signature.filename = unit_name;
        cmi = cmi_infos
      }

      )
    | _ -> print_endline "No match..."; None
    )
);;



Worker.set_onmessage (fun code ->
  let msgType = Js.to_string code##msgType in
  match msgType with
  | "type" -> (
    let err = type_code (Js.to_string code##code) in
    match err with
    | Some (m1, m2, locs, s) ->
      let result = Array.of_list (List.map (fun {Location.loc_start; loc_end} ->
        Js.Unsafe.obj [|
          ("loc_start",
        Js.Unsafe.obj [|
          ("pos_fname", Js.Unsafe.inject (Js.string loc_start.pos_fname));
          ("pos_lnum", Js.Unsafe.inject (Js.number_of_float (float_of_int loc_start.pos_lnum)));
          ("pos_bol", Js.Unsafe.inject (Js.number_of_float (float_of_int loc_start.pos_bol)));
          ("pos_cnum", Js.Unsafe.inject (Js.number_of_float (float_of_int loc_start.pos_cnum)));
        |]);
        ("loc_end",
        Js.Unsafe.obj [|
          ("pos_fname", Js.Unsafe.inject (Js.string loc_end.pos_fname));
          ("pos_lnum", Js.Unsafe.inject (Js.number_of_float (float_of_int loc_end.pos_lnum)));
          ("pos_bol", Js.Unsafe.inject (Js.number_of_float (float_of_int loc_end.pos_bol)));
          ("pos_cnum", Js.Unsafe.inject (Js.number_of_float (float_of_int loc_end.pos_cnum)));
        |])
        |]
        )
        locs)
      in
      Worker.post_message (Js.Unsafe.obj [|
        ("msgId", code##msgId);
        ("type", Js.Unsafe.inject (Js.string m1));
        ("subtype", Js.Unsafe.inject (Js.string m2));
        ("locations", Js.Unsafe.inject (Js.array result));
        ("message", Js.Unsafe.inject (Js.string (String.trim s)));
      |])
    | None -> ()
    )
  | "execute" -> (

    )
  | "autocomplete" -> (
      let pos_fname = Js.to_string code##pos_fname in
      let pos_lnum = int_of_float (Js.float_of_number code##pos_lnum) in
      let pos_bol = int_of_float (Js.float_of_number code##pos_bol) in
      let pos_cnum = int_of_float (Js.float_of_number code##pos_cnum) in
      let pos = Lexing.{pos_fname = pos_fname; pos_lnum = pos_lnum ; pos_bol = pos_bol; pos_cnum = pos_cnum} in
      autocomplete pos (Js.to_string code##text)
    )
  | _ -> ()
(*
  let target = "foo" in
  let test_pos = Lexing.{pos_fname = ""; pos_lnum = 3; pos_bol = 0; pos_cnum = 5} in
  let x = Destruct.node in
  let x = Ocamldoc.associate_comment in
  let x = Browse_tree.all_constructor_occurrences in
  (* outline test *)
  (match !latest_env, !latest_typed_structure with
  | Some env, Some ts -> (
    let (foo:Mbrowse.t list) = [[(env, Structure ts)]] in
    let (env, node) = List.hd (List.hd foo) in
    let x = Outline.get [Browse_tree.of_node node]   in
    print_endline "We have info to create an outline..."
  )
  | _ -> ()
  );
  let _ = type_code (Js.to_string code##code) in
  print_endline "GOOOD";
  let _ = autocomplete test_pos in
  () *)
  (*  *)
  (* match target with
  | "type" -> (type_code (Js.to_string code##code); ())
  | "autocomplete" -> () (* implemented, but not connected here yet *)
  | "compile" -> () (* implemented, but not connected here yet *)
  | "destruct" -> () (* should work, not tested yet...  *)
  | "occurences" -> failwith "not implemented yet" (* get all locations where the variable occurs *)
  | "outline" -> failwith "not implemented yet" (* get outline o*)

  | "locate" -> failwith "not implemented yet" (* jump to location *)
  | "document" -> failwith "not implemented yet" (* get ocaml doc for location *)

  | _ -> ()
; *)
  (* let (result, markLocations) = execute_code (Js.to_string code##code) in
  Worker.post_message (Js.Unsafe.obj [|
    ("id", code##id);
    ("result", Js.Unsafe.inject (Js.string (String.trim result)));
    ("locations", Js.Unsafe.inject !markLocations)
  |]) *)
);;
