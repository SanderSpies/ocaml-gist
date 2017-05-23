Worker.import_scripts ["stdlib2.cmis.js"];;

let (latest_typed_structure:Typedtree.structure option ref) = ref None;;
let (latest_typed_signature:Types.signature_item list option ref) = ref None;;
let (latest_env:Env.t option ref) = ref None;;

let type_code code = (
  let lexbuf = Lexing.from_string code in
  (try
    let structure = Parse.implementation lexbuf in
    let buff = Buffer.create 100 in
    let f = Format.formatter_of_buffer buff
    in
    let env1 = !Toploop.toplevel_env in
    Env.reset_cache ();
    let (typed_structure, typed_signature, env) = Typemod.type_structure env1 structure Location.none in
    let _ = Env.lookup_module ~load:true (Lident "List") in
    latest_typed_structure := Some typed_structure;
    latest_typed_signature := Some typed_signature;
    latest_env := Some env
  with
    | _ -> print_endline "something went wrong here...");
)
;;
let comments = [("This is an example...", Location.{loc_start = Lexing.{pos_fname = ""; pos_lnum = 2; pos_bol = 0; pos_cnum = 0}; loc_end = Lexing.{pos_fname = ""; pos_lnum = 4; pos_bol = 0; pos_cnum = 20}; loc_ghost = false})]
;;
let autocomplete (pos:Lexing.position) = (
  match !latest_env, !latest_typed_structure with
  | Some latest_env, Some latest_typed_structure -> (
    let (foo:Mbrowse.t list) = [[(latest_env, Structure latest_typed_structure)]] in
    let (env, node) = List.hd (List.hd foo) in
    let x = Mbrowse.deepest_before pos foo in
    let foo = Track_definition.get_doc ~env ~local_defs:(`Implementation latest_typed_structure) ~pos ~comments ~config:Mconfig.initial (`User_input "List.iter") in
    (match foo with
    | `Found str -> print_endline ("RAP:" ^ str)
    | _ -> print_endline "NO LOVE");
    let entries = Completion.node_complete env node "List." in
    (* print_endline "autocomplete entries:";
    let _ = List.iter (fun ({Query_protocol.Compl.name; _}) ->
      print_endline ("- " ^ name)) entries in *)
    ()
  )
  | _ ->
    ()
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
    let xml = XmlHttpRequest.create () in
    xml##_open(Js.string "GET", url, Js._false);
    xml##send(Js.null);
    if xml##status = 200 then Some (xml##responseText) else None
  with _ -> None
;;


Sys_js.register_autoload' "/" (fun (_,s) -> load_resource_aux ((Js.string "cmtis/")##concat(s)))

let old_loader = !Env.Persistent_signature.load;;
Env.Persistent_signature.load := (fun ~unit_name ->
  (
    let cmi_infos2 = Cmt_format.read ("/cmis/" ^ (String.lowercase_ascii unit_name) ^ ".cmi") in
    match cmi_infos2 with
    | (Some cmi_infos, _) -> (
      (* List.iter (fun (c, _) -> print_endline c) cmt_infos.cmt_comments; *)


      Some {
        Env.Persistent_signature.filename = unit_name;
        cmi = cmi_infos
      }

      )
    | _ -> None
    )
);;



Worker.set_onmessage (fun code ->
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
  let _ = autocomplete test_pos in
  ()
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
