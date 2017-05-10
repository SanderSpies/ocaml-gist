Worker.import_scripts ["stdlib.cmis.js"];;

type phase =
  | Autocomplete
  | Typing
  | Compilation



let execute_code code = (
  let lexbuf = Lexing.from_string code in
  (try
    let structure = Parse.implementation lexbuf in
    let buff = Buffer.create 100 in
    let f = Format.formatter_of_buffer buff
    in
    (* Compmisc.init_path false; *)
    let env1 = !Toploop.toplevel_env in
    let (typed_structure, _, env) = Typemod.type_structure env1 structure Location.none in
    let pos = Lexing.{pos_fname = ""; pos_lnum = 3; pos_bol = 0; pos_cnum = 5} in
    let (foo:Mbrowse.t list) = [[(env, Structure typed_structure)]] in
    let (env, node) = List.hd (List.hd foo) in
    let x = Mbrowse.deepest_before pos foo in
    let entries = Completion.node_complete env node "bar" in
    print_string "entries:";
    List.iter (fun ({Query_protocol.Compl.name; _}) ->
      print_string name) entries;
    Printtyped.implementation f typed_structure;
    let result = Buffer.to_bytes buff in
    Firebug.console##log (Js.string result);
    ()
    with
    | _ -> ());


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

Worker.set_onmessage (fun code ->
  let (result, markLocations) = execute_code (Js.to_string code##code) in
  Worker.post_message (Js.Unsafe.obj [|
    ("id", code##id);
    ("result", Js.Unsafe.inject (Js.string (String.trim result)));
    ("locations", Js.Unsafe.inject !markLocations)
  |])
);;
