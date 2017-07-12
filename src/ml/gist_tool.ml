
let () = (
  let worker = Worker.create "code_execution_webworker.js" in
  worker##onmessage <- Dom.handler (fun msg -> (
    Firebug.console##info (Js.Unsafe.inject msg);
    Js.bool false
  ));
  Js.Unsafe.global##shared <- worker;
  Firebug.console##log (Js.Unsafe.inject Js.Unsafe.global##shared)
)

(*
let info s = (
  Firebug.console##info (Js.string s)
)

let err s = (
  Firebug.console##error (Js.string s)
)

let log s = (
  Firebug.console##log (Js.string s)
)

let highlight_location editor line1 col1 line2 col2 = (
  (* let _file1,line1,col1 = Location.get_pos_info (loc.Location.loc_start) in
  let _file2,line2,col2 = Location.get_pos_info (loc.Location.loc_end) in *)
  let from = Js.Unsafe.(obj
    [|"line", Js.Unsafe.inject (line1 - 1);
      "ch", Js.Unsafe.inject col1 |]) in
  let to_ = Js.Unsafe.(obj
    [|"line", Js.Unsafe.inject (line2 - 1);
      "ch", Js.Unsafe.inject col2 |]) in
  let options = Js.Unsafe.(obj
    [|"className", Js.Unsafe.inject "ocaml-gist-highlight"|]) in
  editor##doc##markText(from, to_, options);
)

let remove_marks editor = (
  editor##doc##getAllMarks()##forEach(fun mark -> mark##clear())
)

let debounce func timeout_ms = (
  let noop () = () in
  let timeout = ref (Dom_html.setTimeout noop timeout_ms) in
  fun _ -> (
    Dom_html.clearTimeout !timeout;
    timeout := Dom_html.setTimeout func timeout_ms;
    Js.bool false;
  )
)

let to_code_mirror id (textarea:Dom_html.textAreaElement Js.t) worker = (
  let code_mirror = Js.Unsafe.eval_string "CodeMirror" in
  let editor = Js.Unsafe.meth_call code_mirror "fromTextArea" [|
    (Js.Unsafe.inject textarea);
    (Js.Unsafe.obj [|
      ("mode", Js.Unsafe.js_expr "'ocaml'");
      ("lineNumbers", Js.Unsafe.js_expr "true");
      ("matchBrackets", Js.Unsafe.js_expr "true");
    |])
  |] in
  let doc = Dom_html.document in
  let consoleTextArea = Dom_html.createTextarea doc in
  let ta = editor##getTextArea () in
  let nextPart = ta##nextElementSibling##nextElementSibling in
  ignore(ta##parentNode##insertBefore(consoleTextArea, nextPart));
  let console = Js.Unsafe.meth_call code_mirror "fromTextArea" [|
    (Js.Unsafe.inject consoleTextArea);
    (Js.Unsafe.obj [|
      ("mode", Js.Unsafe.js_expr "'ocaml'");
      ("readOnly", Js.Unsafe.js_expr "true")
    |])
    |]
  in
  ignore(console##getTextArea()##nextElementSibling##classList##add (Js.string "console"));
  editor##on (Js.string "change", (Js.Unsafe.inject (Js.Unsafe.callback
      (debounce (fun _ -> (
        worker##postMessage (Js.Unsafe.obj [|
          ("id", Js.Unsafe.inject id);
          ("code", Js.Unsafe.inject editor##getValue())
        |])
      )) 500.)
  )));
  (id, console, editor)
)

let initialize () = (
  (* to reduce input lag we run a webworker *)
  let worker = Worker.create "code_execution_webworker.js" in
  info "[OCaml-gist] The inspector slows down the performance of executing OCaml code significantly.";
  (* TODO: add CodeMirror JS and CSS from here *)
  let textareas = Dom.list_of_nodeList (Dom_html.document##querySelectorAll (Js.string "textarea[data-ocaml]")) in
  let i = ref (-1) in
  let editors = List.map (fun textarea ->
    let textarea = Js.Opt.to_option (Dom_html.CoerceTo.textarea textarea) in
    match textarea with
    | Some textarea -> ( i := !i + 1; Some (to_code_mirror !i textarea worker))
    | None -> None
  ) textareas
  (* in  *)
  worker##onmessage <- Dom.handler (fun msg -> (
    let id = Js.parseInt msg##data##id in
    let x = List.find (fun res ->
      match res with
      | Some (editor_id, _, _) -> id == editor_id
      | None -> false
    ) editors in
    (match x with
    | Some (_, console, editor) -> (
      remove_marks editor;
      console##setValue (Js.string msg##data##result);
      List.iter (fun (line1, col1, line2, col2) ->
        let from = Js.Unsafe.(obj
          [|"line", Js.Unsafe.inject (line1 - 1);
            "ch", Js.Unsafe.inject col1 |]) in
          let to_ = Js.Unsafe.(obj
          [|"line", Js.Unsafe.inject (line2 - 1);
            "ch", Js.Unsafe.inject col2 |]) in
          let options = Js.Unsafe.(obj
            [|"className", Js.Unsafe.inject "ocaml-gist-highlight"|]) in
          editor##doc##markText(from, to_, options);
        ()
      ) msg##data##locations;
    )
    | None -> ());
    Js.bool false
  ))
)
;;

initialize ();; *)
