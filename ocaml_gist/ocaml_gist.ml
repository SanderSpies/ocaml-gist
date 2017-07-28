
let info s = (
  Firebug.console##info (Js.string s)
)

let err s = (
  Firebug.console##error (Js.string s)
)

let log s = (
  Firebug.console##log (Js.string s)
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


let show_error_icon editor = (
  let ta = editor##getTextArea () in
  let toolbar = ta##.nextElementSibling##.nextElementSibling
  |> Dom_html.CoerceTo.div
  |> Js.Opt.to_option in
  match toolbar with
  | Some div -> (
      ignore(div##.classList##add (Js.string "og-show-error"));
      ignore(div##.classList##remove (Js.string "og-show-execute"));
    )
  | None -> ()
)

let showHint worker editor options = (
  let cur = editor##getCursor in
  let token = editor##getTokenAt cur in
  let msgId = int_of_string (Js.to_string (editor##getTextArea##getAttribute (Js.string "position"))) in
  let hint = Js.to_string token##.string in
  print_endline hint;
  worker##postMessage (Js.Unsafe.obj [|
    ("msgId", Js.Unsafe.inject msgId);
    ("text", Js.Unsafe.inject token##.string);
    ("posFname", Js.Unsafe.inject (Js.string ""));
    ("posLnum", Js.Unsafe.inject 1);
    ("posBol", Js.Unsafe.inject 1);
    ("posCnum", Js.Unsafe.inject 1);
    ("msgType", Js.Unsafe.inject (Js.string "complete_prefix"));
  |]);

    (* var token = editor.getTokenAt(cur);
    var Pos = CodeMirror.Pos;
    return {list: ["do", "the", "camel", "dance"],
            from: Pos(cur.line, token.start),
            to: Pos(cur.line, token.end)}; *)
  (* }; *)

)

let show_execute_icon editor = (
  let ta = editor##getTextArea () in
  let toolbar = ta##.nextElementSibling##.nextElementSibling
  |> Dom_html.CoerceTo.div
  |> Js.Opt.to_option in
  match toolbar with
  | Some div -> (
      ignore(div##.classList##remove (Js.string "og-show-error"));
      ignore(div##.classList##add (Js.string "og-show-execute"));
    )
  | None -> ()
)


let to_code_mirror id (textarea:Dom_html.textAreaElement Js.t) worker = (
  let code_mirror = Js.Unsafe.eval_string "CodeMirror" in
  let editor = Js.Unsafe.meth_call code_mirror "fromTextArea" [|
    (Js.Unsafe.inject textarea);
    (Js.Unsafe.obj [|
      ("mode", Js.Unsafe.js_expr "'ocaml'");
      ("lineNumbers", Js.Unsafe.js_expr "false");
      ("matchBrackets", Js.Unsafe.js_expr "true");
      ("extraKeys", (Js.Unsafe.obj [|
        ("Ctrl-Space", Js.Unsafe.js_expr "'autocomplete'");
      |]));
    |])
  |] in
  let doc = Dom_html.document in
  let consoleTextArea = Dom_html.createTextarea doc in
  let ta = editor##getTextArea () in
  let nextPart = ta##.nextElementSibling##.nextElementSibling in
  let toolbar = Dom_html.createDiv doc in
  let error_icon = Dom_html.createDiv doc in
  error_icon##.classList##add (Js.string "og-error-icon");
  let execute_icon = Dom_html.createDiv doc in
  execute_icon##.onclick := Dom_html.handler (fun _ ->
    worker##postMessage (Js.Unsafe.obj [|
      ("msgId", Js.Unsafe.inject id);
      ("code", Js.Unsafe.inject editor##getValue);
      ("msgType", Js.Unsafe.inject (Js.string "execute"));
    |]);
    Js._true
  );
  execute_icon##.classList##add (Js.string "og-execute-icon");
  Dom.appendChild toolbar error_icon;
  Dom.appendChild toolbar execute_icon;
  toolbar##.classList##add (Js.string "og-toolbar");
  toolbar##.classList##add (Js.string "og-show-execute");

  ignore(ta##.parentNode##insertBefore toolbar nextPart);
  ignore(ta##.parentNode##insertBefore consoleTextArea nextPart);
  let console = Js.Unsafe.meth_call code_mirror "fromTextArea" [|
    (Js.Unsafe.inject consoleTextArea);
    (Js.Unsafe.obj [|
      ("mode", Js.Unsafe.js_expr "''");
      ("readOnly", Js.Unsafe.js_expr "true");
      ("lineWrapping", Js.Unsafe.js_expr "true");
    |])
    |]
  in
  ignore(editor##getTextArea##.nextElementSibling##.classList##add (Js.string "og-editor"));
  ignore(console##getTextArea##.nextElementSibling##.classList##add (Js.string "og-console"));
  editor##on (Js.string "change") (Js.Unsafe.inject (Js.Unsafe.callback
      (debounce (fun _ -> (
        worker##postMessage (Js.Unsafe.obj [|
          ("msgId", Js.Unsafe.inject id);
          ("code", Js.Unsafe.inject editor##getValue);
          ("msgType", Js.Unsafe.inject (Js.string "type"));
        |]);
      )) 500.)
  ));
  (id, console, editor)
)


let highlight_location editor loc = (
  let _file1 = loc##.locStart##.posFname in
  let line1 = loc##.locStart##.posLnum in
  let col1 = loc##.locStart##.posCnum - loc##.locStart##.posBol in
  let _file2 = loc##.locEnd##.posFname in
  let line2 = loc##.locEnd##.posLnum in
  let col2 = loc##.locEnd##.posCnum - loc##.locEnd##.posBol in
  let from = Js.Unsafe.(obj
    [|"line", Js.Unsafe.inject (line1 - 1);
      "ch", Js.Unsafe.inject col1 |]) in
  let to_ = Js.Unsafe.(obj
    [|"line", Js.Unsafe.inject (line2 - 1);
      "ch", Js.Unsafe.inject col2 |]) in
  let options = Js.Unsafe.(obj
    [|"className", Js.Unsafe.inject "og-highlight"|]) in
  editor##.doc##markText from to_ options;
)



let remove_marks editor = (
  editor##.doc##getAllMarks##forEach(fun mark -> mark##clear)
)


let () = (
  info "[OCaml-gist] The inspector slows down the performance of executing OCaml code significantly.";
  let worker = Worker.create "ocaml_webworker.js" in
  let textareas = Dom.list_of_nodeList (Dom_html.document##querySelectorAll (Js.string "textarea[data-ocaml]")) in
  let i = ref (-1) in
  let editors = List.map (fun textarea ->
    let textarea = Js.Opt.to_option (Dom_html.CoerceTo.textarea textarea) in
    match textarea with
    | Some textarea -> ( i := !i + 1; textarea##setAttribute (Js.string "position") (Js.string (string_of_int !i)); Some (to_code_mirror !i textarea worker))
    | None -> None
  ) textareas
  in
  worker##.onmessage := Dom.handler (fun msg -> (
    let data = msg##.data in
    let msgType:string = Js.to_string data##.msgType in
    let msgId:int = data##.msgId in
    let maybeEditor = List.nth editors msgId in
    match maybeEditor with
    | Some (id, console, editor) -> (
      remove_marks editor;
      let _ = match msgType with
      | "Output" -> (
          let msg = Js.string data##.message in
          console##setValue msg;
          show_error_icon editor;
        )
      | "TypetexpError"
      | "TypemodError"
      | "TypecoreError"
      | "LexerError"
      | "SyntaxError" -> (
        let locations = data##.locations in
        let msg = Js.string data##.message in
        show_error_icon editor;
        console##setValue msg;
        Array.iter (highlight_location editor) locations;
        )

      | "NoSyntaxErrors" -> (
        show_execute_icon editor;
        console##setValue (Js.string "");
      )
      | "execute" -> (
        console##setValue data##.result;
        )
      | "complete_prefix" -> (
        print_endline "complete the prefix here..."
        )
      | _ as msgType -> failwith ("This should not happen: " ^ msgType)
      in
      Firebug.console##info (Js.Unsafe.inject msg##.data);
      Js.bool false
    )
    | None -> (
      (* should not happen at all... *)
      Js.bool false
      )
  ));
  let code_mirror = Js.Unsafe.eval_string "CodeMirror" in
  ignore(Js.Unsafe.meth_call code_mirror "registerHelper" [|
    Js.Unsafe.inject (Js.string "hint");
    Js.Unsafe.inject (Js.string "ocaml");
    Js.Unsafe.inject (showHint worker)
  |]);
  Js.Unsafe.global##.shared := worker;
)
