
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
  let nextPart = ta##.nextElementSibling##.nextElementSibling in
  ignore(ta##.parentNode##insertBefore consoleTextArea nextPart);
  let console = Js.Unsafe.meth_call code_mirror "fromTextArea" [|
    (Js.Unsafe.inject consoleTextArea);
    (Js.Unsafe.obj [|
      ("mode", Js.Unsafe.js_expr "'ocaml'");
      ("readOnly", Js.Unsafe.js_expr "true")
    |])
    |]
  in
  ignore(console##getTextArea##.nextElementSibling##.classList##add (Js.string "console"));
  editor##on (Js.string "change") (Js.Unsafe.inject (Js.Unsafe.callback
      (debounce (fun _ -> (
        worker##postMessage (Js.Unsafe.obj [|
          ("id", Js.Unsafe.inject id);
          ("code", Js.Unsafe.inject editor##getValue);
          ("msgType", Js.Unsafe.inject (Js.string "type"));
        |])
      )) 500.)
  ));
  (id, console, editor)
)


let () = (
  info "[OCaml-gist] The inspector slows down the performance of executing OCaml code significantly.";
  let worker = Worker.create "ocaml_webworker.js" in
  let textareas = Dom.list_of_nodeList (Dom_html.document##querySelectorAll (Js.string "textarea[data-ocaml]")) in
  let i = ref (-1) in
  let editors = List.map (fun textarea ->
    let textarea = Js.Opt.to_option (Dom_html.CoerceTo.textarea textarea) in
    match textarea with
    | Some textarea -> ( i := !i + 1; Some (to_code_mirror !i textarea worker))
    | None -> None
  ) textareas
  in
  worker##.onmessage := Dom.handler (fun msg -> (
    Firebug.console##info (Js.Unsafe.inject msg##.data);
    Js.bool false
  ));
  Js.Unsafe.global##.shared := worker;
  Firebug.console##log (Js.Unsafe.inject Js.Unsafe.global##.shared)
)
