open Types

let info s = (
  Firebug.console##info (Js.string s)
)

let err s = (
  Firebug.console##error (Js.string s)
)

let log s = (
  Firebug.console##log (Js.string s)
)

let highlight_location editor loc = (
  let _file1,line1,col1 = Location.get_pos_info (loc.Location.loc_start) in
  let _file2,line2,col2 = Location.get_pos_info (loc.Location.loc_end) in
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

let executeCode editor code = (
  remove_marks editor;
  let stdout_buffer = Buffer.create 100 in
  let stderr_buffer = Buffer.create 100 in
  Sys_js.set_channel_flusher stdout (Buffer.add_string stdout_buffer);
  Sys_js.set_channel_flusher stderr (Buffer.add_string stderr_buffer);
  JsooTop.initialize ();
  let answer_buffer = Buffer.create 100 in
  JsooTop.execute true ~highlight_location:(highlight_location editor) (Format.formatter_of_buffer answer_buffer)  (code ^ ";;");
  let error = Buffer.to_bytes stderr_buffer in
  let output = Buffer.to_bytes stdout_buffer in
  let answer = Buffer.to_bytes answer_buffer in
  if String.length error > 0 then
    error
  else if String.length output > 0 then
    output ^ "\n\n" ^ answer
  else
    answer
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

let to_code_mirror (textarea:Dom_html.textAreaElement Js.t) = (
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
        (* TODO: run compilation in a webworker for better user experience *)
          let result = executeCode editor (Js.to_string editor##getValue()) in
          console##setValue (Js.string (String.trim result))
      )) 500.)
  )))
)

let initialize () = (
  info "[OCaml-gist] The inspector slows down the performance of executing OCaml code significantly.";
  (* TODO: add CodeMirror JS and CSS from here *)
  let textareas = Dom_html.document##querySelectorAll (Js.string "textarea[data-ocaml]") in
  let length = textareas##length - 1 in
  for i = 0 to length do
    match Js.Opt.to_option (textareas##item(i)) with
    | Some node -> (
        let textarea = Js.Opt.to_option (Dom_html.CoerceTo.textarea node) in
        match textarea with
        | Some textarea -> let _ = to_code_mirror textarea in ()
        | None -> ()
      )
    | None -> ()
  done
)
;;

initialize ();;
