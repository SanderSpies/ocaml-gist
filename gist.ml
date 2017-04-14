open Types

let a = false

let info s = (
  Firebug.console##info (Js.string s)
)

let log ~s = (
  Firebug.console##log (Js.string s)
)
print_string "foo"
let highlight_location loc = (
  (*log "highlight it"*)
)

module x = struct

let executeCode code = (
  JsooTop.initialize ();
  let buffer = Buffer.create 100 in
  JsooTop.execute true (*~pp_code:Format.std_formatter*) ~highlight_location (Format.formatter_of_buffer buffer)  (code ^ ";;");
  Buffer.to_bytes buffer
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
  editor##on (Js.string "change", (Js.Unsafe.inject (Js.Unsafe.callback
      (debounce (fun _ -> (
        (* TODO: run compilation in a webworker for better user experience *)
          let executedCode = executeCode (Js.to_string editor##getValue()) in
          info executedCode
        *)
      )) 500.)
  )));
  (* "the console output part" *)
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
