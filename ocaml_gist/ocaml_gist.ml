
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

module Code_execution = struct



  let promise_global = Js.Unsafe.global##._Promise

  type response = <msgId: int Js.prop> Js.t

  let worker:((Js.Unsafe.any, response) Worker.worker Js.t) = Worker.create "ocaml_webworker.js"

  type 'a resolve = 'a -> unit

  type 'a responses = (int, 'a resolve) Hashtbl.t

  let awaiting_responses:(response responses) = Hashtbl.create 50

  let unique_id = ref 0;;

  let () = (
    worker##.onmessage := Dom.handler (fun (msg : response Worker.messageEvent Js.t)  -> (
      let data = msg##.data in
      let msgId = data##.msgId in
      let resolve_fn = Hashtbl.find awaiting_responses msgId in
      Hashtbl.remove awaiting_responses msgId;
      ignore(Js.Unsafe.fun_call resolve_fn [| Js.Unsafe.inject data |]);
      Js.bool true;
    ))
  )

  let post_message msg = (
    unique_id := !unique_id + 1;
    let msg = Array.append [|("msgId", Js.Unsafe.inject !unique_id )|] msg in
    worker##postMessage (Js.Unsafe.obj msg);
    Js.Unsafe.new_obj promise_global [|Js.Unsafe.inject (fun resolve _ ->
      Hashtbl.add awaiting_responses !unique_id resolve
    )|]
  )

end

type response = <string: Js.js_string Js.t Js.prop; start: int Js.prop> Js.t

type response_list = response list

let rec get_token editor pos (result:response_list) = (
  (* TODO: ensure autocomplete only takes proper variables *)
  let token:response = editor##getTokenAt pos in
  let str = String.trim (Js.to_string token##.string) in
  match str with
  | "" ->
    if List.length result = 0 then
      (-1, 5, "")
    else
      let str = String.concat "" (List.map (fun (token:response) -> Js.to_string token##.string) result) in
      let foo = List.nth (List.rev result) 0 in
      let x = Js.to_string foo##.string in
      if x.[0] = '.' then
        (foo##.start + 1, Js.Unsafe.get foo "end", str)
      else
        (foo##.start, Js.Unsafe.get foo "end", str)
  | _ ->
    get_token editor (Js.Unsafe.fun_call (Js.Unsafe.js_expr "CodeMirror.Pos") [| Js.Unsafe.inject pos##.line; Js.Unsafe.inject token##.start |] ) ([token] @ result)
)


let remove_marks editor = (
  editor##.doc##getAllMarks##forEach(fun mark -> mark##clear)
)

let showHint editor = (
  let cur = editor##getCursor in
  let (start, _end, hint) = get_token editor cur [] in
  let msgId = int_of_string (Js.to_string (editor##getTextArea##getAttribute (Js.string "position"))) in
  if hint <> "" then
    let code_mirror = Js.Unsafe.eval_string "CodeMirror" in
    let promise = Code_execution.post_message [|
      ("text", Js.Unsafe.inject (Js.string hint));
      ("posFname", Js.Unsafe.inject (Js.string ""));
      ("posLnum", Js.Unsafe.inject cur##.line);
      ("posBol", Js.Unsafe.inject 0);
      ("posCnum", Js.Unsafe.inject start);
      ("msgType", Js.Unsafe.inject (Js.string "complete_prefix"));
    |]
    in
    promise##_then (fun data -> (
      remove_marks editor;
      Firebug.console##log data##.suggestions;
      let suggestions = data##.suggestions##map (fun suggestion ->
        suggestion##.name
      ) in
      Js.Unsafe.obj [|
        ("list", suggestions);
        ("from", (Js.Unsafe.fun_call (Js.Unsafe.js_expr "CodeMirror.Pos") [| Js.Unsafe.inject cur##.line; Js.Unsafe.inject start|] ));
        ("to", (Js.Unsafe.fun_call (Js.Unsafe.js_expr "CodeMirror.Pos") [| Js.Unsafe.inject cur##.line; Js.Unsafe.inject _end|] ));
      |]
    ));
  else (
    Js.Unsafe.obj [|
      ("list", Js.Unsafe.inject (Js.array [| |]));
      ("from", (Js.Unsafe.fun_call (Js.Unsafe.js_expr "CodeMirror.Pos") [| Js.Unsafe.inject cur##.line; Js.Unsafe.inject start|] ));
      ("to", (Js.Unsafe.fun_call (Js.Unsafe.js_expr "CodeMirror.Pos") [| Js.Unsafe.inject cur##.line; Js.Unsafe.inject _end|] ));
    |]
  )
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

let unboundRegexp = Regexp.regexp "^Unbound"

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


let to_code_mirror id (textarea:Dom_html.textAreaElement Js.t) = (
  let code_mirror = Js.Unsafe.eval_string "CodeMirror" in
  let editor = Js.Unsafe.meth_call code_mirror "fromTextArea" [|
    (Js.Unsafe.inject textarea);
    (Js.Unsafe.obj [|
      ("mode", Js.Unsafe.js_expr "'ocaml'");
      ("lineNumbers", Js.Unsafe.js_expr "false");
      ("matchBrackets", Js.Unsafe.js_expr "true");
      (* ("extraKeys", (Js.Unsafe.obj [|
        ("Ctrl-Space", Js.Unsafe.js_expr "'autocomplete'");
      |])); *)
      ("styleActiveLine", Js.Unsafe.inject (Js.bool true))
    |])
  |]
  in
  let doc = Dom_html.document in
  let consoleTextArea = Dom_html.createTextarea doc in
  let ta = editor##getTextArea () in
  let nextPart = ta##.nextElementSibling##.nextElementSibling in
  let toolbar = Dom_html.createDiv doc in
  let error_icon = Dom_html.createDiv doc in
  error_icon##.classList##add (Js.string "og-error-icon");
  let execute_icon = Dom_html.createDiv doc in

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
  execute_icon##.onclick := Dom_html.handler (fun _ ->
    let promise = Code_execution.post_message [|
      ("code", Js.Unsafe.inject editor##getValue);
      ("msgType", Js.Unsafe.inject (Js.string "execute"));
    |]
    in
    promise##_then (fun data -> console##setValue data##.result);
    Js._true
  );
  ignore(editor##getTextArea##.nextElementSibling##.classList##add (Js.string "og-editor"));
  ignore(console##getTextArea##.nextElementSibling##.classList##add (Js.string "og-console"));
  editor##on (Js.string "focus") (Js.Unsafe.inject (Js.Unsafe.callback (fun _ ->
    ignore(Code_execution.post_message [|
      ("code", Js.Unsafe.inject editor##getValue);
      ("msgType", Js.Unsafe.inject (Js.string "type"));
    |]);
    Js._true
  )));

  editor##on (Js.string "change") (Js.Unsafe.inject (Js.Unsafe.callback
      (debounce (fun _ -> (
        let completion = Js.Opt.to_option editor##.state##.completionActive in
        match completion with
        | Some _ -> (
            show_execute_icon editor;
            console##setValue (Js.string "")
          )
        | _ -> (
          let promise = Code_execution.post_message [|
            ("code", Js.Unsafe.inject editor##getValue);
            ("msgType", Js.Unsafe.inject (Js.string "type"));
          |]
          in
          promise##_then (fun data -> (
            remove_marks editor;
            let msgType = Js.to_string data##.msgType in
            match msgType with
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
              let msg = Js.string data##.message in
              let str_match = Regexp.string_match unboundRegexp (Js.to_string msg) 0 in
              match str_match with
              | Some _ ->
                let code_mirror = Js.Unsafe.eval_string "CodeMirror" in
                ignore(Js.Unsafe.meth_call code_mirror "showHint" [| editor |])
              | _ -> (
                let locations = data##.locations in
                let msg = Js.string data##.message in
                show_error_icon editor;
                console##setValue msg;
                Array.iter (highlight_location editor) locations;
              )
              )
            | "NoSyntaxErrors" -> (
              show_execute_icon editor;
              console##setValue (Js.string "");
            )
            | _ as msgType -> failwith ("Not properly handled: " ^ msgType)


          ));
        )

      )) 500.)
  ));
  (id, console, editor)
)

let () = (
  info "[OCaml-gist] The inspector slows down the performance of executing OCaml code significantly.";
  let textareas = Dom.list_of_nodeList (Dom_html.document##querySelectorAll (Js.string "textarea[data-ocaml]")) in
  let i = ref (-1) in
  let editors = List.map (fun textarea ->
    let textarea = Js.Opt.to_option (Dom_html.CoerceTo.textarea textarea) in
    match textarea with
    | Some textarea -> ( i := !i + 1; textarea##setAttribute (Js.string "position") (Js.string (string_of_int !i)); Some (to_code_mirror !i textarea))
    | None -> None
  ) textareas
  in
  let code_mirror = Js.Unsafe.eval_string "CodeMirror" in
  ignore(Js.Unsafe.meth_call code_mirror "registerHelper" [|
    Js.Unsafe.inject (Js.string "hint");
    Js.Unsafe.inject (Js.string "ocaml");
    Js.Unsafe.inject showHint
  |]);
)
