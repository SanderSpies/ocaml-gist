[@@@ocaml.error "-20-3-10-27"];

open Bs_webapi.Dom;

external querySelectorAll : Dom.document => string => array Dom.element =
  "" [@@bs.send];

external textContent : Dom.element => string = "" [@@bs.get];

external trim : string => string = "" [@@bs.send];

module CodeMirror = {
  type t;
  type fn;
  external registerHelper : string => string => fn => unit =
    "" [@@bs.val "CodeMirror.registerHelper"];
  /* external showHint : t => unit = "" [@@bs.val "CodeMirror.showHint"]; */
  external codeMirror : ReasonReact.reactClass =
    "react-codemirror" [@@bs.module];
  let make
      autoFocus::(autoFocus: option bool)=?
      autoSave::(autoSave: option bool)=?
      className::(className: option string)=?
      defaultValue::(defaultValue: option string)=?
      value::(value: option string)=?
      options::(options: option (Js.t {..}))=?
      onChange::(onChange: option (string => unit))=?
      onFocusChange::(onFocusChange: option (bool => unit))=?
      _children =>
    ReasonReact.wrapJsForReason
      reactClass::codeMirror
      props::{
        "autoFocus": Js.Null_undefined.from_opt autoFocus,
        "autoSave": Js.Null_undefined.from_opt autoSave,
        "className": Js.Null_undefined.from_opt className,
        "defaultValue": Js.Null_undefined.from_opt defaultValue,
        "value": Js.Null_undefined.from_opt value,
        "options": Js.Null_undefined.from_opt options,
        "onChange": Js.Null_undefined.from_opt onChange,
        "onFocusChange": Js.Null_undefined.from_opt onFocusChange
      }
      _children;
};

[%%bs.raw
  {|
  require('./../../../src/ocaml.js');
  require('./../../../src/active-line.js');
  require('./../../../src/show-hint.js');
|}
];

module JsPromise = {
  type t 'a;
  type error;
  type resolve 'a = 'a => unit;
  external make :
    (resolve::resolve 'a => reject::(exn => unit) [@bs] => unit) => t 'a =
    "Promise" [@@bs.new];
  external then_ : ('a => t 'b) [@bs.uncurry] => t 'b =
    "then" [@@bs.send.pipe : t 'a];
  external resolve : 'a => t 'a = "resolve" [@@bs.val] [@@bs.scope "Promise"];
  external reject : exn => t 'a = "reject" [@@bs.val] [@@bs.scope "Promise"];
  external catch : (error => t 'a) [@bs.uncurry] => t 'a =
    "catch" [@@bs.send.pipe : t 'a];
};

module JsWorker = {
  type t;
  external make : string => t = "Worker" [@@bs.new];
  external onmessage : t => ('a => unit) => unit = "" [@@bs.set];
  external postMessage : t => 'b => unit = "" [@@bs.send];
};

module MsgMap = {
  type t;
  external make : unit => t = "Map" [@@bs.new];
  external set : t => int => 'a => unit = "" [@@bs.send];
  external delete : t => unit = "" [@@bs.send];
  external get : t => int => 'a = "" [@@bs.send];
};

module CodeExecution = {
  type postMessage =
    | Type string
    | Execute string
    | TypeExpression int int string
    | CompletePrefix int int string;
  let worker = JsWorker.make "ocaml_webworker.js";
  let awaitingResponses = MsgMap.make ();
  let uniqueId = ref 0;
  let () =
    JsWorker.onmessage
      worker
      (
        fun msg => {
          let data = msg##data;
          let uniqueId: int = data##msgId;
          let resolve_fn = MsgMap.get awaitingResponses uniqueId;
          resolve_fn data
        }
      );
  let postMessage (message: postMessage) => {
    uniqueId := !uniqueId + 1;
    let uniqueId = !uniqueId;
    switch message {
    | Type code =>
      JsWorker.postMessage
        worker {"msgId": uniqueId, "msgType": "type", "code": code}
    | Execute code =>
      JsWorker.postMessage
        worker {"msgId": uniqueId, "msgType": "execute", "code": code}
    | TypeExpression line ch expr =>
      JsWorker.postMessage
        worker
        {
          "msgId": uniqueId,
          "msgType": "type_expr",
          "expr": expr,
          "posLnum": line,
          "posCnum": ch,
          "posBol": 0,
          "posFname": ""
        }
    | CompletePrefix line ch expr =>
      JsWorker.postMessage
        worker
        {
          "msgId": uniqueId,
          "msgType": "complete_prefix",
          "text": expr,
          "posLnum": line,
          "posCnum": ch,
          "posBol": 0,
          "posFname": ""
        }
    };
    JsPromise.make (
      fun ::resolve ::reject => MsgMap.set awaitingResponses uniqueId resolve
    )
  };
};

let unboundRegexp = [%bs.re "/^Unbound/"];

module Gist = {
  let debounceReactEvent func timeout_ms => {
    let noop () => ();
    let timeout = ref (Js.Global.setTimeout noop timeout_ms);
    fun event => {
      ReactEventRe.Synthetic.persist event;
      Js.Global.clearTimeout !timeout;
      timeout := Js.Global.setTimeout (fun _ => func event) timeout_ms
    }
  };
  let debounce func timeout_ms => {
    let noop () => ();
    let timeout = ref (Js.Global.setTimeout noop timeout_ms);
    fun event => {
      Js.Global.clearTimeout !timeout;
      timeout := Js.Global.setTimeout (fun _ => func event) timeout_ms
    }
  };
  type codeState =
    | Executable
    | Error
    | Busy
    | Result;
  type location = Js.t {. line : int, ch : int};
  type range = {
    locStart: location,
    locEnd: location
  };
  type errorLocations = array range;
  type tooltip = (string, int, int);
  type state = {
    console: string,
    codeState,
    errorLocations,
    codeMirrorRef: option ReasonReact.reactRef,
    tooltip: option tooltip
  };
  let component = ReasonReact.statefulComponent "Gist";
  let highlightLocations locations editor => {
    let options = {"className": "og-highlight"};
    Array.iter
      (
        fun location =>
          editor##doc##markText location.locStart location.locEnd options
      )
      locations
  };
  let removeMarks editor =>
    (editor##doc##getAllMarks ())##forEach (fun mark => mark##clear ());
  let codeMirrorAction fn {ReasonReact.state: state} => {
    let ref = state.codeMirrorRef;
    switch ref {
    | Some ref =>
      let ref = ReasonReact.refToJsObj ref;
      fn (ref##getCodeMirror ())
    | None => ()
    };
    ReasonReact.NoUpdate
  };
  let token_regexp = [%bs.re "/^[0-9a-zA-Z_.\"\[\]]+/"];
  type pos = Js.t {. line : int, ch : int, outside : bool, xRel : float};
  let getToken editor (pos: pos) => {
    let lineTokens = editor##getLineTokens pos##line;
    /* TODO: change characters that are not [_.a-zA-Z0-9] into separate space entries */
    let start = ref 0;
    let end_ = ref 0;
    let rec it items result isCurrentToken => {
      switch items {
        | [token, ...tl] => {
          let isCurrent = if (pos##ch >= token##start && pos##ch < token##_end) {
            true
          }
          else {
            isCurrentToken
          };

          let token_str = String.trim token##string;
          switch token_str  {
            | "" => {
              if isCurrent {
                end_ := token##_end;
                result
              }
              else {
                it tl "" isCurrent
              }
            }
            | _ => {
              let t = token##string;
              if (t != "]" && t != ")") {
                start := token##start;
                end_ := token##_end + 1;
              };
              let first_ch = Char.escaped (token_str.[0]);
              let last_ch = Char.escaped (token_str.[(String.length token_str) - 1]);
              let first_match = Js.Re.test first_ch token_regexp;
              let last_match = Js.Re.test last_ch token_regexp;
              let token_str = if (first_match == false) {
                String.sub token_str 1 ((String.length token_str) - 1)
              } else {
                token_str
              };
              if (last_match == false && String.length token_str > 0) {
                let r = String.sub token_str 0 ((String.length token_str) - 1);
                it tl (result ^ r) isCurrent
              } else {
                it tl (result ^ token_str) isCurrent
              };
            }
          };
        }
        | [] => {
          result
        }
      }
    };
    let result = it (Array.to_list lineTokens) "" false;
    (!start, !end_, result)
  };
  let autocompleteSuggestions codeMirror => {
    let cur = codeMirror##getCursor ();
    let (start, end_, token) = getToken codeMirror cur;
    let start = start + 1;
    if (token != "") {
      JsPromise.(
        CodeExecution.postMessage (CompletePrefix start end_ token) |>
        then_ (
          fun response => {
            let suggestions:
              array (
                Js.t {
                  .
                  name : string, doc : string, desc : string, kind : string
                }
              ) = response##suggestions;
            if (Js.Array.length suggestions == 0) {
              JsPromise.resolve response
            } else {
              let suggestions =
                Js.Array.map
                  (
                    fun suggestion => {
                        "title": suggestion##name,
                        "doc": suggestion##doc,
                        "desc": suggestion##desc,
                        "kind": suggestion##kind
                    }
                  )
                  suggestions;
              let autoCompleteData = {
                "list": suggestions,
                "from": {"line": cur##line, "ch": start},
                "to": {"line": cur##line, "ch": end_}
              };
              codeMirror##showHint {"hint": fun _ => autoCompleteData};
              JsPromise.resolve response
            }
          }
        )
      )
    } else {
      JsPromise.resolve {"suggestions": [||]}
    }
  };
  let console (codeState, message) {ReasonReact.state: state} =>
    ReasonReact.Update {...state, console: message, codeState};
  let handleCodeTypePhase self response => {
    let update = self.ReasonReact.update;
    update codeMirrorAction removeMarks;
    let msgType = response##msgType;
    switch msgType {
    | "Output" => update console (Error, response##message)
    | "TypetexpError"
    | "TypemodError"
    | "TypecoreError"
    | "LexerError"
    | "SyntaxError" =>
      let msg = response##message;
      let match = Js.Re.test msg unboundRegexp;
      let showErrors () => {
        let locations = response##locations;
        let locations =
          Array.map
            (
              fun loc => {
                let locStart = {
                  "line": loc##locStart##posLnum - 1,
                  "ch": loc##locStart##posCnum - loc##locStart##posBol
                };
                let locEnd = {
                  "line": loc##locEnd##posLnum - 1,
                  "ch": loc##locEnd##posCnum - loc##locEnd##posBol
                };
                {locStart, locEnd}
              }
            )
            locations;
        update codeMirrorAction (highlightLocations locations);
        update console (Error, response##message)
      };
      if match {
        JsPromise.(
          update
            codeMirrorAction
            (
              fun editor => {
                autocompleteSuggestions editor |>
                then_ (
                  fun res => {
                    if (Js.Array.length res##suggestions == 0) {
                      showErrors ()
                    };
                    JsPromise.resolve ()
                  }
                );
                ()
              }
            )
        )
      } else {
        showErrors ()
      }
    | "NoSyntaxErrors" => update console (Executable, "")
    | _ => failwith "Not handled"
    };
    JsPromise.resolve response
  };
  let onChange self newCode => {
    let update = self.ReasonReact.update;
    let handleCodeTypePhase = handleCodeTypePhase self;
    JsPromise.(
      CodeExecution.(postMessage (Type newCode)) |> then_ handleCodeTypePhase
    );
    update console (Busy, "")
  };
  let executeCode self e => {
    let update = self.ReasonReact.update;
    update
      codeMirrorAction
      (
        fun editor => {
          JsPromise.(
            CodeExecution.(postMessage (Execute (editor##getValue ()))) |>
            then_ (
              fun response => {
                update console (Result, response##result);
                JsPromise.resolve response
              }
            )
          );
          ()
        }
      )
  };
  let setCodeMirrorRef self codeMirrorInstance  {ReasonReact.state: state} => {
    let f = Js.Null.to_opt codeMirrorInstance;
    ReasonReact.SilentUpdate {
      ...state,
      codeMirrorRef: f
    }
  };

  let setTooltip tooltip {ReasonReact.state: state} =>
    ReasonReact.Update {...state, tooltip};
  let onMouseMove self e => {
    let update = self.ReasonReact.update;
    let left = ReactEventRe.Mouse.pageX e;
    let top = ReactEventRe.Mouse.pageY e;
    update
      codeMirrorAction
      (
        fun codeMirror => {
          if codeMirror##state##focused {
            let pos = codeMirror##coordsChar {"left": left, "top": top};
            let l = List.rev (Array.to_list (codeMirror##getLineTokens pos##line));
            if (List.length l > 0) {
              let x = List.hd l;
              let end2_ = (codeMirror##charCoords {"ch": x##_end, "line": pos##line});
              if (end2_##right >= left) {
                if pos##outside {
                  update setTooltip None
                } else {
                  let (start, end_, token) = getToken codeMirror pos;
                  let startChar =
                    codeMirror##charCoords {"ch": start, "line": pos##line};
                  let start = startChar##left;
                  let top = startChar##bottom;
                  /* let end_ = (codeMirror##charCoords {"ch": end_, "line": pos##line})##right; */
                  /* print_endline token; */
                  JsPromise.(
                    CodeExecution.(
                      postMessage (TypeExpression (pos##line + 1) pos##ch token)
                    ) |>
                    then_ (
                      fun response => {
                        let info = response##_type;
                        if (info == "") {
                          update setTooltip None
                        } else {
                          update setTooltip (Some (info, top, start))
                        };
                        JsPromise.resolve response
                      }
                    )
                  );
                  ()
                }
              }
              else {

                update setTooltip None
              }
            }
          }
        }
      )

  };

  let onFocus self event {ReasonReact.state: state} => {
    let ref = state.codeMirrorRef;
    switch ref {
    | Some ref =>
      let ref = ReasonReact.refToJsObj ref;
      onChange self ((ref##getCodeMirror ())##getValue ());
    | None => ()
    };
    ReasonReact.NoUpdate
  };

  let make value::(value: string) children => {
    ...component,
    initialState: fun () => {
      console: "",
      errorLocations: [||],
      codeState: Busy,
      codeMirrorRef: None,
      tooltip: None
    },
    render: fun self =>
      <div onMouseMove=(debounceReactEvent (onMouseMove self) 300) onFocus=(self.update (onFocus self))>
        <CodeMirror
          className="og-editor"
          value
          options={
            "mode": "ocaml",
            "lineNumbers": false,
            "matchBrackets": true,
            "styleActiveLine": true
          }
          onChange=(debounce (onChange self) 300)
          ref=(self.update (setCodeMirrorRef self))
        />
        <div className="og-console">
          (
            switch self.state.codeState {
            | Busy => <div className="og-icon og-busy-icon" />
            | Executable =>
              <div
                onClick=(executeCode self)
                className="og-icon og-execute-icon"
              />
            | Error => <div className="og-icon og-error-icon" />
            | Result => <div className="og-icon og-result-icon" />
            }
          )
          <pre className="og-console-text">
            (ReasonReact.stringToElement self.state.console)
          </pre>
        </div>
        (
          switch self.state.tooltip {
          | Some (tooltip, top, left) =>
            <div
              className="og-tooltip"
              style=(
                ReactDOMRe.Style.make
                  top::(string_of_int top ^ "px")
                  left::(string_of_int left ^ "px")
                  ()
              )>
              (ReasonReact.stringToElement tooltip)
            </div>
          | None _ => <div />
          }
        )
      </div>
  };
};

{
  let gistBlocks = querySelectorAll document "div[data-ocaml]";
  Array.iter
    (
      fun gistBlock => {
        let textContent = trim (textContent gistBlock);
        ReactDOMRe.render <Gist value=textContent /> gistBlock
      }
    )
    gistBlocks
};
