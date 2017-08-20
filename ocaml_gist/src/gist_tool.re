open Bs_webapi.Dom;

external querySelectorAll : Dom.document => string => array Dom.element =
  "" [@@bs.send];

external textContent : Dom.element => string = "" [@@bs.get];

external trim : string => string = "" [@@bs.send];

module CodeMirror = {
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
  type resolve 'a = 'a => unit;
  external make :
    (resolve::resolve 'a => reject::(exn => unit) [@bs] => unit) => t 'a =
    "Promise" [@@bs.new];
  external then_ : ('a => t 'b) [@bs.uncurry] => t 'b =
    "then" [@@bs.send.pipe : t 'a];
  external resolve : 'a => t 'a = "resolve" [@@bs.val] [@@bs.scope "Promise"];
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
    | TypeExpression int int string;
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
    };
    JsPromise.make (
      fun ::resolve ::reject => MsgMap.set awaitingResponses uniqueId resolve
    )
  };
};

module Gist = {
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
    | "NoSyntaxErrors" => update console (Executable, "")
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
  let setCodeMirrorRef codeMirrorInstance {ReasonReact.state: state} =>
    ReasonReact.SilentUpdate {
      ...state,
      codeMirrorRef: Js.Null.to_opt codeMirrorInstance
    };
  let onFocusChange self hasFocus =>
    if hasFocus {
      let update = self.ReasonReact.update;
      update
        codeMirrorAction
        (fun codeMirror => onChange self (codeMirror##getValue ()))
    };
  type pos = Js.t {. line : int, ch : int, outside : bool};
  let getToken editor (pos: pos) => {
    let rec inner editor (pos: pos) result => {
      let token = editor##getTokenAt pos;
      let str = String.trim token##string;
      switch str {
      | "" =>
        if (List.length result == 0) {
          ((-1), (-1), "")
        } else {
          let str =
            String.concat "" (List.map (fun token => token##string) result);
          let lastToken = List.nth (List.rev result) 0;
          let lastTokenString = lastToken##string;
          if (lastTokenString.[0] == '.') {
            (lastToken##start + 1, lastToken##_end, str)
          } else {
            (lastToken##start, lastToken##_end, str)
          }
        }
      | _ =>
        let line: int = pos##line;
        let ch: int = token##start;
        let pos: pos = {"line": line, "ch": ch, "outside": false};
        inner editor pos ([token] @ result)
      }
    };
    inner editor pos []
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
          let pos = codeMirror##coordsChar {"left": left, "top": top};
          if pos##outside {
            ()
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
      )
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
      <div onMouseMove=(onMouseMove self)>
        <CodeMirror
          className="og-editor"
          value
          options={
            "mode": "ocaml",
            "lineNumbers": false,
            "matchBrackets": true,
            "styleActiveLine": true
          }
          onFocusChange=(onFocusChange self)
          onChange=(onChange self)
          ref=(self.update setCodeMirrorRef)
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
