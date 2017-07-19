let () = (
  let worker = Worker.create "ocaml_webworker.js" in
  worker##.onmessage := Dom.handler (fun msg -> (
    Firebug.console##info (Js.Unsafe.inject msg##.data);
    Js.bool false
  ));
  Js.Unsafe.global##.shared := worker;
  Firebug.console##log (Js.Unsafe.inject Js.Unsafe.global##.shared)
)
