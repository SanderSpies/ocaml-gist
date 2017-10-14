let (>>=) = Lwt.bind

let () = (
  Lwt_js.sleep 2. >>= fun () ->
    print_endline "Yawn..."
)
