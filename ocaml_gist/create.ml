open Unix

let execute cmd =
  let s = String.concat " " cmd in
  let ret = Sys.command s in
  if ret <> 0
  then failwith (Printf.sprintf "Error: %s" s)

let buffer_size = 8192;;
let buffer = String.create buffer_size;;

let all_files_but_opams dir = (
  let all_files = Array.to_list (Sys.readdir dir) in
  List.filter (fun filename -> filename <> "META" && filename <> "opam" && filename <> "opam.config") all_files
)

let file_copy input_name output_name =
  let fd_in = openfile input_name [O_RDONLY] 0 in
  let fd_out = openfile output_name [O_WRONLY; O_CREAT; O_TRUNC] 0o666 in
  let rec copy_loop () = match read fd_in buffer 0 buffer_size with
    |  0 -> ()
    | r -> ignore (write fd_out buffer 0 r); copy_loop ()
  in
  copy_loop ();
  close fd_in;
  close fd_out;;

try
  let owl = Findlib.package_directory "ocaml-webworker" in
  let og = Findlib.package_directory "ocaml-gist" in
  let output = Cmdliner.(
    let output =
      let doc = "Output folder" in
      Arg.(value & opt string "." & info ["output"; "o"]  ~doc)
    in
    let input =
      let doc = "Input folder" in
      Arg.(value & opt string "." & info ["input"; "i"]  ~doc)
    in
    let deps =
      let doc = "Dependency (should be a .cmo/.cma file)" in
      Arg.(value & opt_all string [] & info ["dependency"; "d"]  ~doc)
    in
    let libs =
      let doc = "Libraries" in
      Arg.(value & opt_all string [] & info ["library"; "l"]  ~doc)
    in
    let x = ref None in
    let exec output input deps libs = ( x := Some output; ) in
    let t = Term.(const exec $ output $ input $ deps $ libs) in
    Term.eval (t, Term.info "og-create");
    let output = match !x with
    | Some output -> Filename.(concat current_dir_name output)
    | None -> Filename.current_dir_name
    in
    output
  ) in
  let owl_files = all_files_but_opams owl in
  let og_files = all_files_but_opams og in


  (* copy the files to the output *)


  print_endline output;




with
| Findlib.No_such_package (p, msg) -> print_endline (p ^ ":" ^ msg)
