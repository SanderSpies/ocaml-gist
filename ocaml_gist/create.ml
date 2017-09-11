open Unix

let execute cmd =
  let s = String.concat " " cmd in
  print_endline s;
  let ret = Unix.system s in
  match ret with
  | Unix.WEXITED _ -> ()
  | _ -> failwith (Printf.sprintf "Error: %s" s)

let buffer_size = 8192;;
let buffer = Bytes.create buffer_size;;

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
  let (output, input, deps, libs) = Cmdliner.(
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
    let out = ref Filename.current_dir_name in
    let input_ = ref Filename.current_dir_name in
    let dependencies = ref [] in
    let libraries = ref [] in
    let exec output input deps libs = (
      out := output;
      input_ := input;
      dependencies := deps;
      libraries := libs;
    ) in
    let t = Term.(const exec $ output $ input $ deps $ libs) in
    ignore(Term.eval (t, Term.info "og-create"));
    (!out, !input_, !dependencies, !libraries)
  ) in
  let owl_files = all_files_but_opams owl in
  let og_files = all_files_but_opams og in

  if Sys.file_exists output = false then
    Unix.mkdir output 0o755;

  (* copy the files to the output *)
  List.iter (fun file -> file_copy (Filename.concat owl file) (Filename.concat output file)) owl_files;
  List.iter (fun file -> file_copy (Filename.concat og file) (Filename.concat output file)) og_files;

  (* packages that should be available in the gist tool *)
  let export_packages = List.fold_left (fun deps acc -> (
      deps ^
      (if acc.[0] = '+' then
        " -jsopt " ^ acc
      else
        " -export-package " ^ acc
      )
    )) "" libs in

  (* local libraries that should be available in the gist tool*)
  let deps2 = List.fold_left (fun deps acc -> deps ^ "-jsopt \" -I . --file " ^ acc ^ "\"") "" deps in

  (* create the actual webworker *)
  execute ([ "jsoo_mktop";
            "-jsopt"; "\"--disable genprim\"";
            (* "-g"; *)
            (* "-jsopt"; "--source-map-inline"; *)
            "-package"; "str";
            "-package"; "unix";
            (* "-jsopt"; "--no-inline"; *)
            (* "-jsopt"; "--pretty"; *)
            (* "-jsopt"; "--debug-info"; *)
            (* "-jsopt";"--opt=3"; *)
            "-jsopt"; "+weak.js";
            "-jsopt"; "+toplevel.js";
            "-jsopt"; "+nat.js";
            "-jsopt"; "+dynlink.js";
            export_packages;
            deps2;
            (Filename.concat owl "ocaml_webworker.cma");
            "-o";(Filename.concat output "ocaml_webworker.js");
          ]);

  (* create the cmi files *)
  (* TODO: use cmti files instead when available *)
  let cmi_files = List.map (fun file -> Filename.chop_extension file ^ ".cmi") deps in
  execute (["jsoo_mkcmis"] @ libs @ cmi_files @ ["-o";Filename.concat output "cmi.js"])
with
| Findlib.No_such_package (p, msg) -> print_endline (p ^ ":" ^ msg)
