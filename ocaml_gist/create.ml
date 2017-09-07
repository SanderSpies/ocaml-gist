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
  let (output, deps) = Cmdliner.(
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
    let out = ref None in
    let dependencies = ref [] in
    let exec output input deps libs = (
      out := Some output;
      dependencies := deps;
    ) in
    let t = Term.(const exec $ output $ input $ deps $ libs) in
    Term.eval (t, Term.info "og-create");
    let output = match !out with
    | Some output -> Filename.(concat current_dir_name output)
    | None -> Filename.current_dir_name
    in
    (output, !dependencies)
  ) in
  let owl_files = all_files_but_opams owl in
  let og_files = all_files_but_opams og in

  if Sys.file_exists output = false then
    Unix.mkdir output 0o755;

  (* copy the files to the output *)
  List.iter (fun file -> file_copy (Filename.concat owl file) (Filename.concat output file)) owl_files;
  List.iter (fun file -> file_copy (Filename.concat og file) (Filename.concat output file)) og_files;

  let fs_files = List.fold_left (fun deps acc -> deps ^ "--file " ^ acc) "" deps in
  let cmi_files = List.map (fun file -> Filename.chop_extension file ^ ".cmi") deps in
  execute ["js_of_ocaml";fs_files;"--extern-fs";"-I";".";"--ofs=fs.js";List.hd deps;];
  execute (["jsoo_mkcmis"] @ cmi_files @ ["-o";"cmi.js"]);
  file_copy "./fs.js" (Filename.concat output "fs.js");
  file_copy "./cmi.js" (Filename.concat output "cmi.js");
with
| Findlib.No_such_package (p, msg) -> print_endline (p ^ ":" ^ msg)
