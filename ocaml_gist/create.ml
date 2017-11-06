open Unix

let execute cmd =
  let s = String.concat " " cmd in
  let ret = Unix.open_process_in s in
  let output = ref "" in
  (try
    while true do
      let l = input_line ret in
      output := !output ^ l ^ "\n"
    done
  with End_of_file -> ());
  !output

let filtered_files dir = (
  let all_files = Array.to_list (Sys.readdir dir) in
  List.filter (fun filename -> filename <> "index.html" && filename <> "META" && filename <> "opam" && filename <> "opam.config") all_files
)

let read_file filename = (
  let buffer_size = 8192 in
  let buffer = Bytes.create buffer_size in
  let fd_in = openfile filename [O_RDONLY] 0 in
  let result = ref "" in
  let rec read_loop () = match read fd_in buffer 0 buffer_size with
    | 0 -> ()
    | r -> result := !result ^ buffer; read_loop ()
  in
  read_loop ();
  !result)

let write_file file s =
   ( let channel = open_out file in
     output_string channel s;
     close_out channel )

let create_index_html og_folder input output_folder = (
  let folder = Filename.concat (Sys.getcwd ()) input in
  let files = Array.to_list (Sys.readdir folder) in
  let inputHTML = ref "" in
  List.iter (fun f -> (
    let filename = (Filename.concat folder f) in
    if not (Sys.is_directory filename) then (
      inputHTML := !inputHTML ^ "<div data-ocaml>" ^ (read_file filename) ^ "</div>"
    )
  )) files;
  let index_file = read_file (Filename.concat og_folder "index.html") in
  let index_content = Str.replace_first (Str.regexp "<!-- replace me -->") !inputHTML index_file in
  write_file (Filename.concat output_folder "index.html") index_content
)

let file_copy input_name output_name =
let buffer_size = 8192 in
let buffer = Bytes.create buffer_size in
  if (Sys.file_exists output_name) then
    failwith ("output name already exists:" ^ output_name);
  let fd_in = openfile input_name [O_RDONLY] 0 in
  let fd_out = openfile output_name [O_WRONLY; O_CREAT; O_TRUNC] 0o666 in
  let rec copy_loop () = match read fd_in buffer 0 buffer_size with
    |  0 -> ()
    | r -> ignore (write fd_out buffer 0 r); copy_loop ()
  in
  copy_loop ();
  close fd_in;
  close fd_out;;

let create_temp_dir () = (
  let tmp = Printf.sprintf "%.30f" (Unix.gettimeofday ()) in
  let tmp_dir = ref (Filename.concat (Filename.get_temp_dir_name ()) tmp) in
  while Sys.file_exists !tmp_dir do
    let tmp = Printf.sprintf "%.30f" (Unix.gettimeofday ()) in
    tmp_dir := Filename.concat (Filename.get_temp_dir_name ()) tmp
  done;
  Unix.mkdir !tmp_dir 0o755;
  !tmp_dir
)

let do_stuff owl og output input libs deps useDocs = (
  let tmp_dir = create_temp_dir () in

  let owl_files = filtered_files owl in
  let og_files = filtered_files og in

  if Sys.file_exists output = false then
    Unix.mkdir output 0o755;

  (* copy the files to the output *)
  List.iter (fun file -> file_copy (Filename.concat owl file) (Filename.concat tmp_dir file)) owl_files;
  List.iter (fun file -> file_copy (Filename.concat og file) (Filename.concat tmp_dir file)) og_files;
  let folder = Sys.getcwd () in
  let files = List.filter (fun f -> not (Sys.is_directory f)) (Array.to_list (Sys.readdir folder)) in
  List.iter (fun file -> file_copy (Filename.concat folder file) (Filename.concat tmp_dir file)) files;
  create_index_html og input tmp_dir;

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
  let result = execute ([ "cd " ^ tmp_dir; "&&";"jsoo_mktop";
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
            "-o";(Filename.concat tmp_dir "ocaml_webworker.js");
          ])
  in
  print_endline result;

  (* create the cmi files *)
  (* TODO: use cmti files instead when available *)
  let cmi_files = List.map (fun file -> Filename.chop_extension file ^ ".cmi") deps in
  let non_js = List.filter (fun file ->
    if file.[0] <> '+' then
      true
    else
      false
    ) libs in
  let js = List.filter (fun file ->
    if file.[0] = '+' then
      true
    else
      false
    ) libs in
  (if List.length non_js > 0 then
    (if useDocs then
      let cmti_bundler_results = execute (["cd"; tmp_dir; "&&"; "cmti-bundler"] @ non_js) in
      print_endline cmti_bundler_results;
      let cmi_result = execute (["cd"; tmp_dir; "&&"; "jsoo_mkcmis"; "./cmtis/*.cmi"] @ js @ cmi_files @ ["-o";Filename.concat tmp_dir "cmi.js"]) in
      print_endline cmi_result
    else
      let cmi_result = execute (["cd"; tmp_dir; "&&"; "jsoo_mkcmis"] @ non_js @ cmi_files @ ["-o";Filename.concat tmp_dir "cmi.js"]) in
      print_endline cmi_result
    )
  else
    (let cmi_js = execute (["cd"; tmp_dir; "&&"; "jsoo_mkcmis"] @ js @ cmi_files @ ["-o";Filename.concat tmp_dir "cmi.js"]) in
    print_endline cmi_js));

  let mv_result = execute (["mv"; Filename.concat tmp_dir "*.js"; Filename.concat tmp_dir "*.html"; Filename.concat tmp_dir "*.svg"; Filename.concat tmp_dir "*.css"; output]) in
  print_endline mv_result;

)

let create () = (
  try
    let owl = Findlib.package_directory "ocaml-webworker" in
    let og = Findlib.package_directory "ocaml-gist" in
    Cmdliner.(
      let output =
        let doc = "Output folder" in
        Arg.(value & opt dir "." & info ["output"; "o"]  ~doc)
      in
      let input =
        let doc = "Input folder" in
        Arg.(value & opt dir "." & info ["input"; "i"]  ~doc)
      in
      let deps =
        let doc = "Dependency (should be a .cmo/.cma file)" in
        Arg.(value & opt_all file [] & info ["dependency"; "d"]  ~doc)
      in
      let libs =
        let doc = "Libraries" in
        Arg.(value & opt_all string [] & info ["library"; "l"]  ~doc)
      in
      let useDocs =
        let doc = "Show documentation (increases filesize considerably)" in
        Arg.(value & flag & info ["doc"]  ~doc)
      in
      let out = ref Filename.current_dir_name in
      let input_ = ref Filename.current_dir_name in
      let dependencies = ref [] in
      let libraries = ref [] in
      let useDocs_ = ref false in
      let exec output input deps libs docs = (
        out := output;
        input_ := input;
        dependencies := deps;
        libraries := libs;
        useDocs_ := docs;
        if !out = "." && !input_ = "." && !dependencies = [] && !libraries = [] && !useDocs_ = false then
          ignore(Unix.system "og-create --help") (* sue me *)
        else (
          do_stuff owl og !out !input_ !libraries !dependencies !useDocs_
        )
      ) in
      let t = Term.(const exec $ output $ input $ deps $ libs $ useDocs) in
      ignore(Term.eval (t, Term.info "og-create"));
    )
  with
  | Findlib.No_such_package (p, msg) -> print_endline (p ^ ":" ^ msg)
)

let () = create ()
