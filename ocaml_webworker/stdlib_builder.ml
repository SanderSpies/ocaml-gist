let execute cmd =
  let s = String.concat " " cmd in
  let ret = Sys.command s in
  if ret <> 0
  then failwith (Printf.sprintf "Error: %s" s)

let () = (
  if Sys.file_exists "cmtis" = false then
    Unix.mkdir "cmtis" 0o755;
  let compiler_libs_location = Findlib.package_directory "compiler-libs" in
  let ocaml_stdlib_folder = Filename.dirname compiler_libs_location in
  if Sys.file_exists "stdlib.cmis.js" = true then
    failwith "stdlib.cmis.js already exists";
  let all_cmtis = List.filter (fun file_name -> Filename.extension file_name = ".cmti") (Array.to_list (Sys.readdir ocaml_stdlib_folder)) in
  List.iter (fun pkg -> execute ["cp"; ocaml_stdlib_folder ^ Filename.dir_sep ^ pkg; "./cmtis/" ^ Filename.chop_extension pkg ^ ".cmi"]) all_cmtis;
  execute ["jsoo_mkcmis"; "./cmtis/*.cmi"; "-o"; "stdlib.cmis.js"];  
);
