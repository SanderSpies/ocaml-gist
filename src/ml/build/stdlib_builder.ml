let execute cmd =
  let s = String.concat " " cmd in
  let ret = Sys.command s in
  if ret <> 0
  then failwith (Printf.sprintf "Error: %s" s)


let () = (
  let compiler_libs_location = Findlib.package_directory "compiler-libs" in
  let ocaml_stdlib_folder = Filename.dirname compiler_libs_location in
  let all_cmtis = List.filter (fun file_name -> Filename.extension file_name = ".cmti") (Array.to_list (Sys.readdir ocaml_stdlib_folder)) in
  List.iter (fun pkg -> execute ["cp"; ocaml_stdlib_folder ^ Filename.dir_sep ^ pkg; "./cmtis/" ^ Filename.chop_extension pkg ^ ".cmi"]) all_cmtis;
  List.iter (fun pkg -> execute ["jsoo_mkcmis"; "./cmtis/" ^ Filename.chop_extension pkg ^ ".cmi"]) all_cmtis;
  List.iter (fun pkg -> execute ["cat"; "./cmtis/" ^ Filename.chop_extension pkg ^ ".cmi.cmis.js"; ">>"; "./stdlib2.cmis.js"; ]) all_cmtis;
);
