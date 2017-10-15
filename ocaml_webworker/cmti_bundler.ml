(* 
  taken and modified from jsoo_common.ml and other JSOO files 

  Note: license here is different from other files. 

 * Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2014 Hugo Heuzard
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
*)

let input_s ic size =
  let b = Bytes.create size in
  really_input ic b 0 size;
  Bytes.unsafe_to_string b


let unit_of_cma filename =
  let ic = open_in_bin filename in
  let len_magic_number = String.length Config.cma_magic_number in
  let magic_number = input_s ic len_magic_number in
  if magic_number <> Config.cma_magic_number
  then failwith "not a cma file";
  let toc_pos = input_binary_int ic in
  seek_in ic toc_pos;
  let lib = (input_value ic : Cmo_format.library) in
  close_in ic;
  List.map (fun u -> u.Cmo_format.cu_name) lib.Cmo_format.lib_units

let read_cmi ~dir cmi =
  let with_name file =
    let cmi_path =
      if Filename.is_relative file
      then Filename.concat dir file
      else file in
    if Sys.file_exists cmi_path
    then
      begin
        (* if !verbose then Format.eprintf "include %s@." cmi_path; *)
        cmi_path
      end
    else raise Not_found
  in
  try with_name (String.uncapitalize_ascii cmi)
  with Not_found ->
  try with_name (String.capitalize_ascii cmi)
  with Not_found ->
    (* Format.eprintf "Could not find cmi %s or %s in %s@."
      (String.capitalize_ascii cmi)
      (String.uncapitalize_ascii cmi) dir *)
    raise Not_found



let filter_map f l =
  let l = List.fold_left (fun acc x -> match f x with
    | Some x -> x::acc
    | None -> acc) [] l
  in List.rev l
;;

let cmis_of_cma ~dir cma_path =
let cma_path =
  if Filename.is_relative cma_path
  then Filename.concat dir cma_path
  else cma_path
in
let contains = unit_of_cma cma_path in
let dir = Filename.dirname cma_path in
filter_map
  (fun s -> try Some (read_cmi ~dir (s ^ ".cmi")) with Not_found -> None)
  contains


let cmis_of_package pkg : string list =
try
  let dir = Findlib.package_directory pkg in
  let fs : string list ref = ref [] in
  let add filename = fs:=filename::!fs in
  let archive =
    try
      Findlib.package_property ["byte"] pkg "archive"
    with exc ->
      if pkg = "stdlib"
      then "stdlib.cma"
      else raise exc
    in
  let n = String.split_on_char ' ' archive in
  List.iter (fun x ->
      if Filename.check_suffix x ".cmo"
      then
        let u = Filename.chop_suffix x ".cmo" in
        add (read_cmi ~dir (u ^ ".cmi"))
      else if Filename.check_suffix x ".cma"
      then List.iter add (cmis_of_cma ~dir x)
      else if Filename.check_suffix x ".cmi"
      then add (read_cmi ~dir (Filename.chop_suffix x ".cmi"))
      else Format.eprintf "Wrong extention for archive %s@." x
    ) n;
    !fs
with exn -> Format.eprintf "Error for package %s@." pkg;
      raise exn

let execute cmd =
  let s = String.concat " " cmd in
  let ret = Sys.command s in
  if ret <> 0
  then print_endline (Printf.sprintf "Error: %s" s);;

let () = (
  let args = List.tl (Array.to_list Sys.argv) in

  if List.length args < 1 then
    failwith "Expected at least one argument";

  if Sys.file_exists "cmtis" = false then
    Unix.mkdir "cmtis" 0o755;

  List.iter Sys.remove (Array.to_list (Sys.readdir "cmtis"));

  List.iter (fun package ->
    let cmis = cmis_of_package package in
    List.iter (fun cmi ->
      let cmt = if Sys.file_exists ((Filename.chop_extension cmi) ^ ".cmti") then
        (Filename.chop_extension cmi) ^ ".cmti"
      else if Sys.file_exists ((Filename.chop_extension cmi) ^ ".cmt") then
        (Filename.chop_extension cmi) ^ ".cmt"
      else
        cmi
      in
      execute ["cp"; cmt; "./cmtis/" ^ Filename.chop_extension (Filename.basename cmi) ^ ".cmi"]
    ) cmis
  ) args;
  execute ["jsoo_mkcmis"; "./cmtis/*.cmi"; "-o"; "output.js"];
);
