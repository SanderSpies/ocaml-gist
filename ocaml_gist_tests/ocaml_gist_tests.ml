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

let og_create_no_arguments () = (
  let og_create_help = execute ["og-create --help"] in
  let og_create_no_args = execute ["og-create"] in
  Alcotest.(check string) "og-create === og-create --help" og_create_help og_create_no_args;
)

(* a very simple test if creating a stdlib actually does something *)
let og_create_stdlib () = (
  let command = execute ["og-create --lib stdlib --input input_stdlib --output output_stdlib --doc"] in
  let nr_of_files = Array.length (Sys.readdir "output_stdlib") in
  Alcotest.(check int) "check if number of files matches what we expect" nr_of_files 10;
)

let og_create_set = [
  "should show documentation when no arguments are given", `Slow, og_create_no_arguments;
  "should create a stdlib gist", `Slow, og_create_stdlib;
]

let () = Alcotest.run "ocaml-gist tests" [
  "og-create", og_create_set
]
