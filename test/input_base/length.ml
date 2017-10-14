open Base

let () = (
  let length = List.length ["Cat"; "Dog"] in
  print_string ("Length of list:" ^ string_of_int length);
)
