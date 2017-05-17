open Longident

let keep_suffix =
  let rec aux = function
    | Lident str ->
      if String.lowercase str <> str then
        Some (Lident str, false)
      else
        None
    | Ldot (t, str) ->
      if String.lowercase str <> str then
        match aux t with
        | None -> Some (Lident str, true)
        | Some (t, is_label) -> Some (Ldot (t, str), is_label)
      else
        None
    | t -> Some (t, false) (* Can be improved... *)
  in
  function
  | Lident s -> Lident s, false
  | Ldot (t, s) ->
    begin match aux t with
    | None -> Lident s, true
    | Some (t, is_label) -> Ldot (t, s), is_label
    end
  | otherwise -> otherwise, false
