open Core_kernel

let () = (
  let zone = Core_kernel.Time.Zone.of_utc_offset ~hours:2 in
  if Date.is_weekday (Date.today ~zone) then
    print_endline "A weekday"
  else
    print_endline "Not a weekday"
)
