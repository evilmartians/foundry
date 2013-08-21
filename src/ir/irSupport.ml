open Unicode.Std

let is_printable (str : string) =
  try
    ignore (Scanf.sscanf (str :> latin1s) "%[A-Za-z0-9_.:]%!" (fun x -> ()));
    true
  with Scanf.Scan_failure _ ->
    false

let is_digits (str : string) =
  try
    ignore (Scanf.sscanf (str :> latin1s) "%[0-9]%!" (fun x -> ()));
    true
  with Scanf.Scan_failure _ ->
    false

let escaped str =
  str (* TODO *)
