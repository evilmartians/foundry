open Unicode.Std

type t = (string, unit) Hashtbl.t

let create () =
  Hashtbl.create 10

let copy symtab =
  Hashtbl.copy symtab

let add symtab name =
  let rec find_name name suffix =
    let name' =
      if name = "" then (string_of_int suffix)
      else name ^ "." ^ (string_of_int suffix)
    in
    if Hashtbl.mem symtab name' then
      find_name name (suffix + 1)
    else
      name'
  in
  let name =
    if name = "" || Hashtbl.mem symtab name
    then find_name name 1
    else name
  in
  Hashtbl.add symtab name ();
  name

let remove symtab name =
  Hashtbl.remove symtab name

let update symtab name name' =
  remove symtab name;
  add symtab name'
