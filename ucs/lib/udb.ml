open Codepoint

let count = Array.length Udb_data.data

let by f =
  let table = lazy (
    let table = Hashtbl.create count in
    Array.iter (fun cp -> Hashtbl.add table (f cp) cp) Udb_data.data;
    table
  ) in
  fun x ->
    List.sort (fun { code = c1 } { code = c2 } ->
      c1 - c2
    ) (Hashtbl.find_all (Lazy.force table) x)

let by_code		= by (fun cp -> cp.code)
let by_name		= by (fun cp -> cp.name)
let by_category		= by (fun cp -> cp.gc)
let by_combining	= by (fun cp -> cp.ccc)
let by_bidi		= by (fun cp -> cp.bc)
let by_decomp		= by (fun cp -> cp.cdm)
