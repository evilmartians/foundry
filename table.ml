open Sexplib.Std

type 'a t = (string, 'a) Hashtbl.t
with sexp_of

let newtable arg f =
  let table = Hashtbl.create (Hashtbl.length arg) in
    f table; table

let create () =
  Hashtbl.create 1

let set arg k v =
  Hashtbl.replace arg k v

let get arg k =
  if Hashtbl.mem arg k then
    Some (Hashtbl.find arg k)
  else None

let pair k v =
  let table = Hashtbl.create 1 in
    Hashtbl.add table k v;
    table

let map f arg =
  newtable arg (fun table ->
    Hashtbl.iter (fun k v -> Hashtbl.add table k (f v)) arg)

let join l r =
  newtable l (fun table ->
    Hashtbl.iter (Hashtbl.add table) l;
    Hashtbl.iter (Hashtbl.replace table) r)
