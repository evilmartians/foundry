open Sexplib.Std

type 'a t = (string, 'a) Hashtbl.t
with sexp_of

let newtable len f =
  let table = Hashtbl.create len in
    f table; table

let create lst =
  newtable (List.length lst)
    (fun table ->
      List.iter (fun (k, v) -> Hashtbl.replace table k v) lst)

let copy table =
  Hashtbl.copy table

let set table k v =
  Hashtbl.replace table k v

let get table k =
  if Hashtbl.mem table k then
    Some (Hashtbl.find table k)
  else None

let pair k v =
  newtable 1
    (fun table -> Hashtbl.add table k v)

let map ~f table =
  newtable (Hashtbl.length table)
    (fun table ->
      Hashtbl.iter (fun k v -> Hashtbl.add table k (f v)) table)

let map_list ~f table =
  Hashtbl.fold (fun k v accum -> (f k v) :: accum) table []

let join l r =
  newtable (max (Hashtbl.length l) (Hashtbl.length r))
    (fun table ->
      Hashtbl.iter (Hashtbl.add table) l;
      Hashtbl.iter (Hashtbl.replace table) r)
