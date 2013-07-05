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

let set arg k v =
  Hashtbl.replace arg k v

let get arg k =
  if Hashtbl.mem arg k then
    Some (Hashtbl.find arg k)
  else None

let pair k v =
  newtable 1
    (fun table -> Hashtbl.add table k v)

let map ~f arg =
  newtable (Hashtbl.length arg)
    (fun table ->
      Hashtbl.iter (fun k v -> Hashtbl.add table k (f v)) arg)

let map_list ~f arg =
  Hashtbl.fold (fun k v accum -> (f k v) :: accum) arg []

let join l r =
  newtable (max (Hashtbl.length l) (Hashtbl.length r))
    (fun table ->
      Hashtbl.iter (Hashtbl.add table) l;
      Hashtbl.iter (Hashtbl.replace table) r)
