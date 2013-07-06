open Sexplib.Std

type 'a t = (string, 'a) Hashtbl.t
with sexp_of

let newtable len f =
  let table = Hashtbl.create len in
    f table; table

let fill table lst =
  List.iter (fun (k, v) -> Hashtbl.replace table k v) lst

let create lst =
  newtable (List.length lst)
    (fun table -> fill table lst)

let copy = Hashtbl.copy

let set = Hashtbl.replace

let get = ExtHashtbl.Hashtbl.find_option

let exists = Hashtbl.mem

let pair k v =
  newtable 1
    (fun table -> Hashtbl.add table k v)

let map ~f table = ExtHashtbl.Hashtbl.map f table

let map_list ~f table =
  Hashtbl.fold (fun k v accum -> (f k v) :: accum) table []

let join l r =
  newtable (max (Hashtbl.length l) (Hashtbl.length r))
    (fun table ->
      Hashtbl.iter (Hashtbl.add table) l;
      Hashtbl.iter (Hashtbl.replace table) r)
