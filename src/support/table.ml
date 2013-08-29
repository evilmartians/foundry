open Sexplib.Std
open Unicode.Std
open ExtList

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

let replace table other =
  Hashtbl.iter (Hashtbl.replace table) other

let copy = Hashtbl.copy

let set = Hashtbl.replace

let get table key =
  try  Some (Hashtbl.find table key)
  with Not_found -> None

let get_exn = Hashtbl.find

let exists = Hashtbl.mem

let is_empty table =
  Hashtbl.length table = 0

let iter ?(ordered=false) ~f table =
  if ordered then
    let lst = Hashtbl.fold (fun k v acc -> (k, v) :: acc) table [] in
    let lst = List.sort ~cmp:(fun (k1,_) (k2,_) -> compare k1 k2) lst in
    List.iter (fun (k, v) -> f k v) lst
  else
    Hashtbl.iter f table

let pair k v =
  newtable 1
    (fun table -> Hashtbl.add table k v)

let map ~f table =
  newtable (Hashtbl.length table)
    (fun result ->
      iter (fun k v -> Hashtbl.add result k (f v)) table)

let map2 ~f left right =
  if (Hashtbl.length left) <> (Hashtbl.length right) then
    raise (Invalid_argument ("Table.map2" :> latin1s));
  try
    newtable (Hashtbl.length left)
      (fun result ->
        iter (fun k v ->
          Hashtbl.add result k (f v (Hashtbl.find right k))) left)
  with Not_found ->
    raise (Invalid_argument ("Table.fold2" :> latin1s))

let map_list ?(ordered=false) ~f table =
  let dest = ref [] in
  iter ~ordered ~f:(fun k v -> dest := (f k v) :: !dest) table;
  List.rev !dest

let fold ~f acc table =
  Hashtbl.fold (fun k v a -> f k a v) table acc

let fold2 ~f acc left right =
  if (Hashtbl.length left) <> (Hashtbl.length right) then
    raise (Invalid_argument ("Table.fold2" :> latin1s));
  try
    fold acc left ~f:(fun k acc v ->
      f k acc v (Hashtbl.find right k))
  with Not_found ->
    raise (Invalid_argument ("Table.fold2" :> latin1s))

let filter ~f table =
  newtable (Hashtbl.length table)
    (fun result ->
      Hashtbl.iter (fun key value ->
        if f key value then
          Hashtbl.add result key value) table)

let join left right =
  newtable (max (Hashtbl.length left) (Hashtbl.length right))
    (fun table ->
      Hashtbl.iter (Hashtbl.add table) left;
      Hashtbl.iter (Hashtbl.replace table) right)

let keys table =
  Hashtbl.fold (fun k v acc -> k :: acc) table []
