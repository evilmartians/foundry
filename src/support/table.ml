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

let get = ExtHashtbl.Hashtbl.find_option

let get_exn = Hashtbl.find

let exists = Hashtbl.mem

let empty table =
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
  ExtHashtbl.Hashtbl.map f table

let map2 ~f left right =
  assert ((Hashtbl.length left) = (Hashtbl.length right));
  newtable (Hashtbl.length left)
    (fun result ->
      iter (fun k v ->
        Hashtbl.add result k (f v (Hashtbl.find right k))) left)

let map_list ?(ordered=false) ~f table =
  let dest = ref [] in
  iter ~ordered ~f:(fun k v -> dest := (f k v) :: !dest) table;
  List.rev !dest

let fold ~f acc table =
  Hashtbl.fold (fun k v a -> f k a v) table acc

let fold2 ~f acc left right =
  assert ((Hashtbl.length left) = (Hashtbl.length right));
  fold acc left ~f:(fun k acc v ->
      f k acc v (Hashtbl.find right k))

let join left right =
  newtable (max (Hashtbl.length left) (Hashtbl.length right))
    (fun table ->
      Hashtbl.iter (Hashtbl.add table) left;
      Hashtbl.iter (Hashtbl.replace table) right)

let keys table =
  List.sort (List.of_enum (ExtHashtbl.Hashtbl.keys table))

let except_keys table keys =
  let table = copy table in
    List.iter (fun k -> Hashtbl.remove table k) keys;
    table

let equal_keys table other =
  (keys table) = (keys other)

let diff_keys table other =
  List.fold_left2
    (fun accum tk ok ->
      if tk = ok then accum else ok :: accum)
    [] (keys table) (keys other)

let includes_keys table other =
  let rec zip table other =
    match table, other with
    (* keys match -- ok *)
    | kt :: trest, ko :: orest when kt = ko
    -> zip trest orest
    (* keys do not match -- skip the key from table *)
    | _  :: trest, _  :: orest
    -> zip trest other
    (* table is empty, but there are keys in other -- not includes *)
    | [], _ :: _ -> false
    (* other is empty, and there may or may not be keys in table -- includes *)
    | _,  []     -> true
  in zip (keys table) (keys other)
