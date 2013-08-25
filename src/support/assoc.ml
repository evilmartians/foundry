open Sexplib.Std
open Unicode.Std
open ExtList

type ('a, 'b) t     = (string * 'a) list
and  sequental
and  sorted
and  'a sequental_t = ('a, sequental) t
and  'a sorted_t    = ('a, sorted) t
with sexp_of

let eq a b =
  (fst a) = (fst b)

let cmp a b =
  compare (fst a) (fst b)

let empty = []

let sequental lst =
  lst

let sorted lst =
  List.sort ~cmp lst

let sort = sorted

let is_empty assoc =
  assoc = []

let find assoc key =
  List.assoc key assoc

let find_option assoc key =
  try
    Some (find assoc key)
  with Not_found ->
    None

let index assoc key =
  fst (List.findi (fun idx (k, v) -> key = k) assoc)

let mem assoc key =
  List.mem_assoc key assoc

let equal ~eq a b =
  try
    List.fold_left2 (fun acc (ka, va) (kb, vb) ->
        acc && ka == kb && eq va vb)
      true a b
  with Invalid_argument _ ->
    false

let map ~f assoc =
  List.map (fun (key, value) ->
    key, f key value) assoc

let map_list ~f assoc =
  List.map (fun (key, value) ->
    f key value) assoc

let fold ~f acc assoc =
  List.fold_left (fun acc (key, value) ->
    f key acc value) acc assoc

let fold2 ~f acc lft rgt =
  assert false

let filter ~f assoc =
  List.filter (fun (key, value) ->
    f key value) assoc

let keys assoc =
  List.map fst assoc

let prepend assoc key value =
  (key, value) :: assoc

let append assoc key value =
  assoc @ [key, value]

let add assoc key value =
  List.unique ~cmp:eq (List.merge cmp assoc [key, value])

let merge lft rgt =
  List.unique ~cmp:eq (List.merge cmp lft rgt)

let remove assoc key =
  List.remove_assoc key assoc
