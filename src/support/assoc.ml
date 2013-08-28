open Sexplib.Std
open Unicode.Std

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
  List.sort cmp lst

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
  fst (ExtList.List.findi (fun idx (k, v) -> key = k) assoc)

let mem assoc key =
  List.mem_assoc key assoc

let equal ~eq a b =
  try
    List.fold_left2 (fun acc (ka, va) (kb, vb) ->
        acc && ka = kb && eq va vb)
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
  List.fold_left2 (fun acc (k1, v1) (k2, v2) ->
    assert (k1 = k2);
    f k1 acc v1 v2) acc lft rgt

let filter ~f assoc =
  List.filter (fun (key, value) ->
    f key value) assoc

let keys assoc =
  List.map fst assoc

let values assoc =
  List.map snd assoc

let pluck assoc =
  match assoc with
  | (key, value) :: assoc -> (key, value), assoc
  | _ -> raise Not_found

let prepend assoc key value =
  (key, value) :: assoc

let append assoc key value =
  assoc @ [key, value]

let add assoc key value =
  ExtList.List.unique ~cmp:eq (List.merge cmp assoc [key, value])

let merge_fold ~f acc lft rgt =
  let rec merge_pair acc lft rgt rest =
    match lft, rgt with
    | (k1, v1) :: lft_rest, (k2, v2) :: rgt_rest
      when k1 = k2
    -> let acc, v = f k1 acc v1 v2 in
       merge_pair acc lft_rest rgt_rest ((k1, v) :: rest)

    | (k1, v1) :: lft_rest, (k2, v2) :: rgt_rest
      when (compare k1 k2) > 0
    -> merge_pair acc lft_rest rgt ((k1, v1) :: rest)

    | (k1, v1) :: lft_rest, (k2, v2) :: rgt_rest
    -> merge_pair acc lft rgt_rest ((k2, v2) :: rest)

    | [], rgt_rest
    -> acc, rgt_rest

    | lft_rest, []
    -> acc, lft_rest
  in
  let acc, assoc = merge_pair acc lft rgt [] in
  acc, assoc

let merge lft rgt =
  snd (merge_fold () lft rgt ~f:(fun _ _ vl vr -> (), vr))

let update assoc values =
  let rec update_pair assoc values rest =
    match assoc, values with
    | (key, _) :: assoc, value :: values
    -> update_pair assoc values ((key, value) :: rest)
    | [], []
    -> rest
    | _, _
    -> raise (Invalid_argument ("Assoc.update" :> latin1s))
  in
  update_pair assoc values []

let remove assoc key =
  List.remove_assoc key assoc
