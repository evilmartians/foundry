open ExtList

type 'a t = 'a list ref

let create () =
  ref []

let some worklist =
  !worklist <> []

let put worklist elem =
  if not (List.memq elem !worklist) then
    worklist := !worklist @ [elem]

let append worklist elems =
  List.iter (put worklist) elems

let take worklist =
  match !worklist with
  | elem :: rest
  -> worklist := rest; elem
  | []
  -> raise Not_found

let remove worklist elem =
  worklist := List.remove_if ((==) elem) !worklist
