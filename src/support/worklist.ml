open ExtList

type 'a t = 'a list ref

let create () =
  ref []

let some worklist =
  !worklist <> []

let put worklist elem =
  worklist := elem :: !worklist

let append worklist elems =
  worklist := elems @ !worklist

let take worklist =
  match !worklist with
  | elem :: rest
  -> worklist := rest; elem
  | []
  -> raise Not_found

let remove worklist elem =
  worklist := List.remove_if ((==) elem) !worklist
