open Unicode.Std

module Make :
  functor(V : Hashtbl.HashedType) ->
  sig
    type t

    val create  : unit -> t
    val bind    : t -> V.t -> string -> string
    val get_exn : t -> V.t -> string
    val get     : t -> V.t -> string option
  end
