open Unicode.Std

module Make :
  functor(V : Hashtbl.HashedType) ->
  sig
    type t

    val create  : unit -> t
    val bind    : t -> V.t -> string -> string
    val lookup  : t -> V.t -> string option
  end
