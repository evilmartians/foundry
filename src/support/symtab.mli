open Unicode.Std

type t

val create : unit -> t
val copy   : t -> t
val add    : t -> string -> string
val update : t -> string -> string -> string
val remove : t -> string -> unit
