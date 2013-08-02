open Unicode.Std

type t = private int * int
with sexp

val empty       : t

val reset       : unit -> unit
val start_file  : string -> int -> unit
val start_line  : int -> unit
val finish_file : int -> unit

val make        : int -> int -> t
val unpack      : t -> string * int * int
val decompose   : t -> string * (int * int) * (int * int)

val is_empty    : t -> bool
val is_present  : t -> bool

val join        : t -> t -> t
val find        : t list -> t

val at          : t -> string
