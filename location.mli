type t
with sexp

val empty       : t

val start_file  : string -> int -> unit
val start_line  : int -> unit
val finish_file : int -> unit
val make        : int -> int -> t

val is_empty    : t -> bool
val is_present  : t -> bool

val join        : t -> t -> t
val find        : t list -> t

val at          : t -> string
