open Unicode.Std

type t = private int * int
with sexp

type file

val empty       : t

val reset       : unit -> unit
val register    : (*filename*) string -> (*line*) int ->
                    (*content*) string -> file

val make        : file -> (*begin*) int -> (*end*) int -> t
val sub         : t -> (*begin*) int -> (*end*) int -> t

val is_present  : t -> bool

val join        : t -> t -> t
val find        : t list -> t

val unpack      : t -> string * int * int
val decompose   : t -> string * (int * int) * (int * int)
val file_line   : t -> string * int

val line_source : t -> string

val at          : t -> string
