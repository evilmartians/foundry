open Unicode.Std

type 'a t
with sexp_of

val create   : (string * 'a) list -> 'a t
val pair     : string -> 'a -> 'a t
val fill     : 'a t -> (string * 'a) list -> unit
val replace  : 'a t -> 'a t -> unit
val copy     : 'a t -> 'a t

val get      : 'a t -> string -> 'a option
val get_exn  : 'a t -> string -> 'a
val set      : 'a t -> string -> 'a -> unit
val exists   : 'a t -> string -> bool
val is_empty : 'a t -> bool

val iter     : ?ordered:bool -> f:(string -> 'a -> unit) -> 'a t -> unit
val map_list : ?ordered:bool -> f:(string -> 'a -> 'b) -> 'a t -> 'b list
val map      : f:('a -> 'b) -> 'a t -> 'b t
val map2     : f:('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
val fold     : f:(string -> 'b -> 'a -> 'b) -> 'b -> 'a t -> 'b
val fold2    : f:(string -> 'c -> 'a -> 'b -> 'c) -> 'c -> 'a t -> 'b t -> 'c
val filter   : f:(string -> 'a -> bool) -> 'a t -> 'a t
val join     : 'a t -> 'a t -> 'a t

val keys     : 'a t -> string list
