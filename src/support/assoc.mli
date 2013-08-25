open Unicode.Std

type ('a, 'b) t     = private (string * 'a) list
and  sequental
and  sorted
and  'a sequental_t = ('a, sequental) t
and  'a sorted_t    = ('a, sorted) t
with sexp_of

val empty       : ('a, 'b) t

val sequental   : (string * 'a) list -> ('a, sequental) t
val sorted      : (string * 'a) list -> ('a, sorted) t
val sort        : ('a, sequental) t  -> ('a, sorted) t

val is_empty    : ('a, 'b) t -> bool
val find        : ('a, 'b) t -> string -> 'a
val find_option : ('a, 'b) t -> string -> 'a option
val index       : ('a, 'b) t -> string -> int
val mem         : ('a, 'b) t -> string -> bool

val keys        : ('a, 'b) t -> string list

val equal       : eq:('a -> 'a -> bool) -> ('a, 'b) t -> ('a, 'c) t -> bool
val map         : f:(string -> 'a -> 'c) -> ('a, 'b) t -> ('c, 'b) t
val map_list    : f:(string -> 'a -> 'c) -> ('a, 'b) t -> 'c list
val fold        : f:(string -> 'c -> 'a -> 'c) -> 'c -> ('a, 'b) t -> 'c
val fold2       : f:(string -> 'b -> 'a -> 'a -> 'b) -> 'b -> ('a, 'c) t -> ('a, 'd) t -> 'b
val filter      : f:(string -> 'a -> bool) -> ('a, 'b) t -> ('a, 'b) t

val prepend     : ('a, 'b) t -> string -> 'a -> ('a, sequental) t
val append      : ('a, 'b) t -> string -> 'a -> ('a, sequental) t
val add         : ('a, sorted) t -> string -> 'a -> ('a, sorted) t
val merge       : ('a, sorted) t -> ('a, sorted) t -> ('a, sorted) t
val remove      : ('a, 'b) t -> string -> ('a, 'b) t
