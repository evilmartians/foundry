type 'a t

val create  : unit -> 'a t
val some    : 'a t -> bool
val put     : 'a t -> 'a -> unit
val append  : 'a t -> 'a list -> unit
val take    : 'a t -> 'a
val remove  : 'a t -> 'a -> unit
