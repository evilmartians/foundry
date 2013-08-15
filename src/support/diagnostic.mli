open Unicode.Std

type severity =
| Fatal
| Error
| Warning
| Note

type t =
  severity * string * Location.t list

val string_of_severity : severity -> string

val print              : t -> unit
