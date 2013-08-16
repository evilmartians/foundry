open Unicode.Std

type severity =
| Fatal
| Error
| Warning
| Note

type t =
  severity * string * Location.t list

let color_of_severity severity =
  match severity with
  | Fatal | Error -> "\x1b[1;31m"
  | Warning       -> "\x1b[1;35m"
  | Note          -> "\x1b[1;30m"

let string_of_severity severity =
  match severity with
  | Fatal   -> "fatal"
  | Error   -> "error"
  | Warning -> "warning"
  | Note    -> "note"

let print diag =
  (* Don't print escape sequences to non-terminals. *)
  let isatty  = Unix.isatty (Unix.stdin) in
  let color x = if isatty then x else "" in

  let severity, message, locations = diag in
  let loc = (List.hd locations) in
  let file, (line_beg, col_beg), (line_end, col_end) = Location.decompose loc in
    prerr_endline (
        (color "\x1b[1m") ^ file ^ ":" ^
        (string_of_int line_beg) ^ ":" ^
        (string_of_int (col_beg + 1)) ^ ": " ^
        (color (color_of_severity severity)) ^
        (string_of_severity severity) ^ ":" ^ (color "\x1b[1;37m") ^ " " ^
        message ^ (color "\x1b[0m"));
    prerr_endline (Location.line_source loc);
    if line_beg = line_end then
      prerr_endline (
        (String.make col_beg ' ') ^ (color "\x1b[1;32m") ^
        (String.make (col_end - col_beg) (Char.of_string "^")) ^ (color "\x1b[0m"))
