open Sexplib.Std

type t = int * int
with sexp

let empty = (0, 0)

let cur_pos  = ref 0
let cur_line = ref 0
let files    = ref []
let lines    = ref []

let start_file file line =
  files    := (file, !cur_pos) :: !files;
  lines    := (line, !cur_pos) :: !lines;
  cur_line := line

let start_line pos =
  cur_line := !cur_line + 1;
  lines    := (!cur_line, !cur_pos + pos) :: !lines

let finish_file pos =
  cur_pos  := !cur_pos + pos

let make lft rgt =
  (!cur_pos + lft, !cur_pos + rgt)

let decompose loc =
  let next pos (_, p) = p <= pos in
  let file, _ = List.find (next (fst loc)) !files in
  let find_line pos =
    let line, start = List.find (next pos) !lines in
      line, pos - start
  in file, find_line (fst loc), find_line (snd loc)

let is_empty loc =
  loc = empty

let is_present loc =
  loc <> empty

let at loc =
  let file, (line1, col1), (line2, col2) = decompose loc in
    "at " ^ file ^ ":" ^ (string_of_int line1) ^ ":" ^ (string_of_int col1)

let join fst_loc snd_loc =
  let (f1, f2), (s1, s2) = fst_loc, snd_loc in
    (min f1 s1, max f2 s2)

let find lst =
  List.find is_present lst
