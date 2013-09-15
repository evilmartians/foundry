open Sexplib.Std
open Unicode.Std
open ExtList

type t = int * int
with sexp

type file = int

let empty = (0, 0)

let files  = ref []
let lines  = ref []
let cursor = ref 0
let data   = ref ""

let reset () =
  files  := [];
  lines  := [];
  cursor := 0;
  data   := ""

let register file line content =
  files  := (file, !cursor) :: !files;
  lines  := (line, !cursor) :: !lines;

  (* Locate and memorize newlines. *)
  let rec start_line line index =
    try
      let index = (String.index_from content index '\n') + 1 in
      lines := (line, !cursor + index) :: !lines;
      start_line (line + 1) index
    with Not_found -> ()
  in
  start_line (line + 1) 0;

  (* Memorize start of the newly added file. *)
  let mark = !cursor in

  (* Extend internal buffer. *)
  cursor := !cursor + (String.length content);
  data   := !data ^ content;

  (* Return mark for this file. *)
  mark

let make file lft rgt =
  file + lft, file + rgt

let sub loc lft rgt =
  let pos, _ = loc in
    pos + lft, pos + rgt

let next_pos target (_, pos) =
  pos <= target

let find_line pos =
  let line, start = List.find (next_pos pos) !lines in
  line, pos - start

let decompose loc =
  let file, _ = List.find (next_pos (fst loc)) !files in
  file, find_line (fst loc), find_line (snd loc)

let file_line loc =
  let file, _ = List.find (next_pos (fst loc)) !files in
  file, fst (find_line (fst loc))

let unpack loc =
  let file, p = List.find (next_pos (fst loc)) !files in
  file, (fst loc) - p, (snd loc) - p

let line_source loc =
  let _, line_start = List.find (next_pos (fst loc)) !lines in
  let line_finish   = try  String.index_from !data line_start '\n'
                      with Not_found -> String.length !data in
    String.sub !data line_start (line_finish - line_start)

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
