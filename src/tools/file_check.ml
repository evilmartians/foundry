(* See also: LLVM, utils/FileCheck/FileCheck.cpp. *)

open ExtString
open ExtHashtbl

exception Error   of Diagnostic.t
exception Failure

let errwith message loc =
  raise (Error (Diagnostic.Error, (Unicode.assert_utf8s message), [loc]))

let compact_whitespace buf =
  let re = Str.regexp "[ \t]+" in
  Str.global_replace re " " buf

(* Read file and register it with the location manager. *)
let read_file_or_stdin ?(canonicalize=true) filename =
  (* Select input channel. *)
  let chan, buf_name =
    if filename = "-" then
      stdin, "<stdin>"
    else
      open_in filename, filename
  in

  (* Read the entire file. *)
  let chunk = String.create 4096 in
  let rec read_some buf =
    let len = input chan chunk 0 4096 in
      if len = 0 then buf
      else read_some (buf ^ (String.sub chunk 0 len))
  in
  let buf = read_some "" in
  let buf = if canonicalize then compact_whitespace buf
            else buf
  in

  (* Register the file with the location manager. *)
  let mark = Location.register (Unicode.adopt_utf8s buf_name) 1
                (Unicode.adopt_utf8s buf) in
  mark, buf

(* === PATTERN PARSER AND EVALUATOR === *)

type patt = {
  patt_loc    : Location.t;
  patt_source : string;
  patt_uses   : (string * (* where to insert     *) int) list;
  patt_defs   : (string * (* capture group index *) int) list;
}

(* OCaml regular expressions are unusual in that they require
   parentheses and pipe symbol to be escaped th *gain* special
   meaning. swap_escape converts from (and, incidentally, to)
   the usual format. *)
let swap_escape input =
  let swap_chr (esc, str) chr =
    match esc, chr with
    (* Go to escape mode. *)
    | false, '\\'
    -> true,  str
    (* *Un*escape parens and pipe. *)
    | true,  '(' | true,  '|' | true,  ')'
    -> false, str ^ (String.of_char chr)
    (* Leave other escaped symbols as they are (escaped).
       Escape bare parens and pipe. *)
    | true,  _   | false, '(' | false, '|' | false, ')'
    -> false, str ^ "\\" ^ (String.of_char chr)
    (* Leave other unescaped symbols as they are (bare). *)
    | false, _
    -> false, str ^ (String.of_char chr)
  in
  snd (String.fold_left swap_chr (false, "") input)

let regexp name = ['A'-'Z' 'a'-'z' '_'] ['A'-'Z' 'a'-'z' '0'-'9' '_']*

let patt_parse loc patt =
  if patt = "" then
    errwith "empty pattern" loc;

  let uses  = ref []
  and defs  = ref []
  and group = ref 1
  in

  let lexeme lexbuf =
    Ulexing.utf8_lexeme lexbuf
  and lexeme_prefix lexbuf len =
    Ulexing.utf8_sub_lexeme lexbuf 0
        ((Ulexing.lexeme_length lexbuf) - len)
  and lexeme_loc lexbuf =
    Location.sub loc
        (Ulexing.lexeme_start lexbuf)
        (Ulexing.lexeme_end lexbuf)
  in
  let rec lex_patt re = lexer
  | [^"{["]+ | ["{["]
  -> lex_patt (re ^ (Str.quote (lexeme lexbuf))) lexbuf
  | "{{"
  -> lex_patt_interp (re ^ "\\(") (lexeme_loc lexbuf) lexbuf
  | "[["
  -> lex_patt_var re (lexeme_loc lexbuf) lexbuf
  | eof
  -> re
  and lex_patt_interp re loc = lexer
  | [^'}']+ | '}'
  -> lex_patt_interp (re ^ (swap_escape (lexeme lexbuf))) loc lexbuf
  | "}}"
  -> (incr group;
      lex_patt (re ^ "\\)") lexbuf)
  | eof
  -> errwith "unterminated regexp interpolation" loc
  and lex_patt_var re loc = lexer
  (* Backreference: [[foo]] *)
  | name "]]"
  -> (let name = lexeme_prefix lexbuf 2 in
      try
        let backref = List.assoc name !defs in
        if backref > 9 then
          errwith "out of backreferences: too many capture groups"
                   (Location.join loc (lexeme_loc lexbuf))
        else
          lex_patt (re ^ "\\" ^ (string_of_int backref)) lexbuf
      with Not_found ->
        uses := (name, String.length re) :: !uses;
        lex_patt re lexbuf)
  (* Variable capture: [[foo:.*]] *)
  | name ':'
  -> (defs := ((lexeme_prefix lexbuf 1),
               !group) :: !defs;
      incr group;
      lex_patt_capture (re ^ "\\(") loc lexbuf)
  (* Syntax error *)
  | name eof -> errwith "unterminated backreference" loc
  | _        -> errwith "invalid variable name in capture group" (lexeme_loc lexbuf)
  and lex_patt_capture re loc = lexer
  | [^']']+ | ']'
  -> lex_patt_capture (re ^ (swap_escape (lexeme lexbuf))) loc lexbuf
  | "]]"
  -> lex_patt (re ^ "\\)") lexbuf
  | eof
  -> errwith "unterminated capture group" loc
  in

  let lexbuf = Ulexing.from_utf8_string patt in
  let regexp = lex_patt "" lexbuf in
  { patt_loc    = loc;
    patt_source = regexp;
    patt_uses   = !uses;
    patt_defs   = !defs; }

let patt_prepare patt env =
  (* Variable uses are present in the pattern in reverse lexical
     order. *)
  let prepared =
    List.fold_left (fun str (var, pos) ->
        (String.slice ~last:pos str) ^
          (Str.quote (Hashtbl.find env var)) ^
          (String.slice ~first:pos str))
      patt.patt_source
      patt.patt_uses
  in Str.regexp prepared

let patt_match patt env input start =
  let re  = patt_prepare patt env in
  let pos = Str.search_forward re input start in
    List.iter (fun (var, index) ->
        Hashtbl.replace env var (Str.matched_group index input))
      patt.patt_defs;
    pos, (Str.match_end ()) - pos

let patt_failure_info patt env loc =
  List.iter (fun (var, _) ->
      let message =
        match Hashtbl.find_option env var with
        | Some value -> "with variable \"" ^ var ^ "\" equal to \"" ^ value ^ "\""
        | None -> "uses undefined variable \"" ^ var ^ "\""
      in Diagnostic.print (Diagnostic.Note, Unicode.assert_utf8s message, [loc]))
    patt.patt_uses

(* === CHECK STRING PARSER AND EVALUATOR === *)

type check = {
  chk_loc     : Location.t;
  chk_next    : bool;
  chk_patt    : patt option;
  chk_dag_not : check_match list;
}
and check_match =
| MatchNot of patt
| MatchDAG of patt

let check_parse ?(canonicalize=true) filename prefix =
  let mark, buf = read_file_or_stdin ~canonicalize filename in
  let dag_not   = ref [] in

  let rec parse buf offset checks =
    try
      let chk_idx = String.find buf prefix in
      let check_suffix suffix =
        let first = chk_idx + (String.length prefix) in
        let last  = first + (String.length suffix) in
        let str   = String.slice ~first ~last buf in
          if str = suffix then Some last else None
      in
      let check_str =
        List.fold_left (fun check_str (kind, suffix) ->
            match check_str with
            | Some _ -> check_str
            | None   -> Option.map (fun idx -> kind, idx) (check_suffix suffix))
          None
          [ `Check,    ":";     `CheckNext, "-NEXT:";
            `CheckNot, "-NOT:"; `CheckDAG,  "-DAG:"   ]
      in
      match check_str with
      | Some (kind, patt_idx)
      -> ((* Find end of the current line. *)
          let eol_idx =
            try  String.index_from buf chk_idx '\n'
            with Not_found -> String.length buf
          in
          (* Trim pattern and map its location back. *)
          let patt     = String.strip (String.slice buf ~first:patt_idx ~last:eol_idx) in
          let patt_idx = String.find buf patt
          and patt_end = patt_idx + (String.length patt) + 1
          in
          (* Create Location.t for check string and pattern. *)
          let chk_loc  = Location.make mark (offset + chk_idx)  (offset + eol_idx)
          and patt_loc = Location.make mark (offset + patt_idx) (offset + patt_end)
          in
          (* Parse the pattern. *)
          let patt = patt_parse patt_loc patt in

          (* Handle parse results and continue. *)
          let buf  = String.slice ~first:eol_idx buf in
          let next = parse buf (offset + eol_idx) in
          match kind, checks with
          (* Verify that CHECK-NEXT is preceded by at least one CHECK. *)
          | `CheckNext, []
          -> errwith ("found '" ^ prefix ^ "-NEXT:' without previous '" ^
                      prefix ^ ":' line") chk_loc
          (* Push CHECK-NOT and CHECK-DAG. *)
          | `CheckNot, _ -> dag_not := (MatchNot patt) :: !dag_not; next checks
          | `CheckDAG, _ -> dag_not := (MatchDAG patt) :: !dag_not; next checks
          (* Handle CHECK. *)
          | `Check, _ | `CheckNext, _
          -> (let chk_dag_not = !dag_not in
              dag_not := [];
              next ({
                chk_loc;
                chk_next = (kind = `CheckNext);
                chk_patt = Some patt;
                chk_dag_not;
              } :: checks)))
      | None -> checks
    with Invalid_string ->
      if !dag_not = [] then
        checks
      else {
        chk_loc     = Location.empty;
        chk_next    = false;
        chk_patt    = None;
        chk_dag_not = [];
      } :: checks
  in
  parse buf 0 []

(* Skip whitespace and point at first significant character. *)
let sensible_loc_after (file, buf) start =
  let re  = Str.regexp "[^ \t\r\n]" in
  let pos = try  Str.search_forward re buf start
            with Not_found -> start
  in Location.make file pos (pos + 1)

let check_failure env patt input_loc =
  Diagnostic.print (Diagnostic.Error,
      u"expected string not found in input",
      [patt.patt_loc]);
  Diagnostic.print (Diagnostic.Note,
      u"scanning from here:",
      [input_loc]);
  patt_failure_info patt env input_loc

let check_not patts env (file, buf) start finish =
  List.iter (fun patt ->
      try
        let pos, len = patt_match patt env buf start in
        if pos <= finish then begin
          Diagnostic.print (Diagnostic.Error,
              u"CHECK-NOT: string occured",
              [Location.make file pos (pos + len)]);
          Diagnostic.print (Diagnostic.Note,
              u"pattern specified here:",
              [patt.patt_loc]);
          raise Failure
        end
      with Not_found ->
        ())
    patts

let check_next chk env (file, buf) start finish =
  let failure_info () =
    Diagnostic.print (Diagnostic.Note,
        u"'next' match was there:",
        [sensible_loc_after (file, buf) finish]);
    Diagnostic.print (Diagnostic.Note,
        u"previous match ended here:",
        [sensible_loc_after (file, buf) start])
  in
  if chk.chk_next then
    let first_eol = try  String.index_from buf start '\n'
                    with Not_found -> finish + 1
    in
    if first_eol > finish then begin
      Diagnostic.print (Diagnostic.Error,
          u"CHECK-NEXT: on the same line as previous match",
          [chk.chk_loc]);
      failure_info ();
    end else
      let second_eol = try  String.index_from buf (first_eol + 1) '\n'
                       with Not_found -> finish + 1
      in
      if second_eol <= finish then begin
        Diagnostic.print (Diagnostic.Error,
            u"CHECK-NEXT: is not on the line after previous match",
            [chk.chk_loc]);
        failure_info ();
      end

let check_dag chk env (file, buf) start =
  if chk.chk_dag_not = [] then
    start, []
  else
    let nots     = ref []
    and last_pos = ref start
    and last_len = ref 0
    and start    = ref start
    in
    List.iter (fun dag_not ->
        match dag_not with
        | MatchNot patt
        -> nots := patt :: !nots
        | MatchDAG patt
        -> (try
              (* CHECK-DAG always matches from the start. *)
              let pos, len = patt_match patt env buf !start in
              if !nots <> [] then begin
                if pos < !last_pos + !last_len then begin
                  (* Reordered? *)
                  Diagnostic.print (Diagnostic.Error,
                      u"found a match of CHECK-DAG reordering across a CHECK-NOT",
                      [Location.make file pos (pos + len)]);
                  Diagnostic.print (Diagnostic.Note,
                      u"the farthest match of CHECK-DAG is found here:",
                      [Location.make file !last_pos (!last_pos + !last_len)]);
                  Diagnostic.print (Diagnostic.Note,
                      u"the crossed pattern is specified here:",
                      [(List.hd !nots).patt_loc]);
                  Diagnostic.print (Diagnostic.Note,
                      u"the reordered pattern is specified here:",
                      [patt.patt_loc]);
                  raise Failure
                end;

                (* If there's CHECK-NOTs between two CHECK-DAGs or from CHECK to
                   CHECK-DAG, verify that there's no 'not' strings occurred in that
                   region. *)
                check_not !nots env (file, buf) pos (!last_pos + !last_len);
                nots := [];

                (* All subsequent CHECK-DAGs should be matched from the farthest
                   position of all precedent CHECK-DAGs (including this one). *)
                start := !last_pos + !last_len
              end;

              if pos + len > !last_pos + !last_len then begin
                last_pos := pos;
                last_len := len
              end
            with Not_found ->
              (* With a group of CHECK-DAGs, a single mismatching means the match on
                 that group of CHECK-DAGs fails immediately. *)
              check_failure env patt (sensible_loc_after (file, buf) !start);
              raise Failure))
      chk.chk_dag_not;

    !last_pos + !last_len, !nots

let check_match chk env (file, buf) start =
  (* Match "dag strings" (with mixed "not strings" if any). *)
  let last, nots = check_dag chk env (file, buf) start in

  (* Match itself from the last position after matching CHECK-DAG. *)
  let pos, len =
    match chk.chk_patt with
    | Some patt
    -> (try
          let pos, len = patt_match patt env buf last in
          pos, len
        with Not_found ->
          check_failure env patt (sensible_loc_after (file, buf) last);
          raise Failure)
    | None (* EOF pattern *)
    -> last, (String.length buf) - last
  in

  (* If this check is a "CHECK-NEXT", verify that the previous match was on
     the previous line (i.e. that there is one newline between them). *)
  check_next chk env (file, buf) last pos;

  (* If this match had "not strings", verify that they don't exist in the
     skipped region. *)
  check_not nots env (file, buf) last pos;

  pos, len

(* === COMMAND LINE PARSER === *)

let _ =
  let check_file   = ref ""
  and input_file   = ref ""
  and prefix       = ref "CHECK"
  and canonicalize = ref true
  in

  Arg.parse (Arg.align [
      "-input-file", Arg.String (fun arg ->
          if !input_file <> "" then
            (prerr_endline "More than one input file is provided."; exit 2);
          input_file := arg),
        "FILE File to check (defaults to stdin)";

      "-check-prefix", Arg.Set_string prefix,
        "PREFIX Prefix to use from check file (defaults to 'CHECK')";

      "-strict-whitespace", Arg.Clear canonicalize,
        " Do not treat all horizontal whitespace as equivalent"
    ])
    (fun arg ->
      if !check_file <> "" then
        (prerr_endline "More than one check file is provided."; exit 2);
      check_file := arg)
    ("Usage: " ^ Sys.argv.(0) ^ " [options] <check-file>\n");

  if !input_file = "" then
    input_file := "-";

  if !check_file = "" then
    (prerr_endline "No check file specified."; exit 2);

  try
    let canonicalize = !canonicalize in

    match check_parse ~canonicalize !check_file !prefix with
    | []
    -> (prerr_endline ("No check strings found with prefix '" ^ !prefix ^ ":'");
        exit 2);
    | checks
    -> ((* Initialize empty capture environment. *)
        let env       = Hashtbl.create 10 in
        (* Read input file. *)
        let mark, buf = read_file_or_stdin ~canonicalize !input_file in

        (* Run checks. *)
        List.fold_left (fun start chk ->
            let pos, len = check_match chk env (mark, buf) start in
            pos + len)
          0 (List.rev checks))
  with
  | Error diag ->
    Diagnostic.print diag;
    exit 2
  | Sys_error msg ->
    prerr_endline msg;
    exit 2
  | Failure ->
    exit 1
