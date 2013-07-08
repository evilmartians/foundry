open Unicode.Std

exception Unexpected of string * Location.t

(* Token helpers *)

let lexeme lexbuf =
  (Unicode.adopt_utf8s (Ulexing.utf8_lexeme lexbuf))

let sub_lexeme lexbuf from to_ =
  let len = if to_ < 0 then (Ulexing.lexeme_length lexbuf) + to_
                       else to_ - from
  in (Unicode.adopt_utf8s (Ulexing.utf8_sub_lexeme lexbuf from len))

let locate lexbuf =
  Location.make (Ulexing.lexeme_start lexbuf) (Ulexing.lexeme_end lexbuf)

let newline lexbuf =
  Location.start_line (Ulexing.lexeme_end lexbuf)

let eof lexbuf =
  Location.finish_file (Ulexing.lexeme_start lexbuf);
  Parser.EOF

let unexpected lexbuf =
  let error = Unexpected (lexeme lexbuf, locate lexbuf) in
  let _ = eof lexbuf in
  raise error

(* State management *)

type state = {
  mutable lexer_stack : (state -> Ulexing.lexbuf -> Parser.token) list;
  mutable curly_stack : int list
}

let goto state next_lexer =
  state.lexer_stack <- next_lexer :: List.tl state.lexer_stack

let push state next_lexer =
  state.lexer_stack <- next_lexer :: state.lexer_stack;
  state.curly_stack <- 0 :: state.curly_stack

let pop state =
  assert ((List.hd state.curly_stack) = 0);
  state.lexer_stack <- List.tl state.lexer_stack;
  state.curly_stack <- List.tl state.curly_stack

let curly state change =
  let (curlies, rest) = (List.hd state.curly_stack,
                         List.tl state.curly_stack) in
    let new_curlies = curlies + change in
      assert (new_curlies >= 0);
      state.curly_stack <- new_curlies :: rest;
      new_curlies

let regexp w_space     = [' ' '\t']+
let regexp w_newline   = '\n' | "\r\n"
let regexp w_any       = w_space | w_newline

let regexp digits      = ['0'-'9']+
let regexp id_lower    = ['a'-'z' '_']
let regexp id_upper    = ['A'-'Z']
let regexp id_alpha    = id_lower | id_upper
let regexp id_alnum    = id_alpha | digits

let regexp ident       = id_alpha id_alnum*
let regexp local       = id_lower id_alnum*
let regexp const       = id_upper id_alnum*

let regexp operator    = ['+' '-' '*' '/' '%' '&' '|' '~'] |
                  "<<" | ">>" | ">>>"
let regexp method_name = local | operator | '<' | '>' | "<=" | ">=" | "==" | "<=>"
let regexp symbol      = ':' method_name

let rec lex_code state = lexer
| w_space     -> lex_code state lexbuf
| w_newline   -> newline lexbuf;
                 lex_code state lexbuf

(* Punctuation *)
| operator '=' -> Parser.Tk_OP_ASGN  (locate lexbuf, sub_lexeme lexbuf 0 (-2))
| "and="       -> Parser.Tk_AND_ASGN (locate lexbuf)
| "or="        -> Parser.Tk_OR_ASGN  (locate lexbuf)

| '{'      -> ignore (curly state 1);
              Parser.Tk_LCURLY   (locate lexbuf)
| '}'      -> if (curly state (-1)) = 0 then begin
                pop state;
                Parser.Vl_QUOTE  (locate lexbuf)
              end else
                Parser.Tk_RCURLY (locate lexbuf)

| '('      -> Parser.Tk_LPAREN  (locate lexbuf)
| ')'      -> Parser.Tk_RPAREN  (locate lexbuf)
| '['      -> Parser.Tk_LBRACK  (locate lexbuf)
| ']'      -> Parser.Tk_RBRACK  (locate lexbuf)
| '='      -> Parser.Tk_ASGN    (locate lexbuf)
| '.'      -> Parser.Tk_DOT     (locate lexbuf)
| ':'      -> Parser.Tk_COLON   (locate lexbuf)
| "::"     -> Parser.Tk_DCOLON  (locate lexbuf)
| ';'      -> Parser.Tk_SEMI    (locate lexbuf)
| ";;"     -> Parser.Tk_DSEMI   (locate lexbuf)
| ','      -> Parser.Tk_COMMA   (locate lexbuf)
| "->"     -> Parser.Tk_ARROW   (locate lexbuf)
| "=>"     -> Parser.Tk_ROCKET  (locate lexbuf)

(* Operators *)
| "+@"     -> Parser.Tk_UPLUS   (locate lexbuf, lexeme lexbuf)
| "-@"     -> Parser.Tk_UMINUS  (locate lexbuf, lexeme lexbuf)
| "~@"     -> Parser.Tk_UTILDE  (locate lexbuf, lexeme lexbuf)
| '+'      -> Parser.Tk_PLUS    (locate lexbuf, lexeme lexbuf)
| '-'      -> Parser.Tk_MINUS   (locate lexbuf, lexeme lexbuf)
| '*'      -> Parser.Tk_STAR    (locate lexbuf, lexeme lexbuf)
| "**"     -> Parser.Tk_DSTAR   (locate lexbuf, lexeme lexbuf)
| '/'      -> Parser.Tk_DIVIDE  (locate lexbuf, lexeme lexbuf)
| '%'      -> Parser.Tk_PERCENT (locate lexbuf, lexeme lexbuf)
| '&'      -> Parser.Tk_AMPER   (locate lexbuf, lexeme lexbuf)
| '|'      -> Parser.Tk_PIPE    (locate lexbuf, lexeme lexbuf)
| '~'      -> Parser.Tk_TILDE   (locate lexbuf, lexeme lexbuf)
| "<<"     -> Parser.Tk_LSHFT   (locate lexbuf, lexeme lexbuf)
| ">>"     -> Parser.Tk_RSHFT   (locate lexbuf, lexeme lexbuf)
| ">>>"    -> Parser.Tk_TILDE   (locate lexbuf, lexeme lexbuf)
| "=="     -> Parser.Tk_EQ      (locate lexbuf, lexeme lexbuf)
| "<="     -> Parser.Tk_LEQ     (locate lexbuf, lexeme lexbuf)
| '<'      -> Parser.Tk_LT      (locate lexbuf, lexeme lexbuf)
| '>'      -> Parser.Tk_GT      (locate lexbuf, lexeme lexbuf)
| ">="     -> Parser.Tk_GEQ     (locate lexbuf, lexeme lexbuf)
| "<=>"    -> Parser.Tk_CMP     (locate lexbuf, lexeme lexbuf)

(* Keywords *)
| "true"   -> Parser.Kw_TRUE    (locate lexbuf, lexeme lexbuf)
| "false"  -> Parser.Kw_FALSE   (locate lexbuf, lexeme lexbuf)
| "nil"    -> Parser.Kw_NIL     (locate lexbuf, lexeme lexbuf)
| "self"   -> Parser.Kw_SELF    (locate lexbuf, lexeme lexbuf)
| "and"    -> Parser.Kw_AND     (locate lexbuf, lexeme lexbuf)
| "or"     -> Parser.Kw_OR      (locate lexbuf, lexeme lexbuf)
| "not"    -> Parser.Kw_NOT     (locate lexbuf, lexeme lexbuf)
| "let"    -> Parser.Kw_LET     (locate lexbuf, lexeme lexbuf)
| "mut"    -> Parser.Kw_MUT     (locate lexbuf, lexeme lexbuf)
| "while"  -> Parser.Kw_WHILE   (locate lexbuf, lexeme lexbuf)
| "do"     -> Parser.Kw_DO      (locate lexbuf, lexeme lexbuf)
| "if"     -> Parser.Kw_IF      (locate lexbuf, lexeme lexbuf)
| "elsif"  -> Parser.Kw_ELSIF   (locate lexbuf, lexeme lexbuf)
| "then"   -> Parser.Kw_THEN    (locate lexbuf, lexeme lexbuf)
| "else"   -> Parser.Kw_ELSE    (locate lexbuf, lexeme lexbuf)
| "match"  -> Parser.Kw_MATCH   (locate lexbuf, lexeme lexbuf)
| "end"    -> Parser.Kw_END     (locate lexbuf, lexeme lexbuf)
| "as"     -> Parser.Kw_AS      (locate lexbuf, lexeme lexbuf)
| "meta"   -> Parser.Kw_META    (locate lexbuf, lexeme lexbuf)
| "type"   -> Parser.Kw_TYPE    (locate lexbuf, lexeme lexbuf)
| "public" -> Parser.Kw_PUBLIC  (locate lexbuf, lexeme lexbuf)
| "dynamic"-> Parser.Kw_DYNAMIC (locate lexbuf, lexeme lexbuf)
| "package"-> Parser.Kw_PACKAGE (locate lexbuf, lexeme lexbuf)
| "class"  -> Parser.Kw_CLASS   (locate lexbuf, lexeme lexbuf)
| "mixin"  -> Parser.Kw_MIXIN   (locate lexbuf, lexeme lexbuf)
| "iface"  -> Parser.Kw_IFACE   (locate lexbuf, lexeme lexbuf)
| "def"    -> Parser.Kw_DEF     (locate lexbuf, lexeme lexbuf)
| "return" -> Parser.Kw_RETURN  (locate lexbuf, lexeme lexbuf)

(* Values *)
| digits          -> Parser.Vl_INT    (locate lexbuf, int_of_string (lexeme lexbuf))
| digits id_alpha -> failwith "trailing junk in a number"
| ':' method_name -> Parser.Vl_SYMBOL (locate lexbuf, sub_lexeme lexbuf 1 (-1))
| '\''            -> goto state lex_string;
                     Parser.Vl_BEGIN  (locate lexbuf, Syntax.Qu_STRING)
| '"'             -> goto state lex_string_interp;
                     Parser.Vl_BEGIN  (locate lexbuf, Syntax.Qu_STRING)
| ":'"            -> goto state lex_string;
                     Parser.Vl_BEGIN  (locate lexbuf, Syntax.Qu_SYMBOL)
| ":\""           -> goto state lex_string_interp;
                     Parser.Vl_BEGIN  (locate lexbuf, Syntax.Qu_SYMBOL)

(* Identifiers *)
| ident ':'       -> Parser.Id_LABEL (locate lexbuf, sub_lexeme lexbuf 0 (-2))
| local           -> Parser.Id_LOCAL (locate lexbuf, lexeme lexbuf)
| const           -> Parser.Id_CONST (locate lexbuf, lexeme lexbuf)
| '@'  ident      -> Parser.Id_IVAR  (locate lexbuf, sub_lexeme lexbuf 1 (-1))
| '\\' ident      -> Parser.Id_TVAR  (locate lexbuf, sub_lexeme lexbuf 1 (-1))

| _               -> unexpected lexbuf
| eof             -> eof lexbuf

and lex_string state = lexer
| '\''            -> goto state lex_code;
                     Parser.Vl_END    (locate lexbuf)
| "\\\\"          -> Parser.Vl_STRING (locate lexbuf, "\\")
| "\\'"           -> Parser.Vl_STRING (locate lexbuf, "'")
| [^'\'' '\\']+   -> Parser.Vl_STRING (locate lexbuf, lexeme lexbuf)

| eof             -> eof lexbuf

and lex_string_interp state = lexer
| '"'               -> goto state lex_code;
                       Parser.Vl_END (locate lexbuf)
| "\\\\"            -> Parser.Vl_STRING  (locate lexbuf, "\\")
| "\\\""            -> Parser.Vl_STRING  (locate lexbuf, "\"")
| "\\" _            -> Parser.Vl_STRING  (locate lexbuf, sub_lexeme lexbuf 1 (-1))
| "#{"              -> push state lex_code; ignore (curly state 1);
                       Parser.Vl_UNQUOTE (locate lexbuf)
| '#'               -> Parser.Vl_STRING  (locate lexbuf, "#")
| [^'"' '#' '\\']+  -> Parser.Vl_STRING  (locate lexbuf, lexeme lexbuf)

| eof               -> eof lexbuf

(* Public API *)
let create file line =
  Location.start_file file line;
  { lexer_stack = [lex_code];
    curly_stack = [1] }

let next state lexbuf =
  (List.hd state.lexer_stack) state lexbuf, Lexing.dummy_pos, Lexing.dummy_pos
