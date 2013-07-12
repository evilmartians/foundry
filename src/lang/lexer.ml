open Unicode.Std
open Parser_tokens

exception Unexpected of Location.t * char

(* Token helpers *)

let lexeme lexbuf =
  (Unicode.adopt_utf8s (Ulexing.utf8_lexeme lexbuf))

let sub_lexeme lexbuf from to_ =
  let from = if from < 0 then (Ulexing.lexeme_length lexbuf) + from
                         else from
  in
  let len  = if to_ <= 0 then (Ulexing.lexeme_length lexbuf) + to_
                         else to_ - from
  in (Unicode.adopt_utf8s (Ulexing.utf8_sub_lexeme lexbuf from len))

let locate lexbuf =
  Location.make (Ulexing.lexeme_start lexbuf) (Ulexing.lexeme_end lexbuf)

let newline lexbuf =
  Location.start_line (Ulexing.lexeme_end lexbuf)

let eof lexbuf =
  let location =
    Location.make (Ulexing.lexeme_start lexbuf - 1) (Ulexing.lexeme_end lexbuf)
  in
  let eof = EOF (location) in
    Location.finish_file (Ulexing.lexeme_start lexbuf);
    eof

let unexpected lexbuf =
  let error = Unexpected (locate lexbuf, (lexeme lexbuf).[0]) in
    Location.finish_file (Ulexing.lexeme_start lexbuf);
    raise error

(* State management *)

type state = {
  mutable lexer_stack : (state -> Ulexing.lexbuf -> token) list;
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

let rec lex_code state = lexer
| w_space     -> lex_code state lexbuf
| w_newline   -> newline lexbuf;
                 lex_code state lexbuf

(* Punctuation *)
| operator '=' -> Tk_OP_ASGN  (locate lexbuf, sub_lexeme lexbuf 0 (-1))
| "and="       -> Tk_AND_ASGN (locate lexbuf)
| "or="        -> Tk_OR_ASGN  (locate lexbuf)

| '{'      -> ignore (curly state 1);
              Tk_LCURLY   (locate lexbuf)
| '}'      -> if (curly state (-1)) = 0 then begin
                pop state;
                Vl_QUOTE  (locate lexbuf)
              end else
                Tk_RCURLY (locate lexbuf)

| '('      -> Tk_LPAREN  (locate lexbuf)
| ')'      -> Tk_RPAREN  (locate lexbuf)
| '['      -> Tk_LBRACK  (locate lexbuf)
| ']'      -> Tk_RBRACK  (locate lexbuf)
| '='      -> Tk_ASGN    (locate lexbuf)
| '.'      -> Tk_DOT     (locate lexbuf)
| ':'      -> Tk_COLON   (locate lexbuf)
| "::"     -> Tk_DCOLON  (locate lexbuf)
| ';'      -> Tk_SEMI    (locate lexbuf)
| ";;"     -> Tk_DSEMI   (locate lexbuf)
| ','      -> Tk_COMMA   (locate lexbuf)
| "->"     -> Tk_ARROW   (locate lexbuf)
| "=>"     -> Tk_ROCKET  (locate lexbuf)

(* Operators *)
| "+@"     -> Tk_UPLUS   (locate lexbuf, lexeme lexbuf)
| "-@"     -> Tk_UMINUS  (locate lexbuf, lexeme lexbuf)
| "~@"     -> Tk_UTILDE  (locate lexbuf, lexeme lexbuf)
| '+'      -> Tk_PLUS    (locate lexbuf, lexeme lexbuf)
| '-'      -> Tk_MINUS   (locate lexbuf, lexeme lexbuf)
| '*'      -> Tk_STAR    (locate lexbuf, lexeme lexbuf)
| "**"     -> Tk_DSTAR   (locate lexbuf, lexeme lexbuf)
| '/'      -> Tk_DIVIDE  (locate lexbuf, lexeme lexbuf)
| '%'      -> Tk_PERCENT (locate lexbuf, lexeme lexbuf)
| '&'      -> Tk_AMPER   (locate lexbuf, lexeme lexbuf)
| '|'      -> Tk_PIPE    (locate lexbuf, lexeme lexbuf)
| '~'      -> Tk_TILDE   (locate lexbuf, lexeme lexbuf)
| "<<"     -> Tk_LSHFT   (locate lexbuf, lexeme lexbuf)
| ">>"     -> Tk_RSHFT   (locate lexbuf, lexeme lexbuf)
| ">>>"    -> Tk_ARSHFT  (locate lexbuf, lexeme lexbuf)
| "=="     -> Tk_EQ      (locate lexbuf, lexeme lexbuf)
| "<="     -> Tk_LEQ     (locate lexbuf, lexeme lexbuf)
| '<'      -> Tk_LT      (locate lexbuf, lexeme lexbuf)
| '>'      -> Tk_GT      (locate lexbuf, lexeme lexbuf)
| ">="     -> Tk_GEQ     (locate lexbuf, lexeme lexbuf)
| "<=>"    -> Tk_CMP     (locate lexbuf, lexeme lexbuf)

(* Keywords *)
| "true"   -> Kw_TRUE    (locate lexbuf, lexeme lexbuf)
| "false"  -> Kw_FALSE   (locate lexbuf, lexeme lexbuf)
| "nil"    -> Kw_NIL     (locate lexbuf, lexeme lexbuf)
| "self"   -> Kw_SELF    (locate lexbuf, lexeme lexbuf)
| "and"    -> Kw_AND     (locate lexbuf, lexeme lexbuf)
| "or"     -> Kw_OR      (locate lexbuf, lexeme lexbuf)
| "not"    -> Kw_NOT     (locate lexbuf, lexeme lexbuf)
| "let"    -> Kw_LET     (locate lexbuf, lexeme lexbuf)
| "mut"    -> Kw_MUT     (locate lexbuf, lexeme lexbuf)
| "while"  -> Kw_WHILE   (locate lexbuf, lexeme lexbuf)
| "do"     -> Kw_DO      (locate lexbuf, lexeme lexbuf)
| "if"     -> Kw_IF      (locate lexbuf, lexeme lexbuf)
| "elsif"  -> Kw_ELSIF   (locate lexbuf, lexeme lexbuf)
| "then"   -> Kw_THEN    (locate lexbuf, lexeme lexbuf)
| "else"   -> Kw_ELSE    (locate lexbuf, lexeme lexbuf)
| "match"  -> Kw_MATCH   (locate lexbuf, lexeme lexbuf)
| "end"    -> Kw_END     (locate lexbuf, lexeme lexbuf)
| "as"     -> Kw_AS      (locate lexbuf, lexeme lexbuf)
| "meta"   -> Kw_META    (locate lexbuf, lexeme lexbuf)
| "type"   -> Kw_TYPE    (locate lexbuf, lexeme lexbuf)
| "public" -> Kw_PUBLIC  (locate lexbuf, lexeme lexbuf)
| "dynamic"-> Kw_DYNAMIC (locate lexbuf, lexeme lexbuf)
| "package"-> Kw_PACKAGE (locate lexbuf, lexeme lexbuf)
| "class"  -> Kw_CLASS   (locate lexbuf, lexeme lexbuf)
| "mixin"  -> Kw_MIXIN   (locate lexbuf, lexeme lexbuf)
| "iface"  -> Kw_IFACE   (locate lexbuf, lexeme lexbuf)
| "def"    -> Kw_DEF     (locate lexbuf, lexeme lexbuf)
| "new"    -> Kw_NEW     (locate lexbuf, lexeme lexbuf)
| "return" -> Kw_RETURN  (locate lexbuf, lexeme lexbuf)

| "invokeprimitive" -> Kw_INVOKE (locate lexbuf, lexeme lexbuf)

(* Values *)
| digits          -> Vl_INT    (locate lexbuf, int_of_string (lexeme lexbuf))
| digits id_alpha -> raise (Unexpected (locate lexbuf, (sub_lexeme lexbuf (-1) (-1)).[0]))
| ':' method_name -> Vl_SYMBOL (locate lexbuf, sub_lexeme lexbuf 1 (-1))
| '\''            -> goto state lex_string;
                     Vl_BEGIN  (locate lexbuf, Syntax.Qu_STRING)
| '"'             -> goto state lex_string_interp;
                     Vl_BEGIN  (locate lexbuf, Syntax.Qu_STRING)
| ":'"            -> goto state lex_string;
                     Vl_BEGIN  (locate lexbuf, Syntax.Qu_SYMBOL)
| ":\""           -> goto state lex_string_interp;
                     Vl_BEGIN  (locate lexbuf, Syntax.Qu_SYMBOL)

(* Identifiers *)
| ident ':'       -> Id_LABEL (locate lexbuf, sub_lexeme lexbuf 0 (-1))
| local           -> Id_LOCAL (locate lexbuf, lexeme lexbuf)
| const           -> Id_CONST (locate lexbuf, lexeme lexbuf)
| '@'  ident      -> Id_IVAR  (locate lexbuf, sub_lexeme lexbuf 1 0)
| '\\' ident      -> Id_TVAR  (locate lexbuf, sub_lexeme lexbuf 1 0)

| _               -> unexpected lexbuf
| eof             -> eof lexbuf

and lex_string state = lexer
| '\''            -> goto state lex_code;
                     Vl_END    (locate lexbuf)
| "\\\\"          -> Vl_STRING (locate lexbuf, "\\")
| "\\'"           -> Vl_STRING (locate lexbuf, "'")
| [^'\'' '\\']+   -> Vl_STRING (locate lexbuf, lexeme lexbuf)

| eof             -> eof lexbuf

and lex_string_interp state = lexer
| '"'               -> goto state lex_code;
                       Vl_END (locate lexbuf)
| "\\\\"            -> Vl_STRING  (locate lexbuf, "\\")
| "\\\""            -> Vl_STRING  (locate lexbuf, "\"")
| "\\" _            -> Vl_STRING  (locate lexbuf, sub_lexeme lexbuf 1 0)
| "#{"              -> push state lex_code; ignore (curly state 1);
                       Vl_UNQUOTE (locate lexbuf)
| '#'               -> Vl_STRING  (locate lexbuf, "#")
| [^'"' '#' '\\']+  -> Vl_STRING  (locate lexbuf, lexeme lexbuf)

| eof               -> eof lexbuf

(* Public API *)
let create file line =
  Location.start_file file line;
  { lexer_stack = [lex_code];
    curly_stack = [1] }

let next state lexbuf =
  (List.hd state.lexer_stack) state lexbuf, Lexing.dummy_pos, Lexing.dummy_pos
