open Unicode.Std
open Big_int
open Big_int_conv
open Parser_tokens

(* State management *)

type state = {
          file        : Location.file;
  mutable lexer_stack : (state -> Ulexing.lexbuf -> token) list;
  mutable curly_stack : int list;
  mutable is_begin    : bool;
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

let expr_begin state is_begin =
  state.is_begin <- is_begin

(* Token helpers *)

exception Unexpected of Location.t * char

let lexeme lexbuf =
  (Unicode.adopt_utf8s (Ulexing.utf8_lexeme lexbuf))

let sub_lexeme lexbuf from to_ =
  let from = if from < 0 then (Ulexing.lexeme_length lexbuf) + from
                         else from
  in
  let len  = if to_ <= 0 then (Ulexing.lexeme_length lexbuf) + to_
                         else to_ - from
  in (Unicode.adopt_utf8s (Ulexing.utf8_sub_lexeme lexbuf from len))

let locate state lexbuf =
  Location.make state.file (Ulexing.lexeme_start lexbuf) (Ulexing.lexeme_end lexbuf)

let eof state lexbuf =
  EOF (Location.make state.file (Ulexing.lexeme_start lexbuf - 1) (Ulexing.lexeme_end lexbuf))

let unexpected state lexbuf =
  raise (Unexpected (locate state lexbuf, (lexeme lexbuf).[0]))

(* Lexer state machine *)

let regexp w_space     = [' ' '\t']+
let regexp w_newline   = '\n' | "\r\n"
let regexp w_any       = w_space | w_newline

let regexp digits      = ['0'-'9']+
let regexp hexdigits   = ['0'-'9' 'a'-'f' 'A'-'F']+
let regexp bindigits   = ['0' '1']+
let regexp id_lower    = ['a'-'z' '_']
let regexp id_upper    = ['A'-'Z']
let regexp id_alpha    = id_lower | id_upper
let regexp id_alnum    = id_alpha | digits

let regexp number      = ( digits    '_'? )+
let regexp hexnumber   = ( hexdigits '_'? )+
let regexp binnumber   = ( bindigits '_'? )+

let regexp ident       = id_alpha id_alnum*
let regexp local       = id_lower id_alnum*
let regexp const       = id_upper id_alnum*

let regexp operator    = ['+' '-' '*' '/' '%' '&' '|' '^' '~'] | "**" | "<<" | ">>"
let regexp method_name = ident ['=' '!' '?']? | operator | "[]" | "[]=" |
                         '<' | '>' | "<=" | ">=" | "==" | "!=" | "<=>" |
                         ['-' '+' '~'] '@'

let rec lex_code state = lexer
| w_space       -> lex_code state lexbuf
| w_newline     -> (if state.is_begin then
                      lex_code state lexbuf
                    else begin
                      expr_begin state true;
                      Tk_NEWLINE (locate state lexbuf)
                    end)
| '#' [^'\n']*  -> lex_code state lexbuf

(* Punctuation *)
| operator '=' -> expr_begin state true; Tk_OP_ASGN  (locate state lexbuf, sub_lexeme lexbuf 0 (-1))
| "and="       -> expr_begin state true; Tk_AND_ASGN (locate state lexbuf)
| "or="        -> expr_begin state true; Tk_OR_ASGN  (locate state lexbuf)

| '{'      -> ignore (curly state 1);
              expr_begin state true;
              Tk_LCURLY   (locate state lexbuf)

| '}'      -> expr_begin state false;
              if (curly state (-1)) = 0 then begin
                pop state;
                Vl_QUOTE  (locate state lexbuf)
              end else
                Tk_RCURLY (locate state lexbuf)

| '('      -> expr_begin state true;  Tk_LPAREN  (locate state lexbuf)
| ')'      -> expr_begin state false; Tk_RPAREN  (locate state lexbuf)
| '['      -> expr_begin state true;  Tk_LBRACK  (locate state lexbuf)
| ']'      -> expr_begin state false; Tk_RBRACK  (locate state lexbuf)
| '='      -> expr_begin state true;  Tk_ASGN    (locate state lexbuf)
| '.'      -> goto state lex_dot;
              expr_begin state false; Tk_DOT     (locate state lexbuf)
| ':'      -> expr_begin state true;  Tk_COLON   (locate state lexbuf)
| "::"     -> expr_begin state true;  Tk_DCOLON  (locate state lexbuf)
| ';'      -> expr_begin state true;  Tk_SEMI    (locate state lexbuf)
| ";;"     -> expr_begin state true;  Tk_DSEMI   (locate state lexbuf)
| ','      -> expr_begin state true;  Tk_COMMA   (locate state lexbuf)
| "->"     -> expr_begin state true;  Tk_ARROW   (locate state lexbuf)
| "=>"     -> expr_begin state true;  Tk_ROCKET  (locate state lexbuf)

(* Operators *)
| "+@"     -> expr_begin state false; Tk_UPLUS   (locate state lexbuf, lexeme lexbuf)
| "-@"     -> expr_begin state false; Tk_UMINUS  (locate state lexbuf, lexeme lexbuf)
| "~@"     -> expr_begin state false; Tk_UTILDE  (locate state lexbuf, lexeme lexbuf)
| '+'      -> expr_begin state true;  Tk_PLUS    (locate state lexbuf, lexeme lexbuf)
| '-'      -> expr_begin state true;  Tk_MINUS   (locate state lexbuf, lexeme lexbuf)
| '*'      -> expr_begin state true;  Tk_STAR    (locate state lexbuf, lexeme lexbuf)
| "**"     -> expr_begin state true;  Tk_DSTAR   (locate state lexbuf, lexeme lexbuf)
| '/'      -> expr_begin state true;  Tk_DIVIDE  (locate state lexbuf, lexeme lexbuf)
| '%'      -> expr_begin state true;  Tk_PERCENT (locate state lexbuf, lexeme lexbuf)
| '&'      -> expr_begin state true;  Tk_AMPER   (locate state lexbuf, lexeme lexbuf)
| '|'      -> expr_begin state true;  Tk_PIPE    (locate state lexbuf, lexeme lexbuf)
| '^'      -> expr_begin state true;  Tk_CARET   (locate state lexbuf, lexeme lexbuf)
| '~'      -> expr_begin state true;  Tk_TILDE   (locate state lexbuf, lexeme lexbuf)
| "<<"     -> expr_begin state true;  Tk_LSHFT   (locate state lexbuf, lexeme lexbuf)
| ">>"     -> expr_begin state true;  Tk_RSHFT   (locate state lexbuf, lexeme lexbuf)
| "=="     -> expr_begin state true;  Tk_EQ      (locate state lexbuf, lexeme lexbuf)
| "!="     -> expr_begin state true;  Tk_NE      (locate state lexbuf, lexeme lexbuf)
| "<="     -> expr_begin state true;  Tk_LE      (locate state lexbuf, lexeme lexbuf)
| '<'      -> expr_begin state true;  Tk_LT      (locate state lexbuf, lexeme lexbuf)
| ">="     -> expr_begin state true;  Tk_GE      (locate state lexbuf, lexeme lexbuf)
| '>'      -> expr_begin state true;  Tk_GT      (locate state lexbuf, lexeme lexbuf)
| "<=>"    -> expr_begin state true;  Tk_CMP     (locate state lexbuf, lexeme lexbuf)

(* Keywords *)
| "true"   -> expr_begin state false; Kw_TRUE    (locate state lexbuf, lexeme lexbuf)
| "false"  -> expr_begin state false; Kw_FALSE   (locate state lexbuf, lexeme lexbuf)
| "nil"    -> expr_begin state false; Kw_NIL     (locate state lexbuf, lexeme lexbuf)
| "self"   -> expr_begin state false; Kw_SELF    (locate state lexbuf, lexeme lexbuf)
| "and"    -> expr_begin state true;  Kw_AND     (locate state lexbuf, lexeme lexbuf)
| "or"     -> expr_begin state true;  Kw_OR      (locate state lexbuf, lexeme lexbuf)
| "not"    -> expr_begin state true;  Kw_NOT     (locate state lexbuf, lexeme lexbuf)
| "let"    -> expr_begin state true;  Kw_LET     (locate state lexbuf, lexeme lexbuf)
| "mut"    -> expr_begin state true;  Kw_MUT     (locate state lexbuf, lexeme lexbuf)
| "while"  -> expr_begin state true;  Kw_WHILE   (locate state lexbuf, lexeme lexbuf)
| "until"  -> expr_begin state true;  Kw_UNTIL   (locate state lexbuf, lexeme lexbuf)
| "do"     -> expr_begin state true;  Kw_DO      (locate state lexbuf, lexeme lexbuf)
| "if"     -> expr_begin state true;  Kw_IF      (locate state lexbuf, lexeme lexbuf)
| "unless" -> expr_begin state true;  Kw_UNLESS  (locate state lexbuf, lexeme lexbuf)
| "elsif"  -> expr_begin state true;  Kw_ELSIF   (locate state lexbuf, lexeme lexbuf)
| "then"   -> expr_begin state true;  Kw_THEN    (locate state lexbuf, lexeme lexbuf)
| "else"   -> expr_begin state true;  Kw_ELSE    (locate state lexbuf, lexeme lexbuf)
| "match"  -> expr_begin state true;  Kw_MATCH   (locate state lexbuf, lexeme lexbuf)
| "end"    -> expr_begin state false; Kw_END     (locate state lexbuf, lexeme lexbuf)
| "as"     -> expr_begin state true;  Kw_AS      (locate state lexbuf, lexeme lexbuf)
| "meta"   -> expr_begin state true;  Kw_META    (locate state lexbuf, lexeme lexbuf)
| "type"   -> expr_begin state true;  Kw_TYPE    (locate state lexbuf, lexeme lexbuf)
| "public" -> expr_begin state true;  Kw_PUBLIC  (locate state lexbuf, lexeme lexbuf)
| "dynamic"-> expr_begin state true;  Kw_DYNAMIC (locate state lexbuf, lexeme lexbuf)
| "package"-> expr_begin state true;  Kw_PACKAGE (locate state lexbuf, lexeme lexbuf)
| "class"  -> expr_begin state true;  Kw_CLASS   (locate state lexbuf, lexeme lexbuf)
| "mixin"  -> expr_begin state true;  Kw_MIXIN   (locate state lexbuf, lexeme lexbuf)
| "iface"  -> expr_begin state true;  Kw_IFACE   (locate state lexbuf, lexeme lexbuf)
| "def"    -> goto state lex_def;
              expr_begin state false; Kw_DEF     (locate state lexbuf, lexeme lexbuf)
| "return" -> expr_begin state false; Kw_RETURN  (locate state lexbuf, lexeme lexbuf)
| "assert" -> expr_begin state false; Kw_ASSERT  (locate state lexbuf, lexeme lexbuf)

| "invokeprimitive" ->
              expr_begin state true;  Kw_INVOKEPRIMITIVE (locate state lexbuf, lexeme lexbuf)

(* Values *)
| "0x" hexnumber  -> expr_begin state false;
                     lex_number state (big_int_of_hex_string (sub_lexeme lexbuf 2 (-2))) lexbuf

| "0b" binnumber  -> expr_begin state false;
                     lex_number state (big_int_of_bin_string (sub_lexeme lexbuf 2 (-2))) lexbuf

| number          -> expr_begin state false;
                     lex_number state (big_int_of_dec_string (lexeme lexbuf)) lexbuf

| ':' method_name -> expr_begin state false;
                     Vl_SYMBOL (locate state lexbuf, sub_lexeme lexbuf 1 (-1))

| '\''            -> expr_begin state false;
                     goto state lex_string;
                     Vl_BEGIN  (locate state lexbuf, Syntax.QuoteAsString)

| '"'             -> expr_begin state false;
                     goto state lex_string_interp;
                     Vl_BEGIN  (locate state lexbuf, Syntax.QuoteAsString)

| ":'"            -> expr_begin state false;
                     goto state lex_string;
                     Vl_BEGIN  (locate state lexbuf, Syntax.QuoteAsSymbol)

| ":\""           -> expr_begin state false;
                     goto state lex_string_interp;
                     Vl_BEGIN  (locate state lexbuf, Syntax.QuoteAsSymbol)

(* Identifiers *)
| ident ':'       -> expr_begin state true;
                     Id_LABEL (locate state lexbuf, sub_lexeme lexbuf 0 (-1))

| local           -> expr_begin state false;
                     Id_LOCAL (locate state lexbuf, lexeme lexbuf)

| const           -> expr_begin state false;
                     Id_CONST (locate state lexbuf, lexeme lexbuf)

| '@'  ident      -> expr_begin state false;
                     Id_IVAR  (locate state lexbuf, sub_lexeme lexbuf 1 (-1))

| '\\' ident      -> expr_begin state false;
                     Id_TVAR  (locate state lexbuf, sub_lexeme lexbuf 1 (-1))

| _               -> unexpected state lexbuf
| eof             -> eof state lexbuf

and lex_dot state = lexer
| w_any           -> lex_dot state lexbuf
| ident ['?''!']? -> goto state lex_code;
                     Id_METHOD (locate state lexbuf, lexeme lexbuf)
| ident '='       -> goto state lex_code;
                     Id_ASSIGN (locate state lexbuf, lexeme lexbuf)

| _               -> Ulexing.rollback lexbuf; goto state lex_code;
                     lex_code state lexbuf
| eof             -> eof state lexbuf

and lex_def state = lexer
| w_any           -> lex_def state lexbuf
| "self"          -> goto state lex_code;
                     Kw_SELF (locate state lexbuf, lexeme lexbuf)
| method_name     -> goto state lex_code;
                     Id_METHOD (locate state lexbuf, lexeme lexbuf)
| '@'  ident      -> goto state lex_code;
                     Id_IVAR  (locate state lexbuf, sub_lexeme lexbuf 1 (-1))
| _               -> unexpected state lexbuf
| eof             -> eof state lexbuf

and lex_number state digits = lexer
| 'u' digits      -> Vl_UINT (locate state lexbuf,
                              int_of_string (sub_lexeme lexbuf 1 (-1)), digits)
| 's' digits      -> Vl_SINT (locate state lexbuf,
                              int_of_string (sub_lexeme lexbuf 1 (-1)), digits)

| id_alpha        -> raise (Unexpected (locate state lexbuf, (lexeme lexbuf).[0]))
| _               -> Ulexing.rollback lexbuf;
                     Vl_INT  (locate state lexbuf, digits)

| eof             -> eof state lexbuf

and lex_string state = lexer
| '\''            -> goto state lex_code;
                     Vl_END    (locate state lexbuf)
| "\\\\"          -> Vl_STRING (locate state lexbuf, "\\")
| "\\'"           -> Vl_STRING (locate state lexbuf, "'")
| [^'\'' '\\']+   -> Vl_STRING (locate state lexbuf, lexeme lexbuf)

| eof             -> eof state lexbuf

and lex_string_interp state = lexer
| '"'               -> goto state lex_code;
                       Vl_END (locate state lexbuf)
| "\\\\"            -> Vl_STRING  (locate state lexbuf, "\\")
| "\\\""            -> Vl_STRING  (locate state lexbuf, "\"")
| "\\" _            -> Vl_STRING  (locate state lexbuf, sub_lexeme lexbuf 1 0)
| "#{"              -> push state lex_code; ignore (curly state 1);
                       Vl_UNQUOTE (locate state lexbuf)
| '#'               -> Vl_STRING  (locate state lexbuf, "#")
| [^'"' '#' '\\']+  -> Vl_STRING  (locate state lexbuf, lexeme lexbuf)

| eof               -> eof state lexbuf

(* Public API *)
let create file =
  { file;
    lexer_stack = [lex_code];
    curly_stack = [1];
    is_begin    = true }

let next state lexbuf =
  (List.hd state.lexer_stack) state lexbuf, Lexing.dummy_pos, Lexing.dummy_pos
