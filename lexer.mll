{
  (* Internal API: state management *)
  type state = {
    mutable lexer_stack : (state -> Lexing.lexbuf -> Parser.token) list;
    mutable curly_stack : int list
  }

  let locate lexbuf =
    (Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf)

  let lexeme =
    Lexing.lexeme

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
}

let w_space     = [' ' '\t']+
let w_newline   = '\n' | "\r\n"
let w_any       = w_space | w_newline

let digits      = ['0'-'9']+
let id_lower    = ['a'-'z' '_']
let id_upper    = ['A'-'Z']
let id_alpha    = id_lower | id_upper
let id_alnum    = id_alpha | digits

let ident       = id_alpha id_alnum*
let local       = id_lower id_alnum*
let const       = id_upper id_alnum*

let operator    = ['+' '-' '*' '/' '%' '&' '|' '~'] |
                  "<<" | ">>" | ">>>"
let method_name = local | operator | '<' | '>' | "<=" | ">=" | "==" | "<=>"
let symbol      = ':' method_name

rule lex_code state = parse
| w_space      { lex_code state lexbuf }
| w_newline    { Lexing.new_line lexbuf; lex_code state lexbuf }

(* Punctuation *)
| operator as op '=' { Parser.Tk_OP_ASGN (locate lexbuf, op) }
| "and="    { Parser.Tk_AND_ASGN (locate lexbuf) }
| "or="     { Parser.Tk_OR_ASGN  (locate lexbuf) }

| '{'       { ignore (curly state 1);
              Parser.Tk_LCURLY   (locate lexbuf) }
| '}'       { if (curly state (-1)) = 0 then begin
                pop state;
                Parser.Vl_QUOTE  (locate lexbuf)
              end else
                Parser.Tk_RCURLY (locate lexbuf) }

| '('       { Parser.Tk_LPAREN  (locate lexbuf) }
| ')'       { Parser.Tk_RPAREN  (locate lexbuf) }
| '['       { Parser.Tk_LBRACK  (locate lexbuf) }
| ']'       { Parser.Tk_RBRACK  (locate lexbuf) }
| '='       { Parser.Tk_ASGN    (locate lexbuf) }
| '.'       { Parser.Tk_DOT     (locate lexbuf) }
| ':'       { Parser.Tk_COLON   (locate lexbuf) }
| "::"      { Parser.Tk_DCOLON  (locate lexbuf) }
| ';'       { Parser.Tk_SEMI    (locate lexbuf) }
| ";;"      { Parser.Tk_DSEMI   (locate lexbuf) }
| ','       { Parser.Tk_COMMA   (locate lexbuf) }
| "->"      { Parser.Tk_ARROW   (locate lexbuf) }
| "=>"      { Parser.Tk_ROCKET  (locate lexbuf) }

(* Operators *)
| "+@"      { Parser.Tk_UPLUS   (locate lexbuf, lexeme lexbuf) }
| "-@"      { Parser.Tk_UMINUS  (locate lexbuf, lexeme lexbuf) }
| "~@"      { Parser.Tk_UTILDE  (locate lexbuf, lexeme lexbuf) }
| '+'       { Parser.Tk_PLUS    (locate lexbuf, lexeme lexbuf) }
| '-'       { Parser.Tk_MINUS   (locate lexbuf, lexeme lexbuf) }
| '*'       { Parser.Tk_STAR    (locate lexbuf, lexeme lexbuf) }
| "**"      { Parser.Tk_DSTAR   (locate lexbuf, lexeme lexbuf) }
| '/'       { Parser.Tk_DIVIDE  (locate lexbuf, lexeme lexbuf) }
| '%'       { Parser.Tk_PERCENT (locate lexbuf, lexeme lexbuf) }
| '&'       { Parser.Tk_AMPER   (locate lexbuf, lexeme lexbuf) }
| '|'       { Parser.Tk_PIPE    (locate lexbuf, lexeme lexbuf) }
| '~'       { Parser.Tk_TILDE   (locate lexbuf, lexeme lexbuf) }
| "<<"      { Parser.Tk_LSHFT   (locate lexbuf, lexeme lexbuf) }
| ">>"      { Parser.Tk_RSHFT   (locate lexbuf, lexeme lexbuf) }
| ">>>"     { Parser.Tk_TILDE   (locate lexbuf, lexeme lexbuf) }
| "=="      { Parser.Tk_EQ      (locate lexbuf, lexeme lexbuf) }
| "<="      { Parser.Tk_LEQ     (locate lexbuf, lexeme lexbuf) }
| '<'       { Parser.Tk_LT      (locate lexbuf, lexeme lexbuf) }
| '>'       { Parser.Tk_GT      (locate lexbuf, lexeme lexbuf) }
| ">="      { Parser.Tk_GEQ     (locate lexbuf, lexeme lexbuf) }
| "<=>"     { Parser.Tk_CMP     (locate lexbuf, lexeme lexbuf) }

(* Keywords *)
| "true"    { Parser.Kw_TRUE    (locate lexbuf, lexeme lexbuf) }
| "false"   { Parser.Kw_FALSE   (locate lexbuf, lexeme lexbuf) }
| "nil"     { Parser.Kw_NIL     (locate lexbuf, lexeme lexbuf) }
| "self"    { Parser.Kw_SELF    (locate lexbuf, lexeme lexbuf) }
| "and"     { Parser.Kw_AND     (locate lexbuf, lexeme lexbuf) }
| "or"      { Parser.Kw_OR      (locate lexbuf, lexeme lexbuf) }
| "not"     { Parser.Kw_NOT     (locate lexbuf, lexeme lexbuf) }
| "let"     { Parser.Kw_LET     (locate lexbuf, lexeme lexbuf) }
| "mut"     { Parser.Kw_MUT     (locate lexbuf, lexeme lexbuf) }
| "while"   { Parser.Kw_WHILE   (locate lexbuf, lexeme lexbuf) }
| "do"      { Parser.Kw_DO      (locate lexbuf, lexeme lexbuf) }
| "if"      { Parser.Kw_IF      (locate lexbuf, lexeme lexbuf) }
| "elsif"   { Parser.Kw_ELSIF   (locate lexbuf, lexeme lexbuf) }
| "then"    { Parser.Kw_THEN    (locate lexbuf, lexeme lexbuf) }
| "else"    { Parser.Kw_ELSE    (locate lexbuf, lexeme lexbuf) }
| "match"   { Parser.Kw_MATCH   (locate lexbuf, lexeme lexbuf) }
| "end"     { Parser.Kw_END     (locate lexbuf, lexeme lexbuf) }
| "as"      { Parser.Kw_AS      (locate lexbuf, lexeme lexbuf) }
| "meta"    { Parser.Kw_META    (locate lexbuf, lexeme lexbuf) }
| "type"    { Parser.Kw_TYPE    (locate lexbuf, lexeme lexbuf) }
| "public"  { Parser.Kw_PUBLIC  (locate lexbuf, lexeme lexbuf) }
| "dynamic" { Parser.Kw_DYNAMIC (locate lexbuf, lexeme lexbuf) }
| "package" { Parser.Kw_PACKAGE (locate lexbuf, lexeme lexbuf) }
| "class"   { Parser.Kw_CLASS   (locate lexbuf, lexeme lexbuf) }
| "mixin"   { Parser.Kw_MIXIN   (locate lexbuf, lexeme lexbuf) }
| "iface"   { Parser.Kw_IFACE   (locate lexbuf, lexeme lexbuf) }
| "def"     { Parser.Kw_DEF     (locate lexbuf, lexeme lexbuf) }
| "return"  { Parser.Kw_RETURN  (locate lexbuf, lexeme lexbuf) }

(* Values *)
| digits as v             { Parser.Vl_INT    (locate lexbuf, int_of_string v) }
| digits id_alpha         { failwith "trailing junk in a number" }
| ':' (method_name as v)  { Parser.Vl_SYMBOL (locate lexbuf, v) }
| '\''                    { goto state lex_string;
                            Parser.Vl_BEGIN  (locate lexbuf, Syntax.Qu_STRING) }
| '"'                     { goto state lex_string_interp;
                            Parser.Vl_BEGIN  (locate lexbuf, Syntax.Qu_STRING) }
| ":'"                    { goto state lex_string;
                            Parser.Vl_BEGIN  (locate lexbuf, Syntax.Qu_SYMBOL) }
| ":\""                   { goto state lex_string_interp;
                            Parser.Vl_BEGIN  (locate lexbuf, Syntax.Qu_SYMBOL) }

(* Identifiers *)
| ident as i ':'        { Parser.Id_LABEL (locate lexbuf, i) }
| local as i            { Parser.Id_LOCAL (locate lexbuf, i) }
| const as i            { Parser.Id_CONST (locate lexbuf, i) }
| '@'  (ident as i)     { Parser.Id_IVAR  (locate lexbuf, i) }
| '\\' (ident as i)     { Parser.Id_TVAR  (locate lexbuf, i) }

| eof                   { Parser.EOF }

and lex_string state = parse
| '\''                  { goto state lex_code;
                          Parser.Vl_END (locate lexbuf) }
| "\\\\"                { Parser.Vl_STRING (locate lexbuf, "\\") }
| "\\'"                 { Parser.Vl_STRING (locate lexbuf, "'") }
| [^'\'' '\\']+ as v    { Parser.Vl_STRING (locate lexbuf, v) }
| eof                   { Parser.EOF }

and lex_string_interp state = parse
| '"'                   { goto state lex_code;
                          Parser.Vl_END (locate lexbuf) }
| "\\\\"                { Parser.Vl_STRING (locate lexbuf, "\\") }
| "\\\""                { Parser.Vl_STRING (locate lexbuf, "\"") }
| "\\" (_ as v)         { Parser.Vl_STRING (locate lexbuf, String.make 1 v) }
| "#{"                  { push state lex_code; ignore (curly state 1);
                          Parser.Vl_UNQUOTE (locate lexbuf) }
| '#'                   { Parser.Vl_STRING (locate lexbuf, "#") }
| [^'"' '#' '\\']+ as v { Parser.Vl_STRING (locate lexbuf, v) }
| eof                   { Parser.EOF }

{
  (* Public API *)
  let create () =
    { lexer_stack = [lex_code]; curly_stack = [1] }

  let next state lexbuf =
    (List.hd state.lexer_stack) state lexbuf
}
