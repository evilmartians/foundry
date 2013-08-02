open Unicode.Std
open Big_int
open IrParser_tokens

(* Utilities *)

let lexeme lexbuf begoff endoff =
  Unicode.adopt_utf8s
    (Ulexing.utf8_sub_lexeme lexbuf
      begoff
      ((Ulexing.lexeme_length lexbuf) - begoff + endoff))

(* Lexer *)

let regexp name = ['A'-'Z' 'a'-'z' '0'-'9' ':' '.']+

let rec lex = lexer
| [' ' '\t'] -> lex lexbuf

| '%' name            -> NAME_LOCAL  (lexeme lexbuf 1 0)
| '%' '"' [^'"']+ '"' -> NAME_LOCAL  (lexeme lexbuf 2 1)
| '@' name            -> NAME_GLOBAL (lexeme lexbuf 1 0)
| '@' '"' [^'"']+ '"' -> NAME_GLOBAL (lexeme lexbuf 2 1)

| '"' [^'"']+ '"' -> LIT_STRING  (lexeme lexbuf 1 1)
| ['0'-'9']+      -> LIT_INTEGER (big_int_of_string (lexeme lexbuf 0 0))

| '(' -> LPAREN
| ')' -> RPAREN
| '[' -> LBRACK
| ']' -> RBRACK
| '{' -> LBRACE
| '}' -> RBRACE
| ',' -> COMMA
| '=' -> EQUAL

| "type"          -> TYPE
| "tvar"          -> TVAR
| "nil"           -> NIL
| "true"          -> TRUE
| "false"         -> FALSE
| "boolean"       -> BOOLEAN
| "int"           -> INT
| "symbol"        -> SYMBOL
| "environment"   -> ENVIRONMENT
| "lambda"        -> LAMBDA
| "class"         -> CLASS
| "mixin"         -> MIXIN
| "package"       -> PACKAGE
| "instance"      -> INSTANCE

| "immutable"     -> IMMUTABLE
| "mutable"       -> MUTABLE
| "meta_mutable"  -> META_MUTABLE
| "dynamic"       -> DYNAMIC

| "parent"        -> PARENT
| "bindings"      -> BINDINGS
| "location"      -> LOCATION

| "local_env"     -> LOCAL_ENV
| "type_env"      -> TYPE_ENV
| "const_env"     -> CONST_ENV

| "metaclass"           -> METACLASS
| "ancestor"            -> ANCESTOR
| "type_variables"      -> TYPE_VARIABLES
| "instance_variables"  -> INSTANCE_VARIABLES
| "methods"             -> METHODS
| "prepended"           -> PREPENDED
| "appended"            -> APPENDED
| "constants"           -> CONSTANTS

| ['a'-'z']+ -> failwith ("unknown keyword: " ^ (lexeme lexbuf 0 0))

let next lexbuf =
  lex lexbuf, Lexing.dummy_pos, Lexing.dummy_pos
