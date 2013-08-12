{
  open Fy_big_int
  open IrParser_tokens
}

let name = ['A'-'Z' 'a'-'z' '0'-'9' ':' '.']+

rule lex = parse
| [' ' '\t' '\n'] { lex lexbuf }
| ';' [^'\n']*    { lex lexbuf }

| '%' (name as n)            { Name_Local  (Unicode.adopt_utf8s n) }
| '%' '"' ([^'"']+ as n) '"' { Name_Local  (Unicode.adopt_utf8s n) }
| '@' (name as n)            { Name_Global (Unicode.adopt_utf8s n) }
| '@' '"' ([^'"']+ as n) '"' { Name_Global (Unicode.adopt_utf8s n) }
| (name as n) ':'            { Name_Label  (Unicode.adopt_utf8s n) }
| '"' ([^'"']+ as n) '"' ':' { Name_Label  (Unicode.adopt_utf8s n) }

| '"' ([^'"']+ as s) '"' { Lit_String  (Unicode.adopt_utf8s s) }
| ['0'-'9']+ as d        { Lit_Integer (big_int_of_string d) }

| '('  { LParen }
| ')'  { RParen }
| '['  { LBrack }
| ']'  { RBrack }
| '{'  { LBrace }
| '}'  { RBrace }
| ','  { Comma }
| '='  { Equal }
| "->" { Arrow }

| "type"          { Type }
| "tvar"          { Tvar }
| "nil"           { Nil }
| "true"          { True }
| "false"         { False }
| "boolean"       { Boolean }
| "int"           { Int }
| "unsigned"      { Unsigned }
| "signed"        { Signed }
| "symbol"        { Symbol }
| "environment"   { Environment }
| "lambda"        { Lambda }
| "class"         { Class }
| "mixin"         { Mixin }
| "package"       { Package }
| "instance"      { Instance }

| "immutable"     { Immutable }
| "mutable"       { Mutable }
| "meta_mutable"  { Meta_mutable }
| "dynamic"       { Dynamic }

| "parent"        { Parent }
| "bindings"      { Bindings }

| "local_env"     { Local_env }
| "type_env"      { Type_env }
| "const_env"     { Const_env }

| "metaclass"           { Metaclass }
| "ancestor"            { Ancestor }
| "type_variables"      { Type_variables }
| "instance_variables"  { Instance_variables }
| "methods"             { Methods }
| "prepended"           { Prepended }
| "appended"            { Appended }
| "constants"           { Constants }

| "args"          { Syntax_Args  (Sexplib.Sexp.scan_sexp lexbuf) }
| "body"          { Syntax_Exprs (Sexplib.Sexp.scan_sexp lexbuf) }

| "empty"         { Empty }

| "function"      { Function }
| "jump"          { Jump }
| "jump_if"       { Jump_if }
| "return"        { Return }
| "frame"         { Frame }
| "lvar_load"     { Lvar_load }
| "lvar_store"    { Lvar_store }
| "primitive"     { Primitive }

| (['a'-'z']+) as kw { failwith ("unknown keyword: " ^ kw) }
| eof                { EOF }

{
  let next lexbuf =
    let token = lex lexbuf in
      token, Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf
}
