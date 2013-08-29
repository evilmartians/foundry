open Parser_tokens

let name_of_token = function
  | Tk_LT _ -> "Tk_LT"
  | Kw_MATCH _ -> "Kw_MATCH"
  | Vl_END _ -> "Vl_END"
  | Kw_TYPE _ -> "Kw_TYPE"
  | Kw_IFACE _ -> "Kw_IFACE"
  | Kw_IF _ -> "Kw_IF"
  | Kw_LET _ -> "Kw_LET"
  | Tk_DCOLON _ -> "Tk_DCOLON"
  | Kw_AS _ -> "Kw_AS"
  | Kw_DEF _ -> "Kw_DEF"
  | Tk_PERCENT _ -> "Tk_PERCENT"
  | Tk_UTILDE _ -> "Tk_UTILDE"
  | Tk_DIVIDE _ -> "Tk_DIVIDE"
  | Tk_LE _ -> "Tk_LE"
  | Tk_AMPER _ -> "Tk_AMPER"
  | Tk_LCURLY _ -> "Tk_LCURLY"
  | Tk_LPAREN _ -> "Tk_LPAREN"
  | Vl_SYMBOL _ -> "Vl_SYMBOL"
  | Tk_RPAREN _ -> "Tk_RPAREN"
  | Kw_DO _ -> "Kw_DO"
  | Kw_META _ -> "Kw_META"
  | Tk_AND_ASGN _ -> "Tk_AND_ASGN"
  | Tk_EQ _ -> "Tk_EQ"
  | Vl_QUOTE _ -> "Vl_QUOTE"
  | Kw_NOT _ -> "Kw_NOT"
  | Tk_DSEMI _ -> "Tk_DSEMI"
  | EOF _ -> "EOF"
  | Tk_RSHFT _ -> "Tk_RSHFT"
  | Tk_PLUS _ -> "Tk_PLUS"
  | Kw_PUBLIC _ -> "Kw_PUBLIC"
  | Kw_ELSIF _ -> "Kw_ELSIF"
  | Tk_ASGN _ -> "Tk_ASGN"
  | Kw_FALSE _ -> "Kw_FALSE"
  | Tk_GE _ -> "Tk_GE"
  | Tk_DOT _ -> "Tk_DOT"
  | Vl_INT _ -> "Vl_INT"
  | Tk_DSTAR _ -> "Tk_DSTAR"
  | Kw_CLASS _ -> "Kw_CLASS"
  | Tk_UPLUS _ -> "Tk_UPLUS"
  | Tk_LBRACK _ -> "Tk_LBRACK"
  | Id_TVAR _ -> "Id_TVAR"
  | Kw_TRUE _ -> "Kw_TRUE"
  | Id_CONST _ -> "Id_CONST"
  | Tk_STAR _ -> "Tk_STAR"
  | Tk_COLON _ -> "Tk_COLON"
  | Tk_RBRACK _ -> "Tk_RBRACK"
  | Id_IVAR _ -> "Id_IVAR"
  | Vl_BEGIN _ -> "Vl_BEGIN"
  | Tk_OR_ASGN _ -> "Tk_OR_ASGN"
  | Tk_MINUS _ -> "Tk_MINUS"
  | Tk_RCURLY _ -> "Tk_RCURLY"
  | Tk_ARROW _ -> "Tk_ARROW"
  | Kw_RETURN _ -> "Kw_RETURN"
  | Tk_SEMI _ -> "Tk_SEMI"
  | Vl_STRING _ -> "Vl_STRING"
  | Kw_END _ -> "Kw_END"
  | Tk_GT _ -> "Tk_GT"
  | Kw_ELSE _ -> "Kw_ELSE"
  | Kw_AND _ -> "Kw_AND"
  | Tk_PIPE _ -> "Tk_PIPE"
  | Vl_UNQUOTE _ -> "Vl_UNQUOTE"
  | Kw_MUT _ -> "Kw_MUT"
  | Kw_DYNAMIC _ -> "Kw_DYNAMIC"
  | Kw_NIL _ -> "Kw_NIL"
  | Id_LABEL _ -> "Id_LABEL"
  | Kw_PACKAGE _ -> "Kw_PACKAGE"
  | Kw_SELF _ -> "Kw_SELF"
  | Id_LOCAL _ -> "Id_LOCAL"
  | Kw_THEN _ -> "Kw_THEN"
  | Tk_TILDE _ -> "Tk_TILDE"
  | Tk_COMMA _ -> "Tk_COMMA"
  | Tk_OP_ASGN _ -> "Tk_OP_ASGN"
  | Kw_WHILE _ -> "Kw_WHILE"
  | Tk_LSHFT _ -> "Tk_LSHFT"
  | Tk_CMP _ -> "Tk_CMP"
  | Kw_MIXIN _ -> "Kw_MIXIN"
  | Tk_UMINUS _ -> "Tk_UMINUS"
  | Tk_ROCKET _ -> "Tk_ROCKET"
  | Kw_OR _ -> "Kw_OR"
  | Kw_INVOKEPRIMITIVE _ -> "Kw_INVOKEPRIMITIVE"
  | Tk_CARET _ -> "Tk_CARET"
  | Tk_NEWLINE _ -> "Tk_NEWLINE"
  | Vl_UINT _ -> "Vl_UINT"
  | Vl_SINT _ -> "Vl_SINT"
  | Kw_UNTIL _ -> "Kw_UNTIL"
  | Kw_UNLESS _ -> "Kw_UNLESS"
  | Kw_ASSERT _ -> "Kw_ASSERT"
  | Id_METHOD _ -> "Id_METHOD"
  | Id_ASSIGN _ -> "Id_ASSIGN"
  | Tk_NE _ -> "Tk_NE"

let loc_of_token token =
  match token with
  | Vl_UNQUOTE (loc) -> loc
  | Vl_UINT (loc, _, _) -> loc
  | Vl_SYMBOL (loc, _) -> loc
  | Vl_STRING (loc, _) -> loc
  | Vl_SINT (loc, _, _) -> loc
  | Vl_QUOTE (loc) -> loc
  | Vl_INT (loc, _) -> loc
  | Vl_END (loc) -> loc
  | Vl_BEGIN (loc, _) -> loc
  | Tk_UTILDE (loc, _) -> loc
  | Tk_UPLUS (loc, _) -> loc
  | Tk_UMINUS (loc, _) -> loc
  | Tk_TILDE (loc, _) -> loc
  | Tk_STAR (loc, _) -> loc
  | Tk_SEMI (loc) -> loc
  | Tk_RSHFT (loc, _) -> loc
  | Tk_RPAREN (loc) -> loc
  | Tk_ROCKET (loc) -> loc
  | Tk_RCURLY (loc) -> loc
  | Tk_RBRACK (loc) -> loc
  | Tk_PLUS (loc, _) -> loc
  | Tk_PIPE (loc, _) -> loc
  | Tk_PERCENT (loc, _) -> loc
  | Tk_OR_ASGN (loc) -> loc
  | Tk_OP_ASGN (loc, _) -> loc
  | Tk_NEWLINE (loc) -> loc
  | Tk_NE (loc, _) -> loc
  | Tk_MINUS (loc, _) -> loc
  | Tk_LT (loc, _) -> loc
  | Tk_LSHFT (loc, _) -> loc
  | Tk_LPAREN (loc) -> loc
  | Tk_LE (loc, _) -> loc
  | Tk_LCURLY (loc) -> loc
  | Tk_LBRACK (loc) -> loc
  | Tk_GT (loc, _) -> loc
  | Tk_GE (loc, _) -> loc
  | Tk_EQ (loc, _) -> loc
  | Tk_DSTAR (loc, _) -> loc
  | Tk_DSEMI (loc) -> loc
  | Tk_DOT (loc) -> loc
  | Tk_DIVIDE (loc, _) -> loc
  | Tk_DCOLON (loc) -> loc
  | Tk_COMMA (loc) -> loc
  | Tk_COLON (loc) -> loc
  | Tk_CMP (loc, _) -> loc
  | Tk_CARET (loc, _) -> loc
  | Tk_ASGN (loc) -> loc
  | Tk_ARROW (loc) -> loc
  | Tk_AND_ASGN (loc) -> loc
  | Tk_AMPER (loc, _) -> loc
  | Kw_WHILE (loc, _) -> loc
  | Kw_UNTIL (loc, _) -> loc
  | Kw_UNLESS (loc, _) -> loc
  | Kw_TYPE (loc, _) -> loc
  | Kw_TRUE (loc, _) -> loc
  | Kw_THEN (loc, _) -> loc
  | Kw_SELF (loc, _) -> loc
  | Kw_RETURN (loc, _) -> loc
  | Kw_PUBLIC (loc, _) -> loc
  | Kw_PACKAGE (loc, _) -> loc
  | Kw_OR (loc, _) -> loc
  | Kw_NOT (loc, _) -> loc
  | Kw_NIL (loc, _) -> loc
  | Kw_MUT (loc, _) -> loc
  | Kw_MIXIN (loc, _) -> loc
  | Kw_META (loc, _) -> loc
  | Kw_MATCH (loc, _) -> loc
  | Kw_LET (loc, _) -> loc
  | Kw_INVOKEPRIMITIVE (loc, _) -> loc
  | Kw_IFACE (loc, _) -> loc
  | Kw_IF (loc, _) -> loc
  | Kw_FALSE (loc, _) -> loc
  | Kw_END (loc, _) -> loc
  | Kw_ELSIF (loc, _) -> loc
  | Kw_ELSE (loc, _) -> loc
  | Kw_DYNAMIC (loc, _) -> loc
  | Kw_DO (loc, _) -> loc
  | Kw_DEF (loc, _) -> loc
  | Kw_CLASS (loc, _) -> loc
  | Kw_ASSERT (loc, _) -> loc
  | Kw_AS (loc, _) -> loc
  | Kw_AND (loc, _) -> loc
  | Id_TVAR (loc, _) -> loc
  | Id_METHOD (loc, _) -> loc
  | Id_LOCAL (loc, _) -> loc
  | Id_LABEL (loc, _) -> loc
  | Id_IVAR (loc, _) -> loc
  | Id_CONST (loc, _) -> loc
  | Id_ASSIGN (loc, _) -> loc
  | EOF (loc) -> loc
