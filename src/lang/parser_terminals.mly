%token <Location.t * int>           Vl_INT
%token <Location.t * Unicode.utf8s> Vl_SYMBOL
%token <Location.t * Syntax.quote>  Vl_BEGIN
%token <Location.t * Unicode.utf8s> Vl_STRING

%token <Location.t> Vl_UNQUOTE
%token <Location.t> Vl_QUOTE
%token <Location.t> Vl_END

%token <Location.t * Unicode.utf8s> Id_LOCAL
%token <Location.t * Unicode.utf8s> Id_CONST
%token <Location.t * Unicode.utf8s> Id_LABEL
%token <Location.t * Unicode.utf8s> Id_IVAR
%token <Location.t * Unicode.utf8s> Id_TVAR

%token <Location.t> Tk_LPAREN
%token <Location.t> Tk_RPAREN
%token <Location.t> Tk_LCURLY
%token <Location.t> Tk_RCURLY
%token <Location.t> Tk_LBRACK
%token <Location.t> Tk_RBRACK
%token <Location.t> Tk_ASGN
%token <Location.t> Tk_ARROW
%token <Location.t> Tk_ROCKET
%token <Location.t> Tk_DOT
%token <Location.t> Tk_COLON
%token <Location.t> Tk_DCOLON
%token <Location.t> Tk_COMMA
%token <Location.t> Tk_SEMI
%token <Location.t> Tk_DSEMI

%token <Location.t> Tk_OR_ASGN
%token <Location.t> Tk_AND_ASGN

%token <Location.t * Unicode.utf8s> Tk_OP_ASGN
%token <Location.t * Unicode.utf8s> Tk_PLUS
%token <Location.t * Unicode.utf8s> Tk_MINUS
%token <Location.t * Unicode.utf8s> Tk_STAR
%token <Location.t * Unicode.utf8s> Tk_DSTAR
%token <Location.t * Unicode.utf8s> Tk_DIVIDE
%token <Location.t * Unicode.utf8s> Tk_PERCENT
%token <Location.t * Unicode.utf8s> Tk_AMPER
%token <Location.t * Unicode.utf8s> Tk_PIPE
%token <Location.t * Unicode.utf8s> Tk_LSHFT
%token <Location.t * Unicode.utf8s> Tk_RSHFT
%token <Location.t * Unicode.utf8s> Tk_ARSHFT
%token <Location.t * Unicode.utf8s> Tk_TILDE
%token <Location.t * Unicode.utf8s> Tk_EQ
%token <Location.t * Unicode.utf8s> Tk_LT
%token <Location.t * Unicode.utf8s> Tk_GT
%token <Location.t * Unicode.utf8s> Tk_LEQ
%token <Location.t * Unicode.utf8s> Tk_GEQ
%token <Location.t * Unicode.utf8s> Tk_CMP
%token <Location.t * Unicode.utf8s> Tk_UPLUS
%token <Location.t * Unicode.utf8s> Tk_UMINUS
%token <Location.t * Unicode.utf8s> Tk_UTILDE

%token <Location.t * Unicode.utf8s> Kw_TRUE
%token <Location.t * Unicode.utf8s> Kw_FALSE
%token <Location.t * Unicode.utf8s> Kw_NIL
%token <Location.t * Unicode.utf8s> Kw_SELF
%token <Location.t * Unicode.utf8s> Kw_AND
%token <Location.t * Unicode.utf8s> Kw_OR
%token <Location.t * Unicode.utf8s> Kw_NOT
%token <Location.t * Unicode.utf8s> Kw_LET
%token <Location.t * Unicode.utf8s> Kw_MUT
%token <Location.t * Unicode.utf8s> Kw_AS
%token <Location.t * Unicode.utf8s> Kw_TYPE
%token <Location.t * Unicode.utf8s> Kw_META
%token <Location.t * Unicode.utf8s> Kw_DYNAMIC
%token <Location.t * Unicode.utf8s> Kw_WHILE
%token <Location.t * Unicode.utf8s> Kw_DO
%token <Location.t * Unicode.utf8s> Kw_IF
%token <Location.t * Unicode.utf8s> Kw_THEN
%token <Location.t * Unicode.utf8s> Kw_ELSE
%token <Location.t * Unicode.utf8s> Kw_ELSIF
%token <Location.t * Unicode.utf8s> Kw_MATCH
%token <Location.t * Unicode.utf8s> Kw_RETURN
%token <Location.t * Unicode.utf8s> Kw_END
%token <Location.t * Unicode.utf8s> Kw_PUBLIC
%token <Location.t * Unicode.utf8s> Kw_PACKAGE
%token <Location.t * Unicode.utf8s> Kw_CLASS
%token <Location.t * Unicode.utf8s> Kw_MIXIN
%token <Location.t * Unicode.utf8s> Kw_IFACE
%token <Location.t * Unicode.utf8s> Kw_DEF

%token EOF

%%
