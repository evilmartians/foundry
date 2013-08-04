%token <Unicode.utf8s>      Name_Local
%token <Unicode.utf8s>      Name_Global

%token <Unicode.utf8s>      Lit_String
%token <Fy_big_int.big_int> Lit_Integer

%token <Sexplib.Sexp.t>     Syntax_Args
%token <Sexplib.Sexp.t>     Syntax_Exprs

%token LParen
%token RParen
%token LBrack
%token RBrack
%token LBrace
%token RBrace
%token Comma
%token Equal
%token Arrow

%token Type
%token Tvar
%token Nil
%token True
%token False
%token Boolean
%token Int
%token Symbol
%token Environment
%token Lambda
%token Class
%token Mixin
%token Package
%token Instance

%token Immutable
%token Mutable
%token Meta_mutable
%token Dynamic

%token Parent
%token Bindings

%token Location
%token Local_env
%token Type_env
%token Const_env

%token Metaclass
%token Ancestor
%token Type_variables
%token Instance_variables
%token Methods
%token Prepended
%token Appended
%token Constants

%token EOF

%%
