%token <Unicode.utf8s>      Name_Local
%token <Unicode.utf8s>      Name_Global
%token <Unicode.utf8s>      Name_Label

%token <Unicode.utf8s>      Lit_String
%token <Fy_big_int.big_int> Lit_Integer

%token <Syntax.formal_args> Syntax_Args
%token <Syntax.exprs>       Syntax_Exprs
%token <Syntax.expr>        Syntax_Lambda

%token LParen
%token RParen
%token LBrack
%token RBrack
%token LBrace
%token RBrace
%token Comma
%token Equal
%token Arrow
%token FatArrow

%token Type
%token Tvar
%token Nil
%token True
%token False
%token Boolean
%token Int
%token Signed
%token Unsigned
%token Symbol
%token Environment
%token Lambda
%token Closure
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

%token Empty

%token Function
%token Jump
%token Jump_if
%token Return
%token Phi
%token Frame
%token Lvar_load
%token Lvar_store
%token Resolve
%token Call
%token Primitive

%token Map

%token EOF

%%
