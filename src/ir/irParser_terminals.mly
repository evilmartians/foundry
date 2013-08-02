%token <Unicode.utf8s>      NAME_LOCAL
%token <Unicode.utf8s>      NAME_GLOBAL

%token <Unicode.utf8s>      LIT_STRING
%token <Fy_big_int.big_int> LIT_INTEGER

%token <Syntax.formal_args> SYNTAX_ARGS
%token <Syntax.exprs>       SYNTAX_EXPRS

%token LPAREN
%token RPAREN
%token LBRACK
%token RBRACK
%token LBRACE
%token RBRACE
%token COMMA
%token EQUAL

%token TYPE
%token TVAR
%token NIL
%token TRUE
%token FALSE
%token BOOLEAN
%token INT
%token SYMBOL
%token ENVIRONMENT
%token LAMBDA
%token CLASS
%token MIXIN
%token PACKAGE
%token INSTANCE

%token IMMUTABLE
%token MUTABLE
%token META_MUTABLE
%token DYNAMIC

%token PARENT
%token BINDINGS

%token LOCATION
%token LOCAL_ENV
%token TYPE_ENV
%token CONST_ENV

%token METACLASS
%token ANCESTOR
%token TYPE_VARIABLES
%token INSTANCE_VARIABLES
%token METHODS
%token PREPENDED
%token APPENDED
%token CONSTANTS

%%
