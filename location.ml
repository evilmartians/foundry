open Sexplib.Std

type t = (int * int)
with sexp_of

type nullary      = t * unit

type collection_i = {
  start           : t;
  finish          : t;
}
type collection   = t * collection_i

type operator_i   = {
  operator        : t;
}
type operator     = t * operator_i

type send_i       = {
  dot             : t option;
  selector        : t option;
  lparen          : t option;
  rparen          : t option;
}
type send         = t * send_i

type let_bind_i   = {
  mut             : t option;
  rename          : t option;
  identifier      : t;
}
type let_bind     = t * let_bind_i

let sexp_of_nullary loc =
  Sexplib.Sexp.List []
let sexp_of_collection loc =
  Sexplib.Sexp.List []
let sexp_of_send loc =
  Sexplib.Sexp.List []
let sexp_of_operator loc =
  Sexplib.Sexp.List []
let sexp_of_let_bind loc =
  Sexplib.Sexp.List []

let combine fst_loc snd_loc =
  let (f1, f2), (s1, s2) = fst_loc, snd_loc in
    (min f1 s1, max f2 s2)

let nullary token =
  (token, ())

let collection start finish =
  (combine start finish, {
    start  = start;
    finish = finish;
  })

let lambda start finish body =
  (combine start body, {
    start  = start;
    finish = finish;
  })

let unary op arg =
  (combine op arg,  { operator = op })

let binary lhs op rhs =
  (combine lhs rhs, { operator = op })

let send_unary op arg =
  (combine op arg,  {
    dot      = None;
    selector = Some op;
    lparen   = None;
    rparen   = None;
  })

let send_binary lhs op rhs =
  (combine lhs rhs,  {
    dot      = None;
    selector = Some op;
    lparen   = None;
    rparen   = None;
  })

let send_method recv dot name lparen rparen =
  (combine recv rparen, {
    dot      = Some dot;
    selector = Some name;
    lparen   = Some lparen;
    rparen   = Some rparen;
  })

let send_attr recv dot name =
  (combine recv name, {
    dot      = Some dot;
    selector = Some name;
    lparen   = None;
    rparen   = None;
  })

let send_call recv lparen rparen =
  (combine recv rparen, {
    dot      = None;
    selector = None;
    lparen   = Some lparen;
    rparen   = Some rparen;
  })

let let_bind mut rename ident =
  let left =  match mut with
              Some x -> x | None ->
                match rename with
                Some x -> x | None -> ident
  in (combine left ident, {
    mut        = mut;
    rename     = rename;
    identifier = ident;
  })
