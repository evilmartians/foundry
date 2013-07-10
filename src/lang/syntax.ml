open Sexplib.Std
open Unicode.Std

(* Enumerations *)

type quote =
  | Qu_STRING
  | Qu_SYMBOL
with sexp

type ivar_kind =
  | IVarImmutable
  | IVarMutable
  | IVarMetaMutable
with sexp

(* Location records *)

type nullary      = Location.t * unit
with sexp

type collection_i = {
  start           : Location.t;
  finish          : Location.t;
}
and  collection   = Location.t * collection_i
with sexp

type operator_i   = {
  operator        : Location.t;
}
and  operator     = Location.t * operator_i
with sexp

type send_i       = {
  dot             : Location.t;
  selector        : Location.t;
  lparen          : Location.t;
  rparen          : Location.t;
}
and  send         = Location.t * send_i
with sexp

type let_bind_i   = {
  mut             : Location.t;
  rename          : Location.t;
  identifier      : Location.t;
}
and  let_bind     = Location.t * let_bind_i
with sexp

(* AST nodes *)

type actual_arg =
  | ActualArg       of nullary    * expr
  | ActualSplice    of operator   * expr
  | ActualKwArg     of operator   * string * expr
  | ActualKwSplice  of operator   * expr
and actual_args = actual_arg list
and formal_arg =
  | FormalSelf      of nullary
  | FormalArg       of nullary    * string
  | FormalOptArg    of operator   * string * expr
  | FormalRest      of operator   * string
  | FormalKwArg     of nullary    * string
  | FormalKwOptArg  of operator   * string * expr
  | FormalKwRest    of operator   * string
and formal_args = formal_arg list
and pair_ty =          operator   * string * ty
and arg_ty =
  | TypeArg         of nullary    * ty
  | TypeArgKw       of operator   * string * ty
and ty =
  | TypeConstr      of operator   * string * pair_ty list
  | TypeTuple       of collection * ty list
  | TypeRecord      of collection * pair_ty list
  | TypeFunction    of collection * arg_ty list * ty
  | TypeVar         of nullary    * string
  | TypeSplice      of nullary    * expr
and tuple_elem =
  | TupleElem       of nullary    * expr
  | TupleSplice     of operator   * expr
and tuple_elems = tuple_elem list
and record_elem =
  | RecordElem      of operator   * string * expr
  | RecordSplice    of operator   * expr
  | RecordPair      of operator   * expr * expr
and record_elems = record_elem list
and quote_elem =
  | QuoteString     of nullary    * string
  | QuoteSplice     of collection * expr
and quote_elems = quote_elem list
and pattern =
  | PatImmutable    of let_bind   * string
  | PatMutable      of let_bind   * string
  | PatTuple        of collection * pattern list
  | PatRecord       of collection * pat_extract list
and pat_extract =
  | PatImplicit     of nullary    * string * pattern
  | PatRename       of operator   * string * pattern
and expr =
  | Self            of nullary
  | Truth           of nullary (* true  *)
  | Lies            of nullary (* false *)
  | Nil             of nullary
  | Int             of nullary    * int
  | Sym             of nullary    * string
  | Var             of nullary    * string
  | TVar            of nullary    * string
  | IVar            of nullary    * string
  | Const           of nullary    * string
  | And             of operator   * expr * expr
  | Or              of operator   * expr * expr
  | Not             of operator   * expr
  | Tuple           of collection * tuple_elems
  | Record          of collection * record_elems
  | Quote           of collection * quote * quote_elems
  | Begin           of collection * exprs
  | Send            of send       * expr * string * actual_args
  | Lambda          of collection * formal_args * ty option * expr
  | Let             of operator   * pattern * ty option * expr
  | Assign          of operator   * expr * expr
  | OrAssign        of operator   * expr * expr
  | AndAssign       of operator   * expr * expr
  | OpAssign        of operator   * expr * string * expr
  | Type            of operator   * ty
  | Class           of nullary    * string * expr option * exprs
  | DefMethod       of nullary    * string * formal_args * ty option * exprs
  | DefIVar         of nullary    * string * ivar_kind * ty
and exprs = expr list
with sexp

let loc expr =
  match expr with
  | Self (loc)    | Truth (loc) | Lies (loc)   | Nil (loc)
  | Int (loc,_)   | Var (loc,_) | TVar (loc,_) | IVar (loc,_)
  | Const (loc,_) | Sym(loc,_) | Class(loc,_,_,_)
  | DefMethod(loc,_,_,_,_) | DefIVar(loc,_,_,_) -> fst loc
  | And (loc,_,_) | Or (loc,_,_) | Not (loc,_)
  | Let(loc,_,_,_)  | Assign(loc,_,_) | OpAssign(loc,_,_,_)
  | OrAssign(loc,_,_) | AndAssign(loc,_,_) | Type(loc,_) -> fst loc
  | Send (loc,_,_,_) -> fst loc
  | Tuple(loc,_) | Record(loc,_) | Begin (loc,_)
  | Quote(loc,_,_) | Lambda(loc,_,_,_) -> fst loc

let pat_loc pattern =
  match pattern with
  | PatImmutable (loc,_) | PatMutable(loc,_) -> fst loc
  | PatRecord (loc,_) | PatTuple (loc,_) -> fst loc

let ty_loc ty =
  match ty with
  | TypeConstr(loc,_,_) -> fst loc
  | TypeTuple(loc,_) | TypeRecord(loc,_) | TypeFunction(loc,_,_) -> fst loc
  | TypeVar(loc,_) | TypeSplice(loc,_) -> fst loc
