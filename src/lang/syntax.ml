open Sexplib.Std
open Unicode.Std
open Big_int

(* Enumerations *)

type quote =
| QuoteAsString
| QuoteAsSymbol
with sexp

type lvar_kind =
| LVarImmutable
| LVarMutable
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
| ActualKwPunArg  of nullary    * string
| ActualKwPair    of operator   * expr   * expr
| ActualKwSplice  of operator   * expr
and actual_args = actual_arg list
and local_var   = lvar_kind * string
and formal_arg  =
| FormalSelf      of nullary
| FormalArg       of nullary    * local_var
| FormalOptArg    of operator   * local_var * expr
| FormalRest      of operator   * local_var
| FormalKwArg     of nullary    * local_var
| FormalKwOptArg  of operator   * local_var * expr
| FormalKwRest    of operator   * local_var
and formal_args = formal_arg list
and pair_ty     = operator * string * ty
and arg_ty      =
| TypeArg         of nullary    * ty
| TypeKwArg       of operator   * string * ty
and ty          =
| TypeConstr      of operator   * string * arg_ty list
| TypeTuple       of collection * ty list
| TypeRecord      of collection * pair_ty list
| TypeFunction    of collection * arg_ty list * ty
| TypeVar         of nullary    * string
| TypeSplice      of nullary    * expr
and formal_ty_arg =
| FormalTypeArg   of nullary    * string
| FormalTypeKwArg of operator   * string * string
and formal_ty_args = formal_ty_arg list
and tuple_elem  =
| TupleElem       of nullary    * expr
| TupleSplice     of operator   * expr
and tuple_elems = tuple_elem list
and record_elem =
| RecordElem      of operator   * string * expr
| RecordPunElem   of nullary    * string
| RecordPair      of operator   * expr * expr
| RecordSplice    of operator   * expr
and record_elems = record_elem list
and quote_elem  =
| QuoteString     of nullary    * string
| QuoteSplice     of collection * expr
and quote_elems = quote_elem list
and pattern     =
| PatVariable     of let_bind   * local_var
| PatTuple        of collection * pattern list
| PatRecord       of collection * pat_extract list
and pat_extract = let_bind  * local_var * pattern
and expr        =
| Self            of nullary
| Truth           of nullary (* true  *)
| Lies            of nullary (* false *)
| Nil             of nullary
| Integer         of nullary    * big_int
| Unsigned        of nullary    * int * big_int
| Signed          of nullary    * int * big_int
| Symbol          of nullary    * string
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
| Super           of send       * actual_args
| Lambda          of collection * formal_args * ty option * expr
| Let             of operator   * pattern * ty option * expr
| If              of nullary    * expr * exprs * expr option
| Unless          of nullary    * expr * exprs
| While           of nullary    * expr * exprs
| Until           of nullary    * expr * exprs
| Assign          of operator   * expr * expr
| OrAssign        of operator   * expr * expr
| AndAssign       of operator   * expr * expr
| OpAssign        of operator   * expr * string * expr
| Type            of operator   * ty
| Class           of nullary    * string * formal_ty_args * expr option * exprs
| DefMethod       of nullary    * string * formal_args * ty option * exprs
| DefSelfMethod   of nullary    * string * formal_args * ty option * exprs
| DefIVar         of nullary    * string * ivar_kind * ty
| Update          of operator   * expr
| InvokePrimitive of nullary    * string * expr list
and exprs       = expr list
with sexp

module ExprsIdentity =
struct
  type t = exprs

  let equal = (=)
  let hash  = Hashtbl.hash
end

module Exprstbl = Hashtbl.Make(ExprsIdentity)

let loc expr =
  match expr with
  | Self (loc)    | Truth (loc) | Lies (loc)   | Nil (loc)
  | Integer (loc,_)   | Signed(loc,_,_) | Unsigned(loc,_,_)
  | Var (loc,_)   | TVar (loc,_) | IVar (loc,_)
  | Const (loc,_) | Symbol(loc,_) | Class(loc,_,_,_,_)
  | DefMethod(loc,_,_,_,_) | DefSelfMethod(loc,_,_,_,_)
  | DefIVar(loc,_,_,_) | If(loc,_,_,_) | Unless(loc,_,_)
  | While(loc,_,_) | Until(loc,_,_)
  | InvokePrimitive(loc,_,_) -> fst loc
  | And(loc,_,_) | Or(loc,_,_) | Not(loc,_) | Update(loc,_)
  | Let(loc,_,_,_)  | Assign(loc,_,_) | OpAssign(loc,_,_,_)
  | OrAssign(loc,_,_) | AndAssign(loc,_,_) | Type(loc,_) -> fst loc
  | Send(loc,_,_,_) | Super(loc,_) -> fst loc
  | Tuple(loc,_) | Record(loc,_) | Begin(loc,_)
  | Quote(loc,_,_) | Lambda(loc,_,_,_) -> fst loc

let pat_loc pattern =
  match pattern with
  | PatVariable (loc,_) -> fst loc
  | PatRecord (loc,_) | PatTuple (loc,_) -> fst loc

let ty_loc ty =
  match ty with
  | TypeConstr(loc,_,_) -> fst loc
  | TypeTuple(loc,_) | TypeRecord(loc,_) | TypeFunction(loc,_,_) -> fst loc
  | TypeVar(loc,_) | TypeSplice(loc,_) -> fst loc
