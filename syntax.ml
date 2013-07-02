open Sexplib.Std

type quote =
  | Qu_STRING
  | Qu_SYMBOL
with sexp_of

type actual_arg =
  | ActualArg       of Location.nullary    * expr
  | ActualSplat     of Location.operator   * expr
  | ActualKwArg     of Location.operator   * string * expr
  | ActualKwSplat   of Location.operator   * expr
and formal_arg =
  | FormalSelf      of Location.nullary
  | FormalArg       of Location.nullary    * string
  | FormalSplat     of Location.operator   * string
  | FormalKwArg     of Location.nullary    * string
  | FormalKwOptArg  of Location.operator   * string * expr
  | FormalKwSplat   of Location.operator   * string
and pair_ty =          Location.operator   * string * ty
and arg_ty =
  | TypeArg         of Location.nullary    * ty
  | TypeArgKw       of Location.operator   * string * ty
and ty =
  | TypeConstr      of Location.operator   * string * pair_ty list
  | TypeTuple       of Location.collection * ty list
  | TypeRecord      of Location.collection * pair_ty list
  | TypeFunction    of Location.collection * arg_ty list * ty
  | TypeVar         of Location.nullary    * string
  | TypeSplice      of Location.nullary    * expr
and tuple_elem =
  | TupleElem       of Location.nullary    * expr
  | TupleSplat      of Location.operator   * expr
and record_elem =
  | RecordElem      of Location.operator   * string * expr
  | RecordSplat     of Location.operator   * expr
  | RecordPair      of Location.operator   * expr * expr
and quote_elem =
  | QuoteString     of Location.nullary    * string
  | QuoteSplice     of Location.collection * expr
and pattern =
  | PatImmutable    of Location.let_bind   * string
  | PatMutable      of Location.let_bind   * string
  | PatTuple        of Location.collection * pattern list
  | PatRecord       of Location.collection * pat_extract list
and pat_extract =
  | PatImplicit     of Location.nullary    * string * pattern
  | PatRename       of Location.operator   * string * pattern
and expr =
  | Self          of Location.nullary
  | Truth         of Location.nullary (* true  *)
  | Lies          of Location.nullary (* false *)
  | Nil           of Location.nullary
  | Int           of Location.nullary    * int
  | Sym           of Location.nullary    * string
  | Var           of Location.nullary    * string
  | TVar          of Location.nullary    * string
  | IVar          of Location.nullary    * string
  | Const         of Location.nullary    * string
  | And           of Location.operator   * expr * expr
  | Or            of Location.operator   * expr * expr
  | Not           of Location.operator   * expr
  | Send          of Location.send       * expr * string * actual_arg list
  | Tuple         of Location.collection * tuple_elem list
  | Record        of Location.collection * record_elem list
  | Quote         of Location.collection * quote * quote_elem list
  | Begin         of Location.collection * expr list
  | Lambda        of Location.collection * formal_arg list * ty option * expr
  | Let           of Location.operator   * pattern * ty option * expr
  | Assign        of Location.operator   * expr * expr
  | OrAssign      of Location.operator   * expr * expr
  | AndAssign     of Location.operator   * expr * expr
  | OpAssign      of Location.operator   * expr * string * expr
  | Type          of Location.operator   * ty
with sexp_of

let loc expr =
  match expr with
  | Self (loc)    | Truth (loc) | Lies (loc)   | Nil (loc)
  | Int (loc,_)   | Var (loc,_) | TVar (loc,_) | IVar (loc,_)
  | Const (loc,_) | Sym(loc,_) -> fst loc
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

let type_loc ty =
  match ty with
  | TypeConstr(loc,_,_) -> fst loc
  | TypeTuple(loc,_) | TypeRecord(loc,_) | TypeFunction(loc,_,_) -> fst loc
  | TypeVar(loc,_) | TypeSplice(loc,_) -> fst loc
