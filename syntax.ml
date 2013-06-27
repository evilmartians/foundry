open Sexplib.Std

type quote =
  | Qu_STRING
  | Qu_SYMBOL
with sexp_of

type actual_arg =
  | ActualArg     of Location.nullary    * expr
  | ActualSplat   of Location.operator   * expr
  | ActualKwArg   of Location.operator   * string * expr
  | ActualKwSplat of Location.operator   * expr
and tuple_elem =
  | TupleElem     of Location.nullary    * expr
  | TupleSplat    of Location.operator   * expr
and record_elem =
  | RecordElem    of Location.operator   * string * expr
  | RecordSplat   of Location.operator   * expr
  | RecordPair    of Location.operator   * expr * expr
and quote_elem =
  | QuoteString   of Location.nullary    * string
  | QuoteSplice   of Location.collection * expr
and let_pattern =
  | LetImmutable  of Location.let_bind   * string
  | LetMutable    of Location.let_bind   * string
  | LetTuple      of Location.collection * let_pattern list
  | LetRecord     of Location.collection * let_extract list
and let_extract =
  | LetImplicit   of Location.nullary    * string * let_pattern
  | LetRename     of Location.operator   * string * let_pattern
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
  | Let           of Location.operator   * let_pattern * expr
  | Begin         of Location.collection * expr list
with sexp_of

let loc expr =
  match expr with
  | Self (loc)    | Truth (loc) | Lies (loc)   | Nil (loc)
  | Int (loc,_)   | Var (loc,_) | TVar (loc,_) | IVar (loc,_)
  | Const (loc,_) | Sym(loc,_) -> fst loc
  | And (loc,_,_) | Or (loc,_,_) | Not (loc,_)
  | Let(loc,_,_) -> fst loc
  | Send (loc,_,_,_) -> fst loc
  | Tuple(loc,_) | Record(loc,_) | Begin (loc,_)
  | Quote(loc,_,_) -> fst loc

let let_loc let_pattern =
  match let_pattern with
  | LetImmutable (loc,_) | LetMutable(loc,_) -> fst loc
  | LetRecord (loc,_) | LetTuple (loc,_) -> fst loc
