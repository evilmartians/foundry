open Unicode.Std
open Ssa

type tuple  = Ssa.name
type record = Ssa.name * Ssa.name
type 'a t

type 'a elem =
| Elem   of 'a
| Splice of name

val empty     : 'a t
val append    : 'a t  -> 'a elem -> 'a t

val tup_apply : tuple  t -> (*basic_block*) name -> (*instr*) name
val rec_apply : record t -> (*basic_block*) name -> (*instr*) name
