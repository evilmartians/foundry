open Unicode.Std
open Syntax

type binding = {
  location   : Location.t;
  is_mutable : bool;
}

type static_env = {
  parent     : static_env option;
  bindings   : binding Table.t;
}

type context = {
  env        : static_env;
}

let conmap f context xs =
  List.concat (List.map (f context) xs)

let is_mutable pat =
  match pat with
  | PatImmutable _ -> false
  | PatMutable _ -> true
  | _ -> assert false

let rec check_pattern cx pat =
  match pat with
  | PatImmutable((loc,_),name) | PatMutable((loc,_),name)
  -> (match Table.get cx.env.bindings name with
      | Some binding
      -> ["Local variable `" ^ name ^ "' is already bound.",
          [loc; binding.location]]
      | None
      -> Table.set cx.env.bindings name
            { location = loc; is_mutable = is_mutable pat; };
         []);
  | PatTuple(_,pats)
  -> conmap check_pattern cx pats

and check_assign cx lhs rhs =
  match lhs with
  | Var((loc,_),name)
  -> (match Table.get cx.env.bindings name with
      | Some binding
      -> (if binding.is_mutable then
            []
          else
            ["Local variable `" ^ name ^ "' is not mutable. " ^
             "(Try `let mut " ^ name ^ "' instead?)",
             [loc; binding.location]])
      | None
      -> ["Local variable `" ^ name ^ "' is not declared." ^
          " (Try `let " ^ name ^ "' instead?)", [loc]])
  | IVar _ | Send _
  -> []
  | _ -> assert false

and check_expr cx expr =
  match expr with
  | Self _ | Truth _ | Lies _ | Nil _ | Int _ | Sym _
  | TVar _ | IVar _ | Const _
  -> []
  | Var((loc,_),name)
  -> ["Local variable `" ^ name ^ "' is not declared.", [loc]]
  | Assign(_,lhs,rhs) | OpAssign(_,lhs,_,rhs)
  | OrAssign(_,lhs,rhs) | AndAssign(_,lhs,rhs)
  -> check_assign cx lhs rhs
  | And(_,lhs,rhs) | Or(_,lhs,rhs)
  -> check_expr cx lhs @ check_expr cx rhs
  | Not(_,arg)
  -> check_expr cx arg
  | Begin(_,exprs)
  -> conmap check_expr cx exprs
  | Let(_,pattern,_,expr)
  -> check_pattern cx pattern @ check_expr cx expr

let check ast =
  let context = {
        env = {
          parent = None;
          bindings = Table.create []
        }
      }
  in conmap check_expr context ast
