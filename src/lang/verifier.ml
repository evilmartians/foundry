open Unicode.Std
open Diagnostic
open Syntax

type binding = {
  location    : Location.t;
  kind        : Syntax.lvar_kind;
}

type static_env = {
  parent      : static_env option;
  bindings    : binding Table.t;
}

type context = {
  mutable env : static_env;
}

let (@:) f xs =
  List.concat (List.map f xs)

let ($?) f x =
  Option.map_default f [] x

let rec lookup env name =
  match Table.get env.bindings name with
  | Some binding -> Some binding
  | None -> match env.parent with
            | Some parent -> lookup parent name
            | None -> None

let bind cx (kind, name) ~loc =
  match Table.get cx.env.bindings name with
  | Some binding
  -> [Error, "Local variable `" ^ name ^ "' is already bound.", [loc];
      Note,  "Previous binding here:", [binding.location]]
  | None
  -> Table.set cx.env.bindings name
        { location = loc; kind = kind; };
     []

let rec check_access cx loc name =
  if name = "" then
    [Error, "Variable name `_' is a placeholder and can not be accessed.", [loc]]
  else
    match lookup cx.env name with
    | Some binding -> []
    | None         -> [Error, "Local variable `" ^ name ^ "' is not bound.", [loc]]

and check_assign cx lhs rhs =
  match lhs with
  | Var((loc,_),name)
  -> (let ds =
        match lookup cx.env name with
        | Some binding
        -> (match binding.kind with
            | Syntax.LVarMutable ->
              []
            | Syntax.LVarImmutable ->
              [Error, "Local variable `" ^ name ^ "' is not mutable. " ^
                      "(Try `let mut " ^ name ^ "' instead?)",
                      [loc; binding.location]])
        | None
        -> [Error, "Local variable `" ^ name ^ "' is not declared." ^
                   " (Try `let " ^ name ^ "' instead?)", [loc]]
      in
      ds @ (check_expr cx rhs))
  | IVar _ | Send _ | Const _
  -> check_expr cx @: [lhs; rhs]
  | _
  -> assert false

and check_pattern cx patt =
  match patt with
  | PatVariable((loc, _), lvar)
  -> bind cx lvar loc
  | PatTuple(_, elems)
  -> (let check_elem elem =
        match elem with
        | TupleElem(_, patt) | TupleSplice(_, patt)
        -> check_pattern cx patt
      in
      check_elem @: elems)
  | PatRecord(_, elems)
  -> (let check_elem elem =
        match elem with
        | RecordElem(_, _, patt)
        -> check_pattern cx patt
        | RecordSplice(_, patt)
        -> check_pattern cx patt
        | RecordPair((loc, _), expr, patt)
        -> check_expr cx expr @ check_pattern cx patt
      in
      check_elem @: elems)

and check_ty cx ty =
  match ty with
  | TypeVar(_, _)
  -> []
  | TypeTuple(_, tys)
  -> check_ty cx @: tys
  | TypeRecord(_, pairs)
  -> (fun (_,_,ty) -> check_ty cx ty) @: pairs
  | TypeFunction(_, args, ty)
  -> (let check_arg arg =
        match arg with
        | TypeArg(_, ty) | TypeKwArg(_, _, ty)
        -> check_ty cx ty
      in
      (check_arg @: args) @ check_ty cx ty)
  | TypeConstr((loc, _), name, args)
  -> (if name = "Tuple" then
        [Error, "Use [...] syntax to construct tuple types.", [loc]]
      else if name = "Record" then
        [Error, "Use {...} syntax to construct record types.", [loc]]
      else if name = "Lambda" then
        [Error, "Use (...) -> ... syntax to construct function types.", [loc]]
      else
        let check_arg arg =
          match arg with
          | TypeArg(_, ty) | TypeKwArg(_, _, ty)
          -> check_ty cx ty
        in
        check_arg @: args)
  | TypeSplice(_,expr)
  -> check_expr cx expr

and check_lambda cx f_args ty exprs =
  let cx = {
    env = { parent   = Some cx.env;
            bindings = Table.create [] }
  }
  in
  let rec check_formal_args args ~opt ~rest ~kwrest =
    match args with
    | FormalSelf(_) :: args
    -> check_formal_args args ~opt ~rest ~kwrest

    | FormalArg((loc, _), lvar) :: args
    -> (let ds = bind cx lvar loc in
        ds @ match rest, opt with
        | Some restloc, _
        -> (ignore (check_formal_args args ~opt ~rest ~kwrest);
            [Error, "Regular argument cannot follow a rest argument.",
                    [loc; restloc]])
        | None, Some optloc
        -> (ignore (check_formal_args args ~opt ~rest ~kwrest);
            [Error, "Regular argument cannot follow an optional argument.",
                    [loc; optloc]])
        | None, None
        -> check_formal_args args ~opt ~rest ~kwrest)

    | FormalOptArg((loc, _), lvar, expr) :: args
    -> (let ds = bind cx lvar loc in
        ds @ let ds = check_expr cx expr in
          match rest with
          | None
          -> ds @ check_formal_args args ~opt:(Some loc) ~rest ~kwrest
          | Some restloc
          -> (ignore (check_formal_args args ~opt:(Some loc) ~rest ~kwrest);
              [Error, "Optional argument cannot follow a rest argument.",
                      [loc; restloc]]))

    | FormalRest((loc, _), lvar) :: args
    -> (let ds = bind cx lvar loc in
        ds @ match rest with
        | None
        -> check_formal_args args ~opt ~rest:(Some loc) ~kwrest
        | Some restloc
        -> (ignore (check_formal_args args ~opt ~rest:(Some loc) ~kwrest);
            [Error, "Rest argument can only be specified once.",
                    [loc; restloc]]))

    | FormalKwArg((loc, _), lvar) :: args
    -> (let ds = bind cx lvar loc in
         ds @ match kwrest with
        | None
        -> ds @ check_formal_args args ~opt ~rest ~kwrest
        | Some kwrestloc
        -> (ignore (check_formal_args args ~opt ~rest ~kwrest);
            [Error, "Keyword argument cannot follow a keyword rest argument.",
                    [loc; kwrestloc]]))

    | FormalKwOptArg((loc, _), lvar, expr) :: args
    -> (let ds = bind cx lvar loc in
         ds @ match kwrest with
        | None
        -> ds @ check_formal_args args ~opt ~rest ~kwrest
        | Some kwrestloc
        -> (ignore (check_formal_args args ~opt ~rest ~kwrest);
            [Error, "Optional keyword argument cannot follow a keyword rest argument.",
                    [loc; kwrestloc]]))

    | FormalKwRest((loc, _), lvar) :: args
    -> (let ds = bind cx lvar loc in
        ds @ match kwrest with
        | None
        -> ds @ check_formal_args args ~opt ~rest ~kwrest:(Some loc)
        | Some kwrestloc
        -> (ignore (check_formal_args args ~opt ~rest ~kwrest:(Some loc));
            [Error, "Keyword rest argument can only be specified once.",
                    [loc; kwrestloc]]))

    | [] -> []
  in
  let ds = check_ty cx $? ty in
    let ds = ds @ (check_formal_args f_args ~opt:None ~rest:None ~kwrest:None) in
      ds @ (check_expr cx @: exprs)

and check_expr cx expr =
  match expr with
  | Self _ | Truth _ | Lies _ | Nil _ | Integer _ | Symbol _
  | TVar _ | IVar _ | Const _
  | Unsigned _ | Signed _
  -> []

  | Tuple(_, elems)
  -> (let rec check_elem elem =
        match elem with
        | TupleElem(_,expr) | TupleSplice(_,expr)
        -> check_expr cx expr
      in
      check_elem @: elems)

  | Record(_, elems)
  -> (let rec check_elem elem =
        match elem with
        | RecordElem(_, _, expr) | RecordSplice(_, expr)
        -> check_expr cx expr
        | RecordPair(_, lhs, rhs)
        -> check_expr cx @: [lhs; rhs]
      in
      check_elem @: elems)

  | Quote(_, _, elems)
  -> (let rec check_elem elem =
        match elem with
        | QuoteString(_,_)    -> []
        | QuoteSplice(_,expr) -> check_expr cx expr
      in
      check_elem @: elems)

  | Var((loc, _), name)
  -> check_access cx loc name

  | Assign(_, lhs, rhs) | OpAssign(_, lhs, _, rhs)
  | OrAssign(_, lhs, rhs) | AndAssign(_, lhs, rhs)
  -> check_assign cx lhs rhs

  | And(_, lhs, rhs) | Or(_, lhs, rhs)
  -> check_expr cx @: [lhs; rhs]

  | Not(_, arg)
  -> check_expr cx arg

  | Begin(_, exprs)
  -> check_expr cx @: exprs

  | Let(_, pattern, ty, expr)
  -> (let ds = check_pattern cx pattern in
      ds @ (check_ty cx $? ty) @ check_expr cx expr)

  | Type(_, ty)
  -> check_ty cx ty

  | If(_, cond, exprs, tail)
  -> (let ds = check_expr cx cond in
      let ds = ds @ (check_expr cx @: exprs) in
      ds @ (check_expr cx $? tail))

  | Unless(_, cond, exprs)
  | While(_, cond, exprs)
  | Until(_, cond, exprs)
  -> (let ds = check_expr cx cond in
      ds @ (check_expr cx @: exprs))

  | Send(_, _, _, args)
  | Super(_, args)
  -> (let check_arg arg =
        match arg with
        | ActualArg(_,expr)     | ActualSplice(_,expr)
        | ActualKwArg(_,_,expr) | ActualKwSplice(_,expr)
        -> check_expr cx expr
        | ActualKwPair(_,lhs,rhs)
        -> check_expr cx @: [lhs; rhs]
        | ActualKwPunArg((loc,_),lvar)
        -> check_access cx loc lvar
      in
      let ds =
        match expr with
        | Send(_, recv, _, _) -> check_expr cx recv
        | Super(_, _)         -> []
        | _ -> assert false
      in
      ds @ (check_arg @: args))

  | Class(_, name, params, ancestor, exprs)
  -> (let rec check_params ds seen params =
        match params with
        | FormalTypeArg((loc, _), name) :: params
        | FormalTypeKwArg((_, { operator = loc }), name, _) :: params
        -> (let ds =
              try
                let seen_loc = List.assoc name seen in
                ds @ [Error, "Type variable `" ^ name ^
                             "' is specified more than once.", [loc; seen_loc]]
              with Not_found ->
                ds
            in
            check_params ds ((name, loc) :: seen) params)
        | []
        -> ds
      in
      let ds = check_params [] [] params in
      let ds = ds @ (check_expr cx $? ancestor) in
      ds @ (check_expr cx @: exprs))

  | DefIVar(_, _, _, ty)
  -> check_ty cx ty

  | Lambda(_, f_args, ty, expr)
  -> check_lambda cx f_args ty [expr]

  | DefMethod(_, _, f_args, ty, exprs)
  | DefSelfMethod(_, _, f_args, ty, exprs)
  -> check_lambda cx f_args ty exprs

  | Update(_, expr)
  -> check_expr cx expr

  | InvokePrimitive((loc, _), name, exprs)
  -> (if Primitive.exists name then
        check_expr cx @: exprs
      else
        [Error, "Unknown primitive `" ^ name ^ "'.", [loc]])

let check ast =
  let context = { env = { parent = None; bindings = Table.create [] } } in
  check_expr context @: ast
