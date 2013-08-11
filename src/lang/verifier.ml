open Unicode.Std
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

let rec check_pattern cx pat =
  let bind (kind, name) loc =
    match lookup cx.env name with
    | Some binding
    -> ["Local variable `" ^ name ^ "' is already bound.",
        [loc; binding.location]]
    | None
    -> Table.set cx.env.bindings name
          { location = loc; kind = kind; };
       []
  in
  match pat with
  | PatVariable((loc,_),lvar)
  -> bind lvar loc
  | PatTuple(_,pats)
  -> check_pattern cx @: pats
  | PatRecord(_,pats)
  -> (fun ((loc,_),lvar,pat) ->
        let ds = bind lvar loc in
          ds @ check_pattern cx pat) @: pats

and check_ty cx ty =
  match ty with
  | TypeVar(_,_)
  -> []
  | TypeTuple(_,tys)
  -> check_ty cx @: tys
  | TypeRecord(_,pairs)
  -> (fun (_,_,ty) -> check_ty cx ty) @: pairs
  | TypeFunction(_,args,ty)
  -> (let check_arg arg =
        match arg with
        | TypeArg(_,ty) | TypeArgKw(_,_,ty) -> check_ty cx ty
      in (check_arg @: args) @ check_ty cx ty)
  | TypeConstr((loc,_),name,pairs)
  -> (if name = "Tuple" then
        ["Use [...] syntax to construct tuple types.", [loc]]
      else if name = "Record" then
        ["Use {...} syntax to construct record types.", [loc]]
      else if name = "Lambda" then
        ["Use (...) -> ... syntax to construct function types.", [loc]]
      else
        (fun (_,_,ty) -> check_ty cx ty) @: pairs)
  | TypeSplice(_,expr)
  -> check_expr cx expr

and check_assign cx lhs rhs =
  match lhs with
  | Var((loc,_),name)
  -> (match lookup cx.env name with
      | Some binding
      -> (match binding.kind with
          | Syntax.LVarMutable ->
            []
          | Syntax.LVarImmutable ->
            ["Local variable `" ^ name ^ "' is not mutable. " ^
             "(Try `let mut " ^ name ^ "' instead?)",
             [loc; binding.location]])
      | None
      -> ["Local variable `" ^ name ^ "' is not declared." ^
          " (Try `let " ^ name ^ "' instead?)", [loc]])
  | IVar _ | Send _
  -> []
  | _ -> assert false

and check_lambda cx f_args ty exprs =
  let cx = {
    env = { parent   = Some cx.env;
            bindings = Table.create [] }
  }
  in
  let bind (kind, name) ~loc =
    match lookup cx.env name with
    | Some binding
    -> ["Argument name `" ^ name ^ "' is already bound.",
        [loc; binding.location]]
    | None
    -> Table.set cx.env.bindings name
          { location = loc; kind = kind; };
       []
  in
  let rec check_formal_args args ~opt ~rest ~kwrest =
    match args with
    | FormalSelf(_) :: args
    -> check_formal_args args ~opt ~rest ~kwrest

    | FormalArg((loc,_),lvar) :: args
    -> (let ds = bind lvar ~loc in
        ds @ match rest, opt with
        | Some restloc, _
        -> (ignore (check_formal_args args ~opt ~rest ~kwrest);
            ["Regular argument cannot follow a rest argument.",
             [loc; restloc]])
        | None, Some optloc
        -> (ignore (check_formal_args args ~opt ~rest ~kwrest);
            ["Regular argument cannot follow an optional argument.",
             [loc; optloc]])
        | None, None
        -> check_formal_args args ~opt ~rest ~kwrest)

    | FormalOptArg((loc,_),lvar,expr) :: args
    -> (let ds = bind lvar ~loc in
        ds @ let ds = check_expr cx expr in
          match rest with
          | None
          -> ds @ check_formal_args args ~opt:(Some loc) ~rest ~kwrest
          | Some restloc
          -> (ignore (check_formal_args args ~opt:(Some loc) ~rest ~kwrest);
              ["Optional argument cannot follow a rest argument.",
               [loc; restloc]]))

    | FormalRest((loc,_),lvar) :: args
    -> (let ds = bind lvar ~loc in
        ds @ match rest with
        | None
        -> check_formal_args args ~opt ~rest:(Some loc) ~kwrest
        | Some restloc
        -> (ignore (check_formal_args args ~opt ~rest:(Some loc) ~kwrest);
            ["Rest argument can only be specified once.",
             [loc; restloc]]))

    | FormalKwArg((loc,_),lvar) :: args
    -> (let ds = bind lvar ~loc in
         ds @ match kwrest with
        | None
        -> ds @ check_formal_args args ~opt ~rest ~kwrest
        | Some kwrestloc
        -> (ignore (check_formal_args args ~opt ~rest ~kwrest);
            ["Keyword argument cannot follow a keyword rest argument.",
             [loc; kwrestloc]]))

    | FormalKwOptArg((loc,_),lvar,expr) :: args
    -> (let ds = bind lvar ~loc:loc in
         ds @ match kwrest with
        | None
        -> ds @ check_formal_args args ~opt ~rest ~kwrest
        | Some kwrestloc
        -> (ignore (check_formal_args args ~opt ~rest ~kwrest);
            ["Optional keyword argument cannot follow a keyword rest argument.",
             [loc; kwrestloc]]))

    | FormalKwRest((loc,_),lvar) :: args
    -> (let ds = bind lvar ~loc in
        ds @ match kwrest with
        | None
        -> ds @ check_formal_args args ~opt ~rest ~kwrest:(Some loc)
        | Some kwrestloc
        -> (ignore (check_formal_args args ~opt ~rest ~kwrest:(Some loc));
            ["Keyword rest argument can only be specified once.",
             [loc; kwrestloc]]))

    | [] -> []
  in
  let ds = check_ty cx $? ty in
    let ds = ds @ (check_formal_args f_args ~opt:None ~rest:None ~kwrest:None) in
      ds @ (check_expr cx @: exprs)

and check_expr cx expr =
  match expr with
  | Self _ | Truth _ | Lies _ | Nil _ | Int _ | Sym _
  | TVar _ | IVar _ | Const _
  -> []
  | Tuple(_,elems)
  -> (let rec check_elem elem =
        match elem with
        | TupleElem(_,expr)   -> check_expr cx expr
        | TupleSplice(_,expr) -> check_expr cx expr
      in check_elem @: elems)
  | Record(_,elems)
  -> (let rec check_elem elem =
        match elem with
        | RecordElem(_,_,expr)  -> check_expr cx expr
        | RecordSplice(_,expr)  -> check_expr cx expr
        | RecordPair(_,lhs,rhs) -> check_expr cx @: [lhs; rhs]
      in check_elem @: elems)
  | Quote(_,_,elems)
  -> (let rec check_elem elem =
        match elem with
        | QuoteString(_,_)    -> []
        | QuoteSplice(_,expr) -> check_expr cx expr
      in check_elem @: elems)
  | Var((loc,_),name)
  -> (match lookup cx.env name with
      | Some binding -> []
      | None -> ["Local variable `" ^ name ^ "' is not declared.", [loc]])
  | Assign(_,lhs,rhs) | OpAssign(_,lhs,_,rhs)
  | OrAssign(_,lhs,rhs) | AndAssign(_,lhs,rhs)
  -> check_assign cx lhs rhs
  | And(_,lhs,rhs) | Or(_,lhs,rhs)
  -> check_expr cx @: [lhs; rhs]
  | Not(_,arg)
  -> check_expr cx arg
  | Begin(_,exprs)
  -> check_expr cx @: exprs
  | Let(_,pattern,ty,expr)
  -> let ds = check_pattern cx pattern in
       ds @ (check_ty cx $? ty) @ check_expr cx expr
  | Type(_,ty)
  -> check_ty cx ty
  | Send(_,expr,_,args)
  -> (let check_arg arg =
        match arg with
        | ActualArg(_,expr)     | ActualSplice(_,expr)
        | ActualKwArg(_,_,expr) | ActualKwSplice(_,expr)
        -> check_expr cx expr
        | ActualKwPair(_,lhs,rhs)
        -> check_expr cx @: [lhs; rhs]
      in
      let ds = check_expr cx expr in
        ds @ (check_arg @: args))
  | Class(_,_,ancestor,exprs)
  -> (let ds = check_expr cx $? ancestor in
        ds @ (check_expr cx @: exprs))
  | DefIVar(_,_,_,ty)
  -> check_ty cx ty
  | Lambda(_,f_args,ty,expr)
  -> check_lambda cx f_args ty [expr]
  | DefMethod(_,_,f_args,ty,exprs)
  | DefSelfMethod(_,_,f_args,ty,exprs)
  -> check_lambda cx f_args ty exprs
  | InvokePrimitive((loc,_),name,exprs)
  -> (if Primitive.exists name then
        check_expr cx @: exprs
      else
        ["Unknown primitive `" ^ name ^ "'.", [loc]])

let check ast =
  let context = {
        env = {
          parent = None;
          bindings = Table.create []
        }
      }
  in check_expr context @: ast
