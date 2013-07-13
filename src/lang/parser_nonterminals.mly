%{
  open Unicode.Std

  (* Location tracking *)

  module Loc = Location

  let nullary token =
    (token, ())

  let unary op arg =
    (Loc.join op arg,  { Syntax.operator = op })

  let binary lhs op rhs =
    (Loc.join lhs rhs, { Syntax.operator = op })

  let op_unary op arg =
    unary op (Syntax.loc arg)

  let op_binary lhs op rhs =
    binary (Syntax.loc lhs) op (Syntax.loc rhs)

  let pat_unary op arg =
    unary op (Syntax.pat_loc arg)

  let collection start finish =
    (Loc.join start finish, {
      Syntax.start  = start;
      Syntax.finish = finish;
    })

  let lambda start finish body =
    (Loc.join start (Syntax.loc body), {
      Syntax.start  = start;
      Syntax.finish = finish;
    })

  let send_unary op arg =
    (Loc.join op (Syntax.loc arg),  {
      Syntax.dot      = Loc.empty;
      Syntax.selector = op;
      Syntax.lparen   = Loc.empty;
      Syntax.rparen   = Loc.empty;
    })

  let send_binary lhs op rhs =
    (Loc.join (Syntax.loc lhs) (Syntax.loc rhs),  {
      Syntax.dot      = Loc.empty;
      Syntax.selector = op;
      Syntax.lparen   = Loc.empty;
      Syntax.rparen   = Loc.empty;
    })

  let send_method recv dot name lparen rparen =
    (Loc.join (Syntax.loc recv) rparen, {
      Syntax.dot      = dot;
      Syntax.selector = name;
      Syntax.lparen   = lparen;
      Syntax.rparen   = rparen;
    })

  let send_attr recv dot name =
    (Loc.join (Syntax.loc recv) name, {
      Syntax.dot      = dot;
      Syntax.selector = name;
      Syntax.lparen   = Loc.empty;
      Syntax.rparen   = Loc.empty;
    })

  let send_call recv lparen rparen =
    (Loc.join (Syntax.loc recv) rparen, {
      Syntax.dot      = Loc.empty;
      Syntax.selector = Loc.empty;
      Syntax.lparen   = lparen;
      Syntax.rparen   = rparen;
    })

  let let_bind mut rename ident =
    let left =
      Loc.find [mut; rename; ident]
    in (Loc.join left ident, {
      Syntax.mut        = mut;
      Syntax.rename     = rename;
      Syntax.identifier = ident;
    })

  let arg expr =
    [Syntax.ActualArg (nullary (Syntax.loc expr), expr)]
%}

%left     Kw_OR
%left     Kw_AND
%right    Kw_NOT
%nonassoc Tk_EQ Tk_LT Tk_LEQ Tk_GT Tk_GEQ Tk_CMP
%left     Tk_PLUS Tk_MINUS
%left     Tk_STAR Tk_DIVIDE Tk_PERCENT
%left     Tk_DSTAR
%left     Tk_LSHFT Tk_RSHFT Tk_ARSHFT
%left     Tk_AMPER Tk_PIPE Tk_CARET
%right    Tk_TILDE Tk_UPLUS Tk_UMINUS
%nonassoc Tk_LBRACK Tk_DOT

%start <Syntax.expr list> toplevel

%%
           eof: EOF | Tk_DSEMI
                {}

      toplevel: stmts=compstmt eof
                { stmts }

         block: lc=Tk_LCURLY stmts=compstmt rc=Tk_RCURLY
                { Syntax.Begin (collection lc rc, stmts) }
              | lc=Kw_DO     stmts=compstmt rc=Kw_END
                { Syntax.Begin (collection (fst lc) (fst rc), stmts) }

    tuple_elem: expr=expr
                { Syntax.TupleElem (nullary (Syntax.loc expr), expr) }
              | op=Tk_STAR expr=expr
                { Syntax.TupleSplice (op_unary (fst op) expr, expr) }

   tuple_elems: elems=separated_list(Tk_COMMA, tuple_elem)
                { elems }

   record_elem: id=Id_LABEL expr=expr
                { let (label_loc, label) = id in
                    Syntax.RecordElem (op_unary label_loc expr, label, expr) }
              | lhs=expr tk=Tk_ROCKET rhs=expr
                { Syntax.RecordPair (op_binary lhs tk rhs, lhs, rhs) }
              | op=Tk_DSTAR expr=expr
                { Syntax.RecordSplice (op_unary (fst op) expr, expr) }

  record_elems: elems=separated_list(Tk_COMMA, record_elem)
                { elems }

    quote_elem: vl=Vl_STRING
                { let (str_loc, str) = vl in
                    Syntax.QuoteString (nullary str_loc, str) }
              | uq=Vl_UNQUOTE expr=expr qu=Vl_QUOTE
                { Syntax.QuoteSplice (collection uq qu, expr) }

   quote_elems: elems=list(quote_elem)
                { elems }

     pat_ident: id=Id_LOCAL
                { let (id_loc, id) = id in
                    Syntax.PatImmutable (let_bind Loc.empty Loc.empty id_loc, id) }
              | kw=Kw_MUT id=Id_LOCAL
                { let (kw_loc, _), (id_loc, id) = kw, id in
                    Syntax.PatMutable (let_bind Loc.empty kw_loc id_loc, id) }

   pat_extract: ident=pat_ident
                { match ident with
                  | Syntax.PatImmutable(loc, name)
                  | Syntax.PatMutable(loc, name) ->
                    Syntax.PatImplicit (nullary (fst loc), name, ident)
                  | _ -> assert false }
              | label=Id_LABEL pattern=pattern
                { let (label_loc, label) = label in
                    Syntax.PatRename (pat_unary label_loc pattern, label, pattern) }

       pattern: ident=pat_ident
                { ident }
              | lb=Tk_LBRACK elems=separated_list(Tk_COMMA, pattern) rb=Tk_RBRACK
                { Syntax.PatTuple (collection lb rb, elems) }
              | lb=Tk_LCURLY elems=separated_list(Tk_COMMA, pat_extract) rb=Tk_RCURLY
                { Syntax.PatRecord (collection lb rb, elems) }

       %inline
   f_local_arg: id=Id_LOCAL
                { let (name_loc, name) = id in
                    Syntax.FormalArg (nullary name_loc, name) }

       %inline
  f_prefix_arg: op=Tk_STAR id=Id_LOCAL
                { let (name_loc, name) = id in
                    Syntax.FormalRest (unary (fst op) name_loc, name) }
              | id=Id_LABEL default=option(expr)
                { let (label_loc, label) = id in
                    match default with
                    | Some expr
                    -> Syntax.FormalKwOptArg (op_unary label_loc expr, label, expr)
                    | None
                    -> Syntax.FormalKwArg (nullary label_loc, label) }
              | op=Tk_DSTAR id=Id_LOCAL
                { let (name_loc, name) = id in
                    Syntax.FormalKwRest (unary (fst op) name_loc, name) }
              | id=Id_LOCAL op=Tk_ASGN expr=expr
                { let (name_loc, name) = id in
                    Syntax.FormalOptArg (binary name_loc op (Syntax.loc expr), name, expr) }

         f_arg: arg=f_local_arg
              | arg=f_prefix_arg
                { arg }

        f_args: args=separated_list(Tk_COMMA, f_arg)
                { args }

    f_lam_rest: arg=f_arg Tk_COMMA args=f_lam_rest
                { arg :: args }
              | arg=f_arg Tk_COMMA arg2=f_arg
                { [arg; arg2] }
              | /* nothing */
                { [] }

    f_lam_args: lp=Tk_LPAREN self=Kw_SELF Tk_COMMA args=f_args rp=Tk_RPAREN
                { lp, (Syntax.FormalSelf (nullary (fst self))) :: args, rp }
              | lp=Tk_LPAREN self=Kw_SELF rp=Tk_RPAREN
                { lp, [Syntax.FormalSelf (nullary (fst self))], rp }
              | lp=Tk_LPAREN arg=f_local_arg rp=Tk_RPAREN
                { lp, [arg], rp }
              | lp=Tk_LPAREN arg=f_prefix_arg rp=Tk_RPAREN
                { lp, [arg], rp }
              | lp=Tk_LPAREN args=f_lam_rest rp=Tk_RPAREN
                { lp, args, rp }

           arg: expr=expr
                { Syntax.ActualArg (nullary (Syntax.loc expr), expr) }
              | op=Tk_STAR expr=expr
                { Syntax.ActualSplice (op_unary (fst op) expr, expr) }
              | id=Id_LABEL expr=expr
                { let (label_loc, label) = id in
                    Syntax.ActualKwArg (op_unary label_loc expr, label, expr) }
              | op=Tk_DSTAR expr=expr
                { Syntax.ActualKwSplice (op_unary (fst op) expr, expr) }

          args: args=separated_list(Tk_COMMA, arg)
                { args }

       pair_ty: id=Id_LABEL ty=ty
                { let (id_loc, id) = id in
                    (unary id_loc (Syntax.ty_loc ty), id, ty) }

        arg_ty: ty=ty
                { Syntax.TypeArg (nullary (Syntax.ty_loc ty), ty) }
              | id=Id_LABEL ty=ty
                { let (id_loc, id) = id in
                    Syntax.TypeArgKw (unary id_loc (Syntax.ty_loc ty), id, ty) }

     splice_ty: id=Id_LOCAL
                { let (id_loc, id) = id in
                    Syntax.Var (nullary id_loc, id) }
              | lit=literal
                { lit }

            ty: id=Id_CONST
                { let (id_loc, id) = id in
                    Syntax.TypeConstr (unary id_loc id_loc, id, []) }
              | id=Id_CONST lp=Tk_LPAREN args_ty=separated_list(Tk_COMMA, pair_ty) rp=Tk_RPAREN
                { let (id_loc, id) = id in
                    Syntax.TypeConstr (unary id_loc rp, id, args_ty) }
              | expr=splice_ty
                { Syntax.TypeSplice (nullary (Syntax.loc expr), expr) }
              | id=Id_TVAR
                { let (id_loc, id) = id in
                    Syntax.TypeVar (nullary id_loc, id) }
              | lb=Tk_LBRACK elems_ty=separated_list(Tk_COMMA, ty) rb=Tk_RBRACK
                { Syntax.TypeTuple (collection lb rb, elems_ty) }
              | lc=Tk_LCURLY elems_ty=separated_list(Tk_COMMA, pair_ty) rc=Tk_RCURLY
                { Syntax.TypeRecord (collection lc rc, elems_ty) }
              | lp=Tk_LPAREN args_ty=separated_list(Tk_COMMA, arg_ty) rp=Tk_RPAREN
                  op=Tk_ARROW return_ty=ty
                { Syntax.TypeFunction (collection lp rp, args_ty, return_ty) }

       ty_decl: op=Tk_COLON ty=ty
                { ty }

       %inline
         binop: t=Tk_PLUS   | t=Tk_MINUS | t=Tk_STAR  | t=Tk_DIVIDE | t=Tk_PERCENT
              | t=Tk_DSTAR  | t=Tk_AMPER | t=Tk_PIPE  | t=Tk_LSHFT  | t=Tk_RSHFT
              | t=Tk_ARSHFT | t=Tk_EQ    | t=Tk_LT    | t=Tk_GT     | t=Tk_LEQ
              | t=Tk_GEQ    | t=Tk_CMP   | t=Tk_CARET
                { t }

       %inline
          unop: t=Tk_UPLUS | t=Tk_UMINUS | t=Tk_UTILDE
                { t }

   method_name: t=Kw_TRUE  | t=Kw_FALSE | t=Kw_NIL    | t=Kw_SELF   | t=Kw_AND
              | t=Kw_OR    | t=Kw_NOT   | t=Kw_LET    | t=Kw_MUT    | t=Kw_DYNAMIC
              | t=Kw_IF    | t=Kw_THEN  | t=Kw_ELSE   | t=Kw_END    | t=Kw_PACKAGE
              | t=Kw_CLASS | t=Kw_MIXIN | t=Kw_IFACE  | t=Kw_DEF    | t=Kw_PUBLIC
              | t=Kw_DO    | t=Kw_WHILE | t=Kw_AS     | t=Kw_RETURN | t=Kw_ELSIF
              | t=Kw_MATCH | t=Kw_META  | t=Kw_INVOKE | t=Kw_NEW
              | t=Id_LOCAL | t=unop     | t=binop
                { t }

       %inline
         local: id=Id_LOCAL
                { let (loc, name) = id in Syntax.Var (nullary loc, name) }
              | kw=Kw_SELF
                { let (loc, _) = kw in Syntax.Self (nullary loc) }

 compstmt_noid: stmt=stmt_noid Tk_SEMI stmts=compstmt
                { stmt :: stmts }
              | stmt=stmt_noid
                { [stmt] }

      compstmt: stmt=stmt Tk_SEMI stmts=compstmt
                { stmt :: stmts }
              | stmt=stmt
                { [stmt] }
              | /* nothing */
                { [] }

      ancestor: tk=Tk_LT expr=expr Tk_SEMI
                { Some (fst tk, expr)}
              | Tk_SEMI
                { None }

          stmt: stmt=stmt_noid
                { stmt }
              | lhs=lhs op=Tk_ASGN rhs=stmt
                { Syntax.Assign (op_binary lhs op rhs, lhs, rhs) }
              | lhs=lhs op=Tk_OP_ASGN rhs=stmt
                { let (op_loc, op) = op in
                    Syntax.OpAssign (op_binary lhs op_loc rhs, lhs, op, rhs) }
              | lhs=lhs op=Tk_OR_ASGN rhs=stmt
                { Syntax.OrAssign (op_binary lhs op rhs, lhs, rhs) }
              | lhs=lhs op=Tk_AND_ASGN rhs=stmt
                { Syntax.AndAssign (op_binary lhs op rhs, lhs, rhs) }
              | local=local
                { local }

     stmt_noid: kw=Kw_LET lhs=pattern ty=option(ty_decl) op=Tk_ASGN rhs=expr
                { Syntax.Let (binary (Syntax.pat_loc lhs) op (Syntax.loc rhs),
                              lhs, ty, rhs) }
                /* TODO refactor */
              | kw=Kw_CLASS id=Id_CONST anc=ancestor stmts=compstmt Kw_END
                { let anc_loc, anc = Option.map fst anc, Option.map snd anc in
                    Syntax.Class (nullary (fst id),
                                  snd id, anc, stmts) }
                /* TODO refactor */
              | kw=Kw_DEF id=method_name
                  lp=Tk_LPAREN args=f_args rp=Tk_RPAREN
                  ty=option(ty_decl) Tk_SEMI stmts=compstmt Kw_END
                { let self = Syntax.FormalSelf (nullary Loc.empty) in
                    Syntax.DefMethod (nullary (fst id),
                                      (snd id), self :: args, ty, stmts) }
              | kw=Kw_DEF self=Kw_SELF dot=Tk_DOT id=method_name
                  lp=Tk_LPAREN args=f_args rp=Tk_RPAREN
                  ty=option(ty_decl) Tk_SEMI stmts=compstmt Kw_END
                { let self = Syntax.FormalSelf (nullary Loc.empty) in
                    Syntax.DefSelfMethod (nullary (fst id),
                                      (snd id), self :: args, ty, stmts) }
                /* TODO refactor */
              | kw=Kw_DEF id=Id_IVAR ty=ty_decl
                { Syntax.DefIVar (nullary (fst id),
                                  (snd id), Syntax.IVarImmutable, ty) }
              | kw=Kw_DEF Kw_MUT id=Id_IVAR ty=ty_decl
                { Syntax.DefIVar (nullary (fst id),
                                  (snd id), Syntax.IVarMutable, ty) }
              | kw=Kw_DEF Kw_META id=Id_IVAR ty=ty_decl
                { Syntax.DefIVar (nullary (fst id),
                                  (snd id), Syntax.IVarMetaMutable, ty) }
              | expr=expr_noid
                { expr }

           lhs: id=Id_LOCAL
                { let (loc, name) = id in Syntax.Var (nullary loc, name) }
              | id=Id_IVAR
                { let (loc, name) = id in Syntax.IVar (nullary loc, name) }
              | id=Id_CONST
                { let (loc, name) = id in Syntax.Const (nullary loc, name) }

              | recv=expr op=Tk_DOT id=method_name
                { let (name_loc, name) = id in
                    Syntax.Send (send_attr recv op name_loc,
                                 recv, name, []) }

              | recv=expr lb=Tk_LBRACK args=args rb=Tk_RBRACK
                { Syntax.Send (send_call recv lb rb,
                               recv, u"[]", args) }

          expr: expr=expr_noid
                { expr }
              | local=local
                { local }

     expr_noid: lhs=expr op=Kw_AND rhs=expr
                { let (op_loc, _) = op in
                    Syntax.And (op_binary lhs op_loc rhs, lhs, rhs) }

              | lhs=expr op=Kw_OR  rhs=expr
                { let (op_loc, _) = op in
                    Syntax.Or (op_binary lhs op_loc rhs, lhs, rhs) }

              | op=Kw_NOT arg=expr
                { let (op_loc, _) = op in
                    Syntax.Not (op_unary op_loc arg, arg) }

              | recv=expr op=Tk_DOT id=method_name lp=Tk_LPAREN args=args rp=Tk_RPAREN
                { let (name_loc, name) = id in
                    Syntax.Send (send_method recv op name_loc lp rp,
                                 recv, name, args) }

              | recv=expr op=Tk_DOT id=method_name
                { let (name_loc, name) = id in
                    Syntax.Send (send_attr recv op name_loc,
                                 recv, name, []) }

              | recv=expr lb=Tk_LBRACK args=args rb=Tk_RBRACK
                { Syntax.Send (send_call recv lb rb,
                               recv, u"[]", args) }

              | lhs=expr op=binop rhs=expr
                { let (name_loc, name) = op in
                    Syntax.Send (send_binary lhs name_loc rhs,
                                 lhs, name, arg rhs) }

              | op=Tk_PLUS  recv=expr %prec Tk_UPLUS
              | op=Tk_MINUS recv=expr %prec Tk_UMINUS
              | op=Tk_TILDE recv=expr
                { let (name_loc, name) = op in
                    Syntax.Send (send_unary name_loc recv,
                                 recv, name ^ u"@", []) }

              | kw=Kw_INVOKE id=Id_LOCAL
                    lp=Tk_LPAREN args=separated_list(Tk_COMMA, expr) rp=Tk_RPAREN
                { let (kw_loc,_) = kw in
                  let (_,id) = id in
                    Syntax.InvokePrimitive (nullary (Loc.join kw_loc rp),
                                            id, args) }

              | kw=Kw_TYPE ty=ty
                { let (kw_loc, _) = kw in
                    Syntax.Type (unary kw_loc (Syntax.ty_loc ty), ty) }

              | lam_args=f_lam_args ty=option(ty_decl) block=block
                { let lp, args, rp = lam_args in
                    Syntax.Lambda (lambda lp rp block,
                                   args, ty, block) }

              | prim=primary_noid
                { prim }

    /* primary: prim=primary_noid
                { prim }
              | local=local
                { local } */

  primary_noid: lit=literal
                { lit }

              | id=Id_TVAR
                { let (loc, name) = id in Syntax.TVar (nullary loc, name) }
              | id=Id_IVAR
                { let (loc, name) = id in Syntax.IVar (nullary loc, name) }
              | id=Id_CONST
                { let (loc, name) = id in Syntax.Const (nullary loc, name) }

              | lb=Tk_LBRACK elems=tuple_elems  rb=Tk_RBRACK
                { Syntax.Tuple (collection lb rb, elems) }
              | lc=Tk_LCURLY elems=record_elems rc=Tk_RCURLY
                { Syntax.Record (collection lc rc, elems) }

              | lp=Tk_LPAREN stmts=compstmt_noid rp=Tk_RPAREN
                { Syntax.Begin (collection lp rp, stmts) }
              | lp=Tk_LPAREN local=local rp=Tk_RPAREN
                { Syntax.Begin (collection lp rp, [local])}

       literal: kw=Kw_TRUE
                { let (loc, _) = kw in Syntax.Truth (nullary loc) }
              | kw=Kw_FALSE
                { let (loc, _) = kw in Syntax.Lies (nullary loc) }
              | kw=Kw_NIL
                { let (loc, _) = kw in Syntax.Nil (nullary loc) }

              | vl=Vl_INT
                { let (loc, num) = vl in Syntax.Int (nullary loc, num) }
              | vl=Vl_SYMBOL
                { let (loc, sym) = vl in Syntax.Sym (nullary loc, sym) }
              | lq=Vl_BEGIN elems=quote_elems rq=Vl_END
                { let (lq_loc, kind) = lq in
                    Syntax.Quote (collection lq_loc rq, kind, elems) }
