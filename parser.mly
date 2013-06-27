(* Values *)
%token <Location.t * int>          Vl_INT
%token <Location.t * string>       Vl_SYMBOL
%token <Location.t * Syntax.quote> Vl_BEGIN
%token <Location.t * string>       Vl_STRING
%token <Location.t>                Vl_UNQUOTE Vl_QUOTE Vl_END

(* Identifiers *)
%token <Location.t * string> Id_LOCAL Id_CONST Id_LABEL
%token <Location.t * string> Id_IVAR Id_TVAR

(* Punctuation *)
%token <Location.t> Tk_LPAREN Tk_RPAREN Tk_LCURLY Tk_RCURLY Tk_LBRACK Tk_RBRACK
%token <Location.t> Tk_ASGN Tk_ARROW Tk_ROCKET
%token <Location.t> Tk_DOT Tk_COLON Tk_DCOLON Tk_COMMA Tk_SEMI

(* Operators *)
%token <Location.t * string> Tk_OP_ASGN
%token <Location.t * string> Tk_PLUS Tk_MINUS Tk_STAR Tk_DSTAR Tk_DIVIDE Tk_PERCENT
%token <Location.t * string> Tk_AMPER Tk_PIPE Tk_LSHFT Tk_RSHFT Tk_ARSHFT Tk_TILDE
%token <Location.t * string> Tk_EQ Tk_LT Tk_GT Tk_LEQ Tk_GEQ Tk_CMP
%token <Location.t * string> Tk_UPLUS Tk_UMINUS Tk_UTILDE

(* Keywords *)
%token <Location.t * string> Kw_TRUE Kw_FALSE Kw_NIL Kw_SELF Kw_AND Kw_OR Kw_NOT
%token <Location.t * string> Kw_LET Kw_MUT Kw_DYNAMIC
%token <Location.t * string> Kw_IF Kw_THEN Kw_ELSE Kw_END
%token <Location.t * string> Kw_MODULE Kw_CLASS Kw_MIXIN Kw_IFACE Kw_DEF

%token EOF

%{
  let nullary =
    Location.nullary
  let unary =
    Location.unary
  let binary =
    Location.binary

  let op_unary op arg =
    Location.unary op (Syntax.loc arg)
  let op_binary lhs op rhs =
    Location.binary (Syntax.loc lhs) op (Syntax.loc rhs)

  let collection =
    Location.collection

  let send_unary op arg =
    Location.send_unary op (Syntax.loc arg)
  let send_binary lhs op rhs =
    Location.send_binary (Syntax.loc lhs) op (Syntax.loc rhs)
  let send_method recv dot name lparen rparen =
    Location.send_method (Syntax.loc recv) dot name lparen rparen
  let send_attr recv dot name =
    Location.send_attr (Syntax.loc recv) dot name
  let send_call recv lparen rparen =
    Location.send_call (Syntax.loc recv) lparen rparen

  let let_bind =
    Location.let_bind

  let arg expr =
    [Syntax.ActualArg (nullary (Syntax.loc expr), expr)]
%}

%right    Tk_ASGN Tk_OP_ASGN
%left     Kw_OR
%left     Kw_AND
%right    Kw_NOT
%nonassoc Tk_EQ Tk_LT Tk_LEQ Tk_GT Tk_GEQ Tk_CMP
%left     Tk_PLUS Tk_MINUS
%left     Tk_STAR Tk_DIVIDE Tk_PERCENT
%left     Tk_DSTAR
%left     Tk_LSHFT Tk_RSHFT Tk_ARSHFT
%left     Tk_AMPER Tk_PIPE
%right    Tk_TILDE Tk_UPLUS Tk_UMINUS
%nonassoc Tk_LBRACK Tk_DOT

%start <Syntax.expr> compstmt

%%
    compstmt: stmt=stmt EOF
              { stmt }
            | stmt=stmt Tk_SEMI
              { stmt }

         arg: expr=expr
              { Syntax.ActualArg (nullary (Syntax.loc expr), expr) }
            | op=Tk_STAR expr=expr
              { Syntax.ActualSplat (op_unary (fst op) expr, expr) }

            | id=Id_LABEL expr=expr
              { let (label_loc, label) = id in
                  Syntax.ActualKwArg (op_unary label_loc expr, label, expr) }
            | op=Tk_DSTAR expr=expr
              { Syntax.ActualKwSplat (op_unary (fst op) expr, expr) }

        args: args=separated_list(Tk_COMMA, arg)
              { args }

  tuple_elem: expr=expr
              { Syntax.TupleElem (nullary (Syntax.loc expr), expr) }
            | op=Tk_STAR expr=expr
              { Syntax.TupleSplat (op_unary (fst op) expr, expr) }

 tuple_elems: elems=separated_list(Tk_COMMA, tuple_elem)
              { elems }

 record_elem: id=Id_LABEL expr=expr
              { let (label_loc, label) = id in
                  Syntax.RecordElem (op_unary label_loc expr, label, expr) }
            | lhs=expr tk=Tk_ROCKET rhs=expr
              { Syntax.RecordPair (op_binary lhs tk rhs, lhs, rhs) }
            | op=Tk_DSTAR expr=expr
              { Syntax.RecordSplat (op_unary (fst op) expr, expr) }

record_elems: elems=separated_list(Tk_COMMA, record_elem)
              { elems }

  quote_elem: vl=Vl_STRING
              { let (str_loc, str) = vl in
                  Syntax.QuoteString (nullary str_loc, str) }
            | uq=Vl_UNQUOTE expr=expr qu=Vl_QUOTE
              { Syntax.QuoteSplice (collection uq qu, expr) }

 quote_elems: elems=list(quote_elem)
              { elems }

   let_ident: id=Id_LOCAL
              { let (id_loc, id) = id in
                  Syntax.LetImmutable (let_bind None None id_loc, id) }
            | kw=Kw_MUT id=Id_LOCAL
              { let (kw_loc, _), (id_loc, id) = id, kw in
                  Syntax.LetMutable (let_bind None (Some kw_loc) id_loc, id) }

 let_extract: ident=let_ident
              { match ident with
                | Syntax.LetImmutable(loc, name)
                | Syntax.LetMutable(loc, name) ->
                  Syntax.LetImplicit (nullary (fst loc), name, ident)
                | _ -> assert false }
            | label=Id_LABEL pattern=let_pattern
              { let (label_loc, label) = label in
                  Syntax.LetRename (unary label_loc (Syntax.let_loc pattern),
                                    label, pattern) }

 let_pattern: ident=let_ident
              { ident }
            | lb=Tk_LBRACK elems=separated_list(Tk_COMMA, let_pattern) rb=Tk_RBRACK
              { Syntax.LetTuple (collection lb rb, elems) }
            | lb=Tk_LCURLY elems=separated_list(Tk_COMMA, let_extract) rb=Tk_RCURLY
              { Syntax.LetRecord (collection lb rb, elems) }

        stmt: kw=Kw_LET lhs=let_pattern op=Tk_ASGN rhs=expr
              { Syntax.Let ((binary (Syntax.let_loc lhs) op (Syntax.loc rhs)), lhs, rhs) }
            | expr=expr
              { expr }

     %inline
       binop: t=Tk_PLUS   | t=Tk_MINUS | t=Tk_STAR  | t=Tk_DIVIDE | t=Tk_PERCENT
            | t=Tk_DSTAR  | t=Tk_AMPER | t=Tk_PIPE  | t=Tk_LSHFT  | t=Tk_RSHFT
            | t=Tk_ARSHFT | t=Tk_EQ    | t=Tk_LT    | t=Tk_GT     | t=Tk_LEQ
            | t=Tk_GEQ    | t=Tk_CMP
              { t }

     %inline
        unop: t=Tk_UPLUS | t=Tk_UMINUS | t=Tk_UTILDE
              { t }

 method_name: t=Kw_TRUE  | t=Kw_FALSE | t=Kw_NIL   | t=Kw_SELF | t=Kw_AND
            | t=Kw_OR    | t=Kw_NOT   | t=Kw_LET   | t=Kw_MUT  | t=Kw_DYNAMIC
            | t=Kw_IF    | t=Kw_THEN  | t=Kw_ELSE  | t=Kw_END  | t=Kw_MODULE
            | t=Kw_CLASS | t=Kw_MIXIN | t=Kw_IFACE | t=Kw_DEF
            | t=Id_LOCAL | t=unop     | t=binop
              { t }

        expr: lhs=expr op=Kw_AND rhs=expr
              { let (op_loc, _) = op in Syntax.And (op_binary lhs op_loc rhs, lhs, rhs) }
            | lhs=expr op=Kw_OR  rhs=expr
              { let (op_loc, _) = op in Syntax.Or (op_binary lhs op_loc rhs, lhs, rhs) }
            |          op=Kw_NOT arg=expr
              { let (op_loc, _) = op in Syntax.Not (op_unary op_loc arg, arg) }

            | recv=expr op=Tk_DOT id=method_name lp=Tk_LPAREN args=args rp=Tk_RPAREN
              { let (name_loc, name) = id in
                  Syntax.Send (send_method recv op name_loc lp rp, recv, name, args) }
            | recv=expr op=Tk_DOT id=method_name
              { let (name_loc, name) = id in
                  Syntax.Send (send_attr recv op name_loc, recv, name, []) }
            | recv=expr lb=Tk_LBRACK args=args rb=Tk_RBRACK
              { Syntax.Send (send_call recv lb rb, recv, "[]", args) }

            | lhs=expr op=binop rhs=expr
              { let (name_loc, name) = op in
                  Syntax.Send (send_binary lhs name_loc rhs, lhs, name, arg rhs) }

            | op=Tk_PLUS  recv=expr %prec Tk_UPLUS
            | op=Tk_MINUS recv=expr %prec Tk_UMINUS
            | op=Tk_TILDE recv=expr
              { let (name_loc, name) = op in
                  Syntax.Send (send_unary name_loc recv, recv, name ^ "@", []) }

            | p=primary
              { p }

     primary: kw=Kw_SELF
              { let (loc, _) = kw in Syntax.Self (nullary loc) }
            | kw=Kw_TRUE
              { let (loc, _) = kw in Syntax.Self (nullary loc) }
            | kw=Kw_FALSE
              { let (loc, _) = kw in Syntax.Self (nullary loc) }
            | kw=Kw_NIL
              { let (loc, _) = kw in Syntax.Self (nullary loc) }

            | vl=Vl_INT
              { let (loc, num) = vl in Syntax.Int ((nullary loc), num) }
            | vl=Vl_SYMBOL
              { let (loc, sym) = vl in Syntax.Sym ((nullary loc), sym) }

            | id=Id_LOCAL
              { let (loc, name) = id in Syntax.Var ((nullary loc), name) }
            | id=Id_LOCAL lp=Tk_LPAREN args=args rp=Tk_RPAREN
              { let (name_loc, name) = id in
                  let recv = Syntax.Var ((nullary name_loc), name) in
                    Syntax.Send ((send_call recv lp rp), recv, "call", args) }

            | id=Id_TVAR
              { let (loc, name) = id in Syntax.TVar ((nullary loc), name) }
            | id=Id_IVAR
              { let (loc, name) = id in Syntax.IVar ((nullary loc), name) }
            | id=Id_CONST
              { let (loc, name) = id in Syntax.Const ((nullary loc), name) }

            | lb=Tk_LBRACK elems=tuple_elems  rb=Tk_RBRACK
              { Syntax.Tuple ((collection lb rb), elems) }
            | lc=Tk_LCURLY elems=record_elems rc=Tk_RCURLY
              { Syntax.Record ((collection lc rc), elems) }
            | lq=Vl_BEGIN elems=quote_elems rq=Vl_END
              { let (lq_loc, kind) = lq in
                  Syntax.Quote ((collection lq_loc rq), kind, elems) }

            | lp=Tk_LPAREN expr=expr rp=Tk_RPAREN
              { Syntax.Begin ((collection lp rp), [expr]) }
