open Unicode.Std
open Rt

type ir_value =
| IRClass   of klass
| IRPackage of package

module ValueIdentity =
struct
  type t = ir_value

  let equal a b =
    match a, b with
    | IRClass(a),   IRClass(b)   -> a == b
    | IRPackage(a), IRPackage(b) -> a == b
    | _, _ -> false

  let hash = Hashtbl.hash
end

module ValueEnvironment = IrEnvironment.Make(ValueIdentity)

(* Strings and identifiers *)

let print_string str =
  "\"" ^ (IrSupport.escaped str) ^ "\""

let print_ident str =
  if IrSupport.is_printable str
  then str
  else print_string str

(* Names *)

type name =
| Global of string
| Local  of string

let print_name name =
  match name with
  | Global name -> "@" ^ (print_ident name)
  | Local  name -> "%" ^ (print_ident name)

let mangle_name ?(prefix="") ?(suffix="") name =
  match name with
  | Global name -> Global (prefix ^ name ^ suffix)
  | Local  name -> Local  (prefix ^ name ^ suffix)

(* Environments *)

type env = {
         global : ValueEnvironment.t;
          local : ValueEnvironment.t;
  mutable image : string
}

let create_env () =
  {
    global = ValueEnvironment.create ();
    local  = ValueEnvironment.create ();
    image  = "";
  }

let bind env value name =
  let ir_bind env name =
    ValueEnvironment.bind env value name
  in
  match name with
  | Global name -> Global (ir_bind env.global name)
  | Local  name -> Local  (ir_bind env.local name)

(* Printer *)

let with_lookup env value name printer =
  match ValueEnvironment.get env.global value with
  | Some name
  -> Global name
  | None
  -> (let name   = bind env value name in
      let assign = (print_name name) ^ " = " ^ (printer ()) in
        env.image <- env.image ^ assign ^ "\n";
        name
      )

let rec print_klass env klass =
  with_lookup env (IRClass klass) (Global ("c." ^ klass.k_name))
    (fun () ->
      "class " ^ (print_string klass.k_name) ^ " {\n" ^
        "  metaclass " ^ (print_name (print_klass env klass.k_metaclass)) ^ "\n" ^
        (Option.map_default (fun klass ->
            "  ancestor " ^ (print_name (print_klass env klass)) ^ "\n")
          "" klass.k_ancestor) ^
        "  type_variables {}\n" ^
        "  instance_variables {}\n" ^
        "  methods {}\n" ^
      "}")

let print_package env package =
  with_lookup env (IRPackage package) (Global ("p." ^ package.p_name))
    (fun () ->
      "package " ^ (print_string package.p_name) ^ " {\n" ^
        "  metaclass " ^ (print_name (print_klass env package.p_metaclass)) ^ "\n" ^
        "  constants {}\n" ^
      "}")

let print_roots roots =
  let env = create_env () in
    List.iter (fun klass -> ignore (print_klass env klass)) [
      roots.kClass;
      roots.kTypeVariable;
      roots.kNil;
      roots.kBoolean;
      roots.kInteger;
      roots.kSymbol;
      roots.kTuple;
      roots.kRecord;
      roots.kLambda;
      roots.kMixin;
      roots.kPackage
    ];
    print_package env roots.pToplevel;
    env.image
