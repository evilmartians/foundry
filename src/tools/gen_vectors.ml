let _ =
  let input_file = ref "" in
  Arg.parse (Arg.align [
      "-input", Arg.Set_string input_file, "Input file"
    ])
    (fun arg -> prerr_endline ("Extraneous argument " ^ arg))
    ("Usage: " ^ Sys.argv.(0) ^ " [options]");

  let vectors =
    let chan = open_in !input_file in
    let rec read_all vectors =
      try
        let line = input_line chan in
        if line.[0] = ';' then read_all vectors
        else read_all ((String.lowercase line) :: vectors)
      with End_of_file -> List.rev vectors
    in
    read_all []
  in

  let code = ref "" in
  let append decl =
    code := !code ^ decl
  in

  List.iter (fun vector ->
      if vector <> "-" then
        append ("declare extern_weak void @_" ^ vector ^ "()\n"))
    vectors;

  append ("\n@__vectors__ = appending global [" ^
          (string_of_int (List.length vectors)) ^
          " x i32*] [\n");

  List.iteri (fun index vector ->
      let comma = if index = (List.length vectors) - 1 then "" else "," in
      if vector = "-" then
        append ("  i32* null" ^ comma ^ " ; reserved\n")
      else
        append ("  i32* bitcast(void()* @_" ^ vector ^ " to i32*)" ^ comma ^ "\n"))
    vectors;

  append ("], section \".vectors\"\n");

  print_string !code
