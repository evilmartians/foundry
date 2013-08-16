open Codepoint

let string_of_general_category = function
  | Cc -> "Cc"
  | Cf -> "Cf"
  | Co -> "Co"
  | Cs -> "Cs"
  | Ll -> "Ll"
  | Lm -> "Lm"
  | Lo -> "Lo"
  | Lt -> "Lt"
  | Lu -> "Lu"
  | Mc -> "Mc"
  | Me -> "Me"
  | Mn -> "Mn"
  | Nd -> "Nd"
  | Nl -> "Nl"
  | No -> "No"
  | Pc -> "Pc"
  | Pd -> "Pd"
  | Pe -> "Pe"
  | Pf -> "Pf"
  | Pi -> "Pi"
  | Po -> "Po"
  | Ps -> "Ps"
  | Sc -> "Sc"
  | Sk -> "Sk"
  | Sm -> "Sm"
  | So -> "So"
  | Zl -> "Zl"
  | Zp -> "Zp"
  | Zs -> "Zs"


let string_of_bidirectional_category = function
  | AL  -> "AL"
  | AN  -> "AN"
  | B   -> "B"
  | BN  -> "BN"
  | CS  -> "CS"
  | EN  -> "EN"
  | ES  -> "ES"
  | ET  -> "ET"
  | L   -> "L"
  | LRE -> "LRE"
  | LRO -> "LRO"
  | NSM -> "NSM"
  | ON  -> "ON"
  | PDF -> "PDF"
  | R   -> "R"
  | RLE -> "RLE"
  | RLO -> "RLO"
  | S   -> "S"
  | WS  -> "WS"


let rec string_of_int_list = function
  | [] -> ""
  | [hd] -> Printf.sprintf "0x%04x" hd
  | hd :: tl -> Printf.sprintf "0x%04x;%s" hd (string_of_int_list tl)

let string_of_character_decomposition_mapping = function
  | Standard, a -> "Standard, [" ^ string_of_int_list a ^ "]"
  | Font, a -> "Font, [" ^ string_of_int_list a ^ "]"
  | NoBreak, a -> "NoBreak, [" ^ string_of_int_list a ^ "]"
  | Initial, a -> "Initial, [" ^ string_of_int_list a ^ "]"
  | Medial, a -> "Medial, [" ^ string_of_int_list a ^ "]"
  | Final, a -> "Final, [" ^ string_of_int_list a ^ "]"
  | Isolated, a -> "Isolated, [" ^ string_of_int_list a ^ "]"
  | Circle, a -> "Circle, [" ^ string_of_int_list a ^ "]"
  | Super, a -> "Super, [" ^ string_of_int_list a ^ "]"
  | Sub, a -> "Sub, [" ^ string_of_int_list a ^ "]"
  | Vertical, a -> "Vertical, [" ^ string_of_int_list a ^ "]"
  | Wide, a -> "Wide, [" ^ string_of_int_list a ^ "]"
  | Narrow, a -> "Narrow, [" ^ string_of_int_list a ^ "]"
  | Small, a -> "Small, [" ^ string_of_int_list a ^ "]"
  | Square, a -> "Square, [" ^ string_of_int_list a ^ "]"
  | Fraction, a -> "Fraction, [" ^ string_of_int_list a ^ "]"
  | Compat, a -> "Compat, [" ^ string_of_int_list a ^ "]"


let string_of_codepoint =
  let opts = function
    | None -> "None"
    | Some x -> "Some (\"" ^ (String.escaped x) ^ "\")"
  in

  let opti = function
    | None -> "None"
    | Some x -> Printf.sprintf "Some (0x%04x)" x
  in

  let optf f = function
    | None -> "None"
    | Some x -> "Some (" ^ f x ^ ")"
  in

  function
  {
    code = codepoint;
    name = character_name;
    gc = general_category;
    ccc = canonical_combining_classes;
    bc = bidirectional_category;
    cdm = character_decomposition_mapping;
    dec = decimal_digit_value;
    dig = digit_value;
    num = numeric_value;
    mirrored = mirrored;
    uni10 = unicode_1_0_name;
    comment = comment;
    uc = uppercase_mapping;
    lc = lowercase_mapping;
    tc = titlecase_mapping;
  } ->
    Printf.sprintf "{ \
        code = 0x%04x; gc = %s; ccc = %3d; bc = %3s; \
        mirrored = %5s; \
        uc = %s; lc = %s; tc = %s; \
        dec = %s; dig = %s; num = %s; \
        cdm =\t%s; \
        name =\t%s; uni10 =\t%s; comment =\t%s }"
      codepoint
      (string_of_general_category general_category)
      canonical_combining_classes
      (string_of_bidirectional_category bidirectional_category)
      (string_of_bool mirrored)
      (opti uppercase_mapping)
      (opti lowercase_mapping)
      (opti titlecase_mapping)
      (opts decimal_digit_value)
      (opts digit_value)
      (opts numeric_value)
      (optf string_of_character_decomposition_mapping character_decomposition_mapping)
      (opts character_name)
      (opts unicode_1_0_name)
      (opts comment)


let csv_of_codepoint =
  let opts = function
    | None -> ""
    | Some x -> x
  in

  let opti = function
    | None -> ""
    | Some x -> Printf.sprintf "%04x" x
  in

  let optf f = function
    | None -> ""
    | Some x -> f x
  in
  
  function
  {
    code = codepoint;
    name = character_name;
    gc = general_category;
    ccc = canonical_combining_classes;
    bc = bidirectional_category;
    cdm = character_decomposition_mapping;
    dec = decimal_digit_value;
    dig = digit_value;
    num = numeric_value;
    mirrored = mirrored;
    uni10 = unicode_1_0_name;
    comment = comment;
    uc = uppercase_mapping;
    lc = lowercase_mapping;
    tc = titlecase_mapping;
  } ->
    Printf.sprintf "%04x;%s;%s;%d;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s"
      codepoint
      (opts character_name)
      (string_of_general_category general_category)
      canonical_combining_classes
      (string_of_bidirectional_category bidirectional_category)
      (optf string_of_character_decomposition_mapping character_decomposition_mapping)
      (opts decimal_digit_value)
      (opts digit_value)
      (opts numeric_value)
      (string_of_bool mirrored)
      (opts unicode_1_0_name)
      (opts comment)
      (opti uppercase_mapping)
      (opti lowercase_mapping)
      (opti titlecase_mapping)
