type general_category =
  | Cc
  | Cf
  | Co
  | Cs
  | Ll
  | Lm
  | Lo
  | Lt
  | Lu
  | Mc
  | Me
  | Mn
  | Nd
  | Nl
  | No
  | Pc
  | Pd
  | Pe
  | Pf
  | Pi
  | Po
  | Ps
  | Sc
  | Sk
  | Sm
  | So
  | Zl
  | Zp
  | Zs


type bidirectional_category =
  | AL
  | AN
  | B
  | BN
  | CS
  | EN
  | ES
  | ET
  | L
  | LRE
  | LRO
  | NSM
  | ON
  | PDF
  | R
  | RLE
  | RLO
  | S
  | WS


type character_decomposition_mapping =
  | Standard
  | Font
  | NoBreak
  | Initial
  | Medial
  | Final
  | Isolated
  | Circle
  | Super
  | Sub
  | Vertical
  | Wide
  | Narrow
  | Small
  | Square
  | Fraction
  | Compat


type canonical_combining_class = int
type decimal_digit_value = string option
type digit_value = string option
type numeric_value = string option
type unicode_1_0_name = string option
type case_mapping = int option

type t = {
  code : int; (* codepoint *)
  name : string option; (* character_name *)
  gc : general_category;
  ccc : canonical_combining_class;
  bc : bidirectional_category;
  cdm : (character_decomposition_mapping * int list) option;
  dec : decimal_digit_value;
  dig : digit_value;
  num : numeric_value;
  mirrored : bool;
  uni10 : unicode_1_0_name;
  comment : string option;
  uc : case_mapping; (* uppercase_mapping *)
  lc : case_mapping; (* lowercase_mapping *)
  tc : case_mapping; (* titlecase_mapping *)
}
