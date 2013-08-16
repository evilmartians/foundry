let (|>) x f = f x


let uniescape s =
  let buf = Buffer.create (String.length s * 4) in
  String.iter (fun c ->
    Printf.bprintf buf "\\x%02x" (int_of_char c)
  ) s;
  Buffer.contents buf


let show func result =
  print_string func;
  print_endline result


let show _ _ = ()


let test () =
  let open Unicode in
  let open Codepoint in
  let s = Unicode.adopt_utf8s "hey 你好ä𤭢 ho" in
  assert ((utf8s_of_utf32s (utf32s_of_utf8s s)) = s);

  print_endline (string_of_utf32s (utf32s_of_utf8s s));

  Array.iter (fun c ->
    if c.code < 0xd800 || c.code > 0xf8ff then (
      let orig = adopt_utf32 c.code in
      let utf32 = orig in
      let utf16 = utf16_of_utf32 utf32 in
      let utf8  = utf8_of_utf32  utf32 in

      show "code: " (Printf.sprintf "%04x" c.code);

      (* lists of characters *)
      let utf32s = utf32s_of_list [utf32; utf32] in
      let utf32 = List.hd (list_of_utf32s utf32s) in
      assert (utf32 = orig);

      let utf16s = utf16s_of_list [utf16; utf16] in
      let utf16 = List.hd (list_of_utf16s utf16s) in
      assert (utf32_of_utf16 utf16 = orig);

      let utf8s = utf8s_of_list [utf8; utf8] in
      let utf8 = List.hd (list_of_utf8s utf8s) in
      assert (utf32_of_utf8 utf8 = orig);

      (* from utf32 *)
      let utf8   =   utf8_of_utf32  utf32  in xstring_of_utf8 utf8 |> show "  utf8_of_utf32  ";
      assert (utf32_of_utf8 utf8 = orig);
      let utf8s  =  utf8s_of_utf32  utf32  in xstring_of_utf8s utf8s |> show " utf8s_of_utf32  ";
      assert (utf32_of_utf8s utf8s = orig);

      let utf16  =  utf16_of_utf32  utf32  in xstring_of_utf16 utf16 |> show " utf16_of_utf32  ";
      assert (utf32_of_utf16 utf16 = orig);
      let utf16s = utf16s_of_utf32  utf32  in xstring_of_utf16s utf16s |> show "utf16s_of_utf32  ";
      assert (utf32_of_utf16s utf16s = orig);

      (*let utf32  =  utf32_of_utf32  utf32  in xstring_of_utf32 utf32 |> show " utf32_of_utf32  ";*)
      (*assert (utf32_of_utf32 utf32 = orig);*)
      let utf32s = utf32s_of_utf32  utf32  in xstring_of_utf32s utf32s |> show "utf32s_of_utf32  ";
      assert (utf32_of_utf32s utf32s = orig);

      (* from utf32s *)
      let utf8   =   utf8_of_utf32s utf32s in xstring_of_utf8 utf8 |> show "  utf8_of_utf32s ";
      assert (utf32_of_utf8 utf8 = orig);
      let utf8s  =  utf8s_of_utf32s utf32s in xstring_of_utf8s utf8s |> show " utf8s_of_utf32s ";
      assert (utf32_of_utf8s utf8s = orig);

      let utf16  =  utf16_of_utf32s utf32s in xstring_of_utf16 utf16 |> show " utf16_of_utf32s ";
      assert (utf32_of_utf16 utf16 = orig);
      let utf16s = utf16s_of_utf32s utf32s in xstring_of_utf16s utf16s |> show "utf16s_of_utf32s ";
      assert (utf32_of_utf16s utf16s = orig);

      let utf32  =  utf32_of_utf32s utf32s in xstring_of_utf32 utf32 |> show " utf32_of_utf32s ";
      (*assert (utf32_of_utf32 utf32 = orig);*)
      (*let utf32s = utf32s_of_utf32utf3utf322s utf32s in xstring_of_utf32s utf32s |> show "utf32s_of_utf32s ";*)
      (*assert (utf32_of_utf32s utf32s = orig);*)

      (* from utf16 *)
      let utf8   =   utf8_of_utf16  utf16  in xstring_of_utf8 utf8 |> show "  utf8_of_utf16  ";
      assert (utf32_of_utf8 utf8 = orig);
      let utf8s  =  utf8s_of_utf16  utf16  in xstring_of_utf8s utf8s |> show " utf8s_of_utf16  ";
      assert (utf32_of_utf8s utf8s = orig);

      (*let utf16  =  utf16_of_utf16  utf16  in xstring_of_utf16 utf16 |> show " utf16_of_utf16  ";*)
      (*assert (utf32_of_utf16 utf16 = orig);*)
      let utf16s = utf16s_of_utf16  utf16  in xstring_of_utf16s utf16s |> show "utf16s_of_utf16  ";
      assert (utf32_of_utf16s utf16s = orig);

      let utf32  =  utf32_of_utf16  utf16  in xstring_of_utf32 utf32 |> show " utf32_of_utf16  ";
      (*assert (utf32_of_utf32 utf32 = orig);*)
      let utf32s = utf32s_of_utf16  utf16  in xstring_of_utf32s utf32s |> show "utf32s_of_utf16  ";
      assert (utf32_of_utf32s utf32s = orig);

      (* from utf16s *)
      let utf8   =   utf8_of_utf16s utf16s in xstring_of_utf8 utf8 |> show "  utf8_of_utf16s ";
      assert (utf32_of_utf8 utf8 = orig);
      let utf8s  =  utf8s_of_utf16s utf16s in xstring_of_utf8s utf8s |> show " utf8s_of_utf16s ";
      assert (utf32_of_utf8s utf8s = orig);

      let utf16  =  utf16_of_utf16s utf16s in xstring_of_utf16 utf16 |> show " utf16_of_utf16s ";
      assert (utf32_of_utf16 utf16 = orig);
      (*let utf16s = utf16s_of_utf16s utf16s in xstring_of_utf16s utf16s |> show "utf16s_of_utf16s ";*)
      (*assert (utf32_of_utf16s utf16s = orig);*)

      let utf32  =  utf32_of_utf16s utf16s in xstring_of_utf32 utf32 |> show " utf32_of_utf16s ";
      (*assert (utf32_of_utf32 utf32 = orig);*)
      let utf32s = utf32s_of_utf16s utf16s in xstring_of_utf32s utf32s |> show "utf32s_of_utf16s ";
      assert (utf32_of_utf32s utf32s = orig);

      (* from utf8 *)
      (*let utf8   =   utf8_of_utf8   utf8   in xstring_of_utf8 utf8 |> show "  utf8_of_utf8   ";*)
      (*assert (utf32_of_utf8 utf8 = orig);*)
      let utf8s  =  utf8s_of_utf8   utf8   in xstring_of_utf8s utf8s |> show " utf8s_of_utf8   ";
      assert (utf32_of_utf8s utf8s = orig);

      let utf16  =  utf16_of_utf8   utf8   in xstring_of_utf16 utf16 |> show " utf16_of_utf8   ";
      assert (utf32_of_utf16 utf16 = orig);
      let utf16s = utf16s_of_utf8   utf8   in xstring_of_utf16s utf16s |> show "utf16s_of_utf8   ";
      assert (utf32_of_utf16s utf16s = orig);

      let utf32  =  utf32_of_utf8   utf8   in xstring_of_utf32 utf32 |> show " utf32_of_utf8   ";
      (*assert (utf32_of_utf32 utf32 = orig);*)
      let utf32s = utf32s_of_utf8   utf8   in xstring_of_utf32s utf32s |> show "utf32s_of_utf8   ";
      assert (utf32_of_utf32s utf32s = orig);

      (* from utf8s *)
      let utf8   =   utf8_of_utf8s  utf8s  in xstring_of_utf8 utf8 |> show "  utf8_of_utf8s  ";
      assert (utf32_of_utf8 utf8 = orig);
      (*let utf8s  =  utf8s_of_utf8s  utf8s  in xstring_of_utf8s utf8s |> show " utf8s_of_utf8s  ";*)
      (*assert (utf32_of_utf8s utf8s = orig);*)

      let utf16  =  utf16_of_utf8s  utf8s  in xstring_of_utf16 utf16 |> show " utf16_of_utf8s  ";
      assert (utf32_of_utf16 utf16 = orig);
      let utf16s = utf16s_of_utf8s  utf8s  in xstring_of_utf16s utf16s |> show "utf16s_of_utf8s  ";
      assert (utf32_of_utf16s utf16s = orig);

      let utf32  =  utf32_of_utf8s  utf8s  in xstring_of_utf32 utf32 |> show " utf32_of_utf8s  ";
      (*assert (utf32_of_utf32 utf32 = orig);*)
      let utf32s = utf32s_of_utf8s  utf8s  in xstring_of_utf32s utf32s |> show "utf32s_of_utf8s  ";
      assert (utf32_of_utf32s utf32s = orig);

      assert (utf32 = orig);
    )

  ) Udb_data.data;
;;
