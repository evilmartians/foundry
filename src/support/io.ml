open Unicode.Std

let open_in filename =
  if filename = ("-" :> latin1s) then stdin
  else open_in filename

let open_out filename =
  if filename = ("-" :> latin1s) then stdout
  else open_out filename

let input_all channel =
  let rec input_some chunk pos len =
    let new_pos = pos + (input channel chunk pos (len - pos)) in
    if new_pos = pos then
      ByteArray.sub chunk 0 pos
    else if new_pos < len then
      input_some chunk new_pos len
    else
      let new_len   = len * 2 in
      let new_chunk = ByteArray.create new_len in
      ByteArray.blit chunk 0 new_chunk 0 new_pos;
      input_some new_chunk new_pos new_len
  in
  let bytes = input_some (ByteArray.create 4096) 0 4096 in
  Unicode.assert_utf8s bytes
