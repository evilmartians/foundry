let _ =
  let prog = Sys.argv.(1)
  and args = Array.sub Sys.argv 1 ((Array.length Sys.argv) - 1) in
  if Unix.fork () = 0 then
    Unix.execv prog args
  else
    let pid, status = Unix.wait () in
    if status = Unix.WEXITED 0 then
      exit 1
    else
      exit 0
