open Printf
open A

let () =
  let s = "true" in
  printf "input: %s\n%!" s;
  let open ASDF1 in
  let stream = Lexer.create s in
  match f stream with
  | Parsed (ans,(),_) -> printf "Parsed: %s\n%!" ans
  | Failed () -> printf "Failed\n%!"; exit 1
