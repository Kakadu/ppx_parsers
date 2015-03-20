open Printf
open B

let () =
  let s = "true" in
  printf "input: %s\n%!" s;
  let open ASDF1 in
  let stream = Lexer.create s in
  match h stream with
  | Parsed (ans,(),_) -> printf "Parsed successfully: %s\n%!" ans
  | Failed _ -> printf "Parsing failed\n%!"; exit 1
