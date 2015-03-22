open Printf
open D

let () =
  let s = "true true" in
  printf "input: %s\n%!" s;
  let stream = ASDF1.Lexer.create s in
  let open ASDF1 in
  match main stream with
  | Parsed (ans,(),_) -> printf "Parsed successfully: '%s'\n%!" ans
  | Failed _ -> printf "Parsing failed\n%!"; exit 1
