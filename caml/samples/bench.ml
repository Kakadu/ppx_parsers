open Printf
open TstJSON

let () =
  let s =
    let filename = "../../scala/test1/src/test/resources/json1" in
    let filename = "json1" in
    let ch = open_in filename in
    let ans = ExtLib.input_all ch in
    let ()  = close_in ch in
    ans
  in
  printf "input length: '%d'\n%!" (String.length s);
  let open ASDF1 in
  let stream = Lexer.create s in
  match arr stream with
  | Parsed (_,(),_) -> printf "Parsed successfully \n%!"
  | Failed _ -> printf "Parsing failed\n%!"; exit 1
