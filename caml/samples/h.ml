(* JSON-like string *)
module ASDF1  = struct
  include Comb.Make(TestLexer.SimpleStream)
  module Lexer = TestLexer.SimpleStream

  let main =
    ((look "{") @~> (listBy ((look "true")<|>(look "false")) (look ",")) <~@ (look "}"))
    --> (fun (xs: string list) -> Printf.sprintf "[%s]" (String.concat ";" xs) )
      [@@parser]

  (* TODO: think why if I add type to `main' everything is broken *)

  (* let main = *)
  (*   ( (listBy ((look "true") <|> (look "false")) (look ",")) ) *)
  (*   --> (fun (xs: string list) -> Printf.sprintf "[%s]" (String.concat ";" xs) ) *)
  (*     [@@parser] *)

end [@@parsers]
