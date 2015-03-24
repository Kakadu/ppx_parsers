module ASDF1  = struct
  include Comb.Make(TestLexer.SimpleStream)
  module Lexer = TestLexer.SimpleStream

  let rec main stream = ((look "{") @~> theval <~@ (look "}")) stream
      [@@parser]
  and theval = look "x"
      [@@parser]

  (* let rec main stream = *)
  (*   (((look "{") @~> (listBy theval (look ";")) <~@ (look "}")) *)
  (*    --> (fun xs -> sprintf "<%s>" @@ String.concat ";" xs)) stream *)
  (*     [@@parser] *)
  (* and theval stream = *)
  (*   (((look "[") @~> (listBy main   (look ";")) <~@ (look "]")) *)
  (*    --> (fun xs -> sprintf "(%s)" @@ String.concat ";" xs)) stream *)
  (*     [@@parser] *)


end [@@parsers]
