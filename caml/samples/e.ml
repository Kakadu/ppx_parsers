open Printf
module ASDF1  = struct
  include Comb.Make(TestLexer.SimpleStream)
  module Lexer = TestLexer.SimpleStream

  (* TODO: listBy accepts at least 1 element. That parsers nothing *)
  let rec main stream =
    (((look "{") @~> (listBy theval (look ";")) <~@ (look "}"))
     --> (fun xs -> sprintf "<%s>" @@ String.concat ";" xs)) stream
      [@@parser]
  and theval stream =
    (((look "[") @~> (listBy main   (look ";")) <~@ (look "]"))
     --> (fun xs -> sprintf "(%s)" @@ String.concat ";" xs)) stream
      [@@parser]


end [@@parsers]
