open Printf
module ASDF1  = struct
  include Comb.Make(TestLexer.SimpleStream)
  module Lexer = TestLexer.SimpleStream

  let rec main stream =
    (((look "{") @~> (list0By theval (look ";")) <~@ (look "}"))
     --> (fun xs -> sprintf "<%s>" @@ String.concat ";" xs)) stream
      [@@parser]
  and theval stream =
    (((look "[") @~> (list0By main   (look ";")) <~@ (look "]"))
     --> (fun xs -> sprintf "(%s)" @@ String.concat ";" xs)) stream
      [@@parser]


end [@@parsers]
