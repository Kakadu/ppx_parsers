(* Example for parsing alternatives *)
module ASDF1  = struct
  include Comb.Make(TestLexer.SimpleStream)
  module Lexer = TestLexer.SimpleStream (* TODO: move it to functor *)

  let main = (look "true") <|> (look "false") [@@parser]

end [@@parsers]
