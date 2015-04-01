
module ASDF1  = struct
  include Comb.Make(TestLexer.SimpleStream)
  module Lexer = TestLexer.SimpleStream (* TODO: move it to functor *)

  let main = (decimal) [@@parser]

end [@@parsers]
