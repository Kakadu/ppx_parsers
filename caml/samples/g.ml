module ASDF1  = struct
  include Comb.Make(TestLexer.SimpleStream)
  module Lexer = TestLexer.SimpleStream


  let main = ((look "false") @~> (look "true")) [@@parser]

end [@@parsers]
