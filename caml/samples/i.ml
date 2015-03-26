module ASDF1  = struct
  include Comb.Make(TestLexer.SimpleStream)
  module Lexer = TestLexer.SimpleStream

  let main = many1 (wss @~> look "true") --> (String.concat ",")
      [@@parser]

end [@@parsers]
