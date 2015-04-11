
module ASDF1 = struct
  include Comb.Make(TestLexer.SimpleStream)
  module Lexer = TestLexer.SimpleStream

  let main =
     ( (look "true" <|> look "false") @~> (look ",") )
      [@@parser]

end [@@parsers]
