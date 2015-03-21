
module ASDF1  = struct
  include Comb.Make(Lexer.SimpleStream)
  module Lexer = Lexer.SimpleStream (* TODO: move it to functor *)

  let main = (look "true") <|> (look "false") [@@parser]

end [@@parsers]
