
module ASDF1  = struct
  include Comb.Make(Lexer.SimpleStream)
  module Lexer = Lexer.SimpleStream (* TODO: move it to functor *)


  let f = (look "true") <|> (look "false") [@@parser]
  (* let f = (look "true") [@@parser] *)

end [@@parsers]
