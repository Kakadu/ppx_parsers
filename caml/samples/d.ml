
module ASDF1  = struct
  include Comb.Make(Lexer.SimpleStream)
  module Lexer = Lexer.SimpleStream

  let main = (many (look "true")) --> (String.concat ",") [@@parser]


end [@@parsers]
