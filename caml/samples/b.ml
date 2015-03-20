
module ASDF1  = struct
  include Comb.Make(Lexer.SimpleStream)
  module Lexer = Lexer.SimpleStream

  let h =  (look "true") --> ( (^)"1") [@@parser]


end [@@parsers]
