(* testing string literal parser there *)

module ASDF1  = struct
  include Comb.MakeExt(TestLexer.SimpleStream)
  module Lexer = TestLexer.SimpleStream

  let main = (many (wss @~> string_lit)) --> (String.concat ",") [@@parser]


end [@@parsers]
