module ASDF1  = struct
  include Comb.Make(TestLexer.SimpleStream)
  module Lexer = TestLexer.SimpleStream

  let main = (wss @~> (look "false")) @~> (wss @~> (look "true")) [@@parser]
  (* let main = wss @~> (look "false") [@@parser] *)
  (* let main = wss [@@parser] *)

end [@@parsers]
